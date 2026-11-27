# /// script
# requires-python = ">=3.10"
# dependencies = ["torch", "numpy"]
#
# [tool.uv.sources]
# torch = { index = "pytorch-cpu" }
#
# [[tool.uv.index]]
# name = "pytorch-cpu"
# url = "https://download.pytorch.org/whl/cpu"
# explicit = true
# ///
"""PPO updates (backprop and Adam) for lib/ppo.ml, in PyTorch.

OCaml plays the rounds and runs the networks forward itself; this process
only does the training step, because owl-base's autodiff was too slow
(issue #41). It keeps no state: every request carries the networks, their
Adam state and the samples, and the reply carries them back updated, so an
OCaml learner stays a plain value.

Protocol, over stdin/stdout. On start it prints the line "ready". Then for
each request:

  update <key>=<value> ...\n   (keys below; floats as hex, e.g. 0x1.8p+1)
  <raw little-endian float64s>

  keys: floats         how many float64s follow the line
        policy, value  layer widths, e.g. 4,5,5,2
        n              number of samples
        epochs, minibatch, steps (Adam steps taken so far)
        clip, lr, beta1, beta2, eps, symmetry
        flips          the mirrorings as signs, e.g. -1,1;1,-1;-1,-1

  floats, in order:
    policy params (w1 b1 w2 b2 ... row-major, then log_std), its Adam m, v
    value params, its Adam m, v
    n samples of: obs (4), act (2), logp, adv (already normalized), ret
    epochs * n sample indices, the order of each epoch

It replies with

  ok <count> <steps> <policy loss> <value loss> <policy asym> <value asym>\n
  <count raw float64s>: policy params, m, v, then value params, m, v

where the losses are those of the networks before this update on all n
samples, without the symmetry penalty (epochs=0 just computes them), or with
the single line "error <message>" if the request was bad.
"""

import math
import sys

import numpy as np
import torch

torch.set_num_threads(1)  # the matrices are tiny; threads only add overhead
DTYPE = torch.float64


def sizes_of(text):
    return [int(s) for s in text.split(",")]


def numel(sizes):
    """Parameter shapes of an MLP with these widths, as (w, b) pairs."""
    return [((a, b), (b,)) for a, b in zip(sizes, sizes[1:])]


def forward(ps, x):
    """tanh on every hidden layer, linear output; ps = [w1, b1, w2, b2, ...]."""
    for i in range(0, len(ps), 2):
        x = x @ ps[i] + ps[i + 1]
        if i + 2 < len(ps):
            x = torch.tanh(x)
    return x


def asymmetry(ps, x, flips, out_sign):
    """Mean squared difference between the network at mirrored observations
    and the mirrored output, averaged over flips (see Ppo.symmetry_loss)."""
    y = forward(ps, x)
    total = 0.0
    for sx, sy in flips:
        gx = x * torch.tensor([sx, sy, sx, sy], dtype=DTYPE)
        gy = torch.tensor(out_sign(sx, sy), dtype=DTYPE)
        total = total + torch.mean((forward(ps, gx) - y * gy) ** 2)
    return total / len(flips)


def policy_loss(pp, x, a, old_logp, adv, clip):
    mlp, log_std = pp[:-1], pp[-1]
    mu = forward(mlp, x)
    z = (a - mu) / torch.exp(log_std)
    d = a.shape[1]
    logp = torch.sum(-0.5 * z * z - log_std, dim=1) - 0.5 * d * math.log(2 * math.pi)
    ratio = torch.exp(logp - old_logp)
    clipped = torch.clamp(ratio, 1 - clip, 1 + clip)
    return -torch.mean(torch.minimum(ratio * adv, clipped * adv))


def value_loss(vp, x, ret):
    return torch.mean((forward(vp, x)[:, 0] - ret) ** 2)


class Reader:
    """Hands out consecutive chunks of a flat float array."""

    def __init__(self, data):
        self.data, self.pos = data, 0

    def take(self, count):
        chunk = self.data[self.pos : self.pos + count]
        if len(chunk) != count:
            raise ValueError("request has too few floats")
        self.pos += count
        return chunk

    def tensors(self, shapes):
        out = []
        for shape in shapes:
            count = math.prod(shape)
            out.append(torch.tensor(self.take(count), dtype=DTYPE).reshape(shape))
        return out


def update(fields, data):
    psizes, vsizes = sizes_of(fields["policy"]), sizes_of(fields["value"])
    n, epochs = int(fields["n"]), int(fields["epochs"])
    minibatch, steps = int(fields["minibatch"]), int(fields["steps"])
    clip, lr = float.fromhex(fields["clip"]), float.fromhex(fields["lr"])
    beta1, beta2 = float.fromhex(fields["beta1"]), float.fromhex(fields["beta2"])
    eps, coef = float.fromhex(fields["eps"]), float.fromhex(fields["symmetry"])
    flips = [tuple(int(s) for s in f.split(",")) for f in fields["flips"].split(";")]

    pshapes = [s for layer in numel(psizes) for s in layer] + [(psizes[-1],)]
    vshapes = [s for layer in numel(vsizes) for s in layer]
    r = Reader(data)
    groups = [r.tensors(pshapes) for _ in range(3)] + [r.tensors(vshapes) for _ in range(3)]
    pp, pm, pv, vp, vm, vv = groups
    samples = torch.tensor(r.take(9 * n), dtype=DTYPE).reshape(n, 9)
    order = torch.tensor(r.take(epochs * n), dtype=torch.int64).reshape(epochs, n)
    if r.pos != len(data):
        raise ValueError("request has too many floats")
    for p in pp + vp:
        p.requires_grad_(True)

    policy_flip = lambda sx, sy: [sx, sy]
    value_flip = lambda sx, sy: [1]

    def objective(batch):
        x, a = batch[:, 0:4], batch[:, 4:6]
        logp, adv, ret = batch[:, 6], batch[:, 7], batch[:, 8]
        p = policy_loss(pp, x, a, logp, adv, clip) + coef * asymmetry(pp[:-1], x, flips, policy_flip)
        v = value_loss(vp, x, ret) + coef * asymmetry(vp, x, flips, value_flip)
        # the two networks share no parameters, so one backward gives each
        # its own gradient
        return p + v

    with torch.no_grad():
        x = samples[:, 0:4]
        before = [
            policy_loss(pp, x, samples[:, 4:6], samples[:, 6], samples[:, 7], clip),
            value_loss(vp, x, samples[:, 8]),
            asymmetry(pp[:-1], x, flips, policy_flip),
            asymmetry(vp, x, flips, value_flip),
        ]

    params, ms, vs = pp + vp, pm + vm, pv + vv
    for e in range(epochs):
        shuffled = samples[order[e]]
        for start in range(0, n, minibatch):
            for p in params:
                p.grad = None
            objective(shuffled[start : start + minibatch]).backward()
            steps += 1
            c1, c2 = 1 - beta1**steps, 1 - beta2**steps
            with torch.no_grad():
                for p, m, v in zip(params, ms, vs):
                    g = p.grad
                    m.mul_(beta1).add_(g * (1 - beta1))
                    v.mul_(beta2).add_(g * g * (1 - beta2))
                    p.sub_((m / c1) / (torch.sqrt(v / c2) + eps) * lr)

    out = torch.cat([t.detach().reshape(-1) for t in pp + pm + pv + vp + vm + vv])
    header = "ok %d %d %s\n" % (out.numel(), steps, " ".join(float(b).hex() for b in before))
    return header, out.numpy().astype("<f8").tobytes()


def main():
    stdin, stdout = sys.stdin.buffer, sys.stdout.buffer
    stdout.write(b"ready\n")
    stdout.flush()
    while True:
        line = stdin.readline()
        if not line:
            return
        words = line.decode().split()
        fields = dict(w.split("=", 1) for w in words[1:])
        try:
            count = int(fields["floats"])
            raw = stdin.read(8 * count)
            data = np.frombuffer(raw, dtype="<f8").tolist()
            if words[0] != "update":
                raise ValueError("unknown request " + words[0])
            header, body = update(fields, data)
            stdout.write(header.encode() + body)
        except Exception as e:  # report it to OCaml instead of dying
            stdout.write(("error %s: %s\n" % (type(e).__name__, e)).replace("\n", " ").encode() + b"\n")
        stdout.flush()


if __name__ == "__main__":
    main()
