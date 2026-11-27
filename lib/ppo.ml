
(* ---------- hyperparameters ---------- *)

(* Width of [World.default] in pixels. Positions are measured from the
   middle of the arena and divided by this, so the networks get inputs in
   -0.5..0.5 and mirroring the map in x or y is just negating that
   coordinate (see [flips]). *)
let arena = 400.

(* Discount halflife in game seconds: a reward this far ahead counts half
   as much as one now, 2 halflives a quarter, and so on, whatever the step
   size. A hider's return is 1 - 0.5^(seconds survived / halflife) (see
   [Trainer.survival_reward]), so this is also the survival time that
   scores 0.5. Lower it to make the agents care more about the near
   future; keep the round length (--time) well above it. *)
let halflife = 20.

(* The per-step discount for steps [dt] seconds apart: 0.5^(dt / halflife).
   At [Trainer.dt] = 0.05 that is about 0.9983. *)
let gamma ~dt = 0.5 ** (dt /. halflife)

(* GAE lambda: how far advantages look ahead before trusting the value
   network. 0 = one-step TD (low variance, biased by a bad critic); 1 = full
   Monte Carlo return (unbiased, noisy); 0.95 is the usual middle.
   Set as a halflife in game seconds, like [halflife]: after this long, the
   advantage leans half on the rewards seen and half on the value network.
   5 s gives lambda of about 0.993 at [Trainer.dt] = 0.05. At 0.7 s
   (lambda 0.95) the hider only saw about a second ahead, learned to run
   straight away from the hunter and got cornered (survival fell from ~10 s
   to ~6 s); at 5 s it learned to survive ~25 s on average over 200 rounds
   against the greedy hunter. Raising it to [halflife] would almost turn
   off what GAE does. *)
let lambda_halflife = 5.

(* The per-step GAE lambda for steps [dt] seconds apart. *)
let lambda ~dt = 0.5 ** (dt /. lambda_halflife)

(* PPO clip range. In one update the new/old action probability ratio only
   counts inside [1 - clip, 1 + clip], so the policy cannot move too far
   from the one that collected the data. Smaller = more cautious updates. *)
let clip = 0.2

(* Adam learning rate for both the policy and value networks. Too high and
   training swings or diverges; too low and it barely moves. At 3e-4 the
   policy hardly changed in 200 rounds (its mean stayed about 0.1 against
   noise of std 1, so the hider wandered at random). *)
let lr = 3e-3

(* Passes over the same batch of samples per update. More reuses data
   harder but drifts further from the data-collecting policy (clip only
   partly guards against that). *)
let epochs = 4

(* Samples per gradient step. One round of 20 s is about 400 samples per
   side, so --batch=10 rounds gives roughly 16 steps per epoch. Smaller =
   more, noisier steps. *)
let minibatch = 256

(* Weight of the symmetry penalty (issue #36) against the PPO losses. The
   map looks the same mirrored left-right or top-bottom, so the best move in
   a mirrored position is the mirrored move and the value is the same;
   [update] adds this times the mean squared difference from that to both
   losses, so what is learned on one side of the map carries over to the
   other three. 0 turns it off. *)
let symmetry_coef = 1.

let log_2pi = log (2. *. Float.pi)

type agent = { policy : Mlp.t; value : Mlp.t; log_std : float array }

(* Layer widths: 4 inputs (see [observe]), two hidden layers of 5, then 2
   outputs (the mean move x, y) for the policy or 1 (the expected return)
   for the value network. Wider/deeper can learn more but trains slower.
   [log_std] is the log of the exploration noise on each action; 0 means
   standard deviation 1, and it is learned along with the policy. *)
let init rng =
  {
    policy = Mlp.init rng [ 4; 5; 5; 2 ];
    value = Mlp.init rng [ 4; 16; 16; 16; 1 ];
    log_std = [| 0.; 0. |];
  }

(* ---------- symmetry ---------- *)

(* The three mirrorings of the map other than doing nothing, as the sign
   they multiply x and y by: left-right, top-bottom, and both. Observations
   are measured from the middle of the arena, so that is all they do. *)
let flips = [ (-1., 1.); (1., -1.); (-1., -1.) ]
let flip_obs (sx, sy) o = [| sx *. o.(0); sy *. o.(1); sx *. o.(2); sy *. o.(3) |]
let flip_act (sx, sy) a = [| sx *. a.(0); sy *. a.(1) |]

(* ---------- acting ---------- *)

let center e = Rect.center (Entity.hitbox e)

let observe w (e : Entity.t) =
  let me = center e in
  let opponents =
    List.filter
      (fun (o : Entity.t) -> o.alive && o.role <> e.role)
      (World.entities w)
  in
  (* strict < keeps the earlier entity on a tie *)
  let other =
    List.fold_left
      (fun best o ->
        let c = center o in
        match best with
        | Some b when Vec2d.dist me b <= Vec2d.dist me c -> best
        | _ -> Some c)
      None opponents
    |> Option.value ~default:me
  in
  let mid = arena /. 2. in
  Array.map (fun v -> (v -. mid) /. arena) [| me.x; me.y; other.x; other.y |]

let log_prob ~mean ~log_std a =
  let total = ref 0. in
  Array.iteri
    (fun i x ->
      let z = (x -. mean.(i)) /. exp log_std.(i) in
      total := !total -. (0.5 *. z *. z) -. log_std.(i) -. (0.5 *. log_2pi))
    a;
  !total

(* a standard normal sample (Box-Muller); 1 - u keeps log away from 0 *)
let normal rng =
  let u = 1. -. Random.State.float rng 1. and v = Random.State.float rng 1. in
  sqrt (-2. *. log u) *. cos (2. *. Float.pi *. v)

let mean ag o = Mlp.forward ag.policy o
let action_log_prob ag o a = log_prob ~mean:(mean ag o) ~log_std:ag.log_std a

let act rng ag o =
  let mean = Mlp.forward ag.policy o in
  let a = Array.mapi (fun i m -> m +. (exp ag.log_std.(i) *. normal rng)) mean in
  (a, log_prob ~mean ~log_std:ag.log_std a)

let value ag o = (Mlp.forward ag.value o).(0)

(* ---------- advantages ---------- *)

let gae ?(bootstrap = 0.) ~gamma ~lambda ~rewards ~values () =
  let n = Array.length rewards in
  if Array.length values <> n then invalid_arg "Ppo.gae: length mismatch";
  let adv = Array.make n 0. in
  let next_adv = ref 0. and next_v = ref bootstrap in
  for t = n - 1 downto 0 do
    let delta = rewards.(t) +. (gamma *. !next_v) -. values.(t) in
    adv.(t) <- delta +. (gamma *. lambda *. !next_adv);
    next_adv := adv.(t);
    next_v := values.(t)
  done;
  (adv, Array.mapi (fun t a -> a +. values.(t)) adv)

(* ---------- training ---------- *)

type sample = {
  obs : float array;
  act : float array;
  logp : float;
  adv : float;
  ret : float;
}

(* Adam's standard settings. beta1: decay of the running mean of gradients
   (momentum). beta2: decay of the running mean of squared gradients (the
   per-parameter step scale). eps: keeps the division away from 0. Rarely
   worth changing. The Adam step itself runs in python/ppo_torch.py. *)
let beta1 = 0.9
let beta2 = 0.999
let eps = 1e-8

(* The learner's Adam state: running means [m] and [v] of each parameter
   array's gradient (same shapes as the parameters) for the policy (whose
   last array is log_std) and the value network, and how many steps both
   have taken. *)
type learner = {
  ag : agent;
  policy_m : float array list;
  policy_v : float array list;
  value_m : float array list;
  value_v : float array list;
  steps : int;
}

let policy_params ag = Mlp.params ag.policy @ [ Array.copy ag.log_std ]
let zeros = List.map (fun p -> Array.make (Array.length p) 0.)

let learner ag =
  let pz = zeros (policy_params ag) and vz = zeros (Mlp.params ag.value) in
  { ag; policy_m = pz; policy_v = pz; value_m = vz; value_v = vz; steps = 0 }

let agent l = l.ag

let normalize_adv samples =
  let n = float (Array.length samples) in
  let mu = Array.fold_left (fun acc s -> acc +. s.adv) 0. samples /. n in
  let var =
    Array.fold_left (fun acc s -> acc +. ((s.adv -. mu) ** 2.)) 0. samples /. n
  in
  let sd = sqrt var +. 1e-8 in
  Array.map (fun s -> { s with adv = (s.adv -. mu) /. sd }) samples

(* a random order of 0 .. n - 1 (Fisher-Yates) *)
let permutation rng n =
  let a = Array.init n Fun.id in
  for i = n - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  a

let widths n = String.concat "," (List.map string_of_int (Mlp.sizes n))

(* the [count] arrays at the front of [floats], with [like]'s lengths, and
   the rest *)
let split like floats =
  let pos = ref 0 in
  let arrs =
    List.map
      (fun p ->
        let a = Array.sub floats !pos (Array.length p) in
        pos := !pos + Array.length p;
        a)
      like
  in
  (arrs, Array.sub floats !pos (Array.length floats - !pos))

(* Sends [l] and the normalized [samples] to the backend for [epochs]
   passes in the orders [orders]; is the updated learner and the losses the
   backend computed before updating: (policy, value, policy asymmetry,
   value asymmetry). *)
let torch_update backend l samples orders =
  let pp = policy_params l.ag and vp = Mlp.params l.ag.value in
  let flips =
    String.concat ";"
      (List.map (fun (sx, sy) -> Printf.sprintf "%d,%d" (truncate sx) (truncate sy)) flips)
  in
  let line =
    Printf.sprintf
      "update policy=%s value=%s n=%d epochs=%d minibatch=%d steps=%d clip=%s \
       lr=%s beta1=%s beta2=%s eps=%s symmetry=%s flips=%s"
      (widths l.ag.policy) (widths l.ag.value) (Array.length samples)
      (List.length orders) minibatch l.steps (Torch_backend.hex clip)
      (Torch_backend.hex lr) (Torch_backend.hex beta1) (Torch_backend.hex beta2)
      (Torch_backend.hex eps) (Torch_backend.hex symmetry_coef) flips
  in
  let sample s = Array.concat [ s.obs; s.act; [| s.logp; s.adv; s.ret |] ] in
  let floats =
    Array.concat
      (List.concat
         [
           pp; l.policy_m; l.policy_v; vp; l.value_m; l.value_v;
           Array.to_list (Array.map sample samples);
           List.map (Array.map float) orders;
         ])
  in
  let words, reply = Torch_backend.request backend line floats in
  let pp', rest = split pp reply in
  let pm, rest = split pp rest in
  let pv, rest = split pp rest in
  let vp', rest = split vp rest in
  let vm, rest = split vp rest in
  let vv, _ = split vp rest in
  let steps, before =
    match words with
    | steps :: losses when List.length losses = 4 ->
        (int_of_string steps, Array.of_list (List.map float_of_string losses))
    | _ -> failwith "Ppo.update: bad reply from the backend"
  in
  let k = List.length pp' - 1 in
  let ag =
    {
      policy = Mlp.with_params l.ag.policy (List.filteri (fun i _ -> i < k) pp');
      value = Mlp.with_params l.ag.value vp';
      log_std = List.nth pp' k;
    }
  in
  ( { ag; policy_m = pm; policy_v = pv; value_m = vm; value_v = vv; steps },
    before )

let update backend rng l samples =
  if Array.length samples = 0 then l
  else
    let samples = normalize_adv samples in
    let n = Array.length samples in
    let orders = List.init epochs (fun _ -> permutation rng n) in
    fst (torch_update backend l samples orders)

let torch_losses backend l samples =
  if Array.length samples = 0 then ((nan, nan), (nan, nan))
  else
    let _, b = torch_update backend l (normalize_adv samples) [] in
    ((b.(0), b.(1)), (b.(2), b.(3)))

let mean_of f xs = Array.fold_left (fun acc x -> acc +. f x) 0. xs /. float (Array.length xs)

let losses l samples =
  if Array.length samples = 0 then (nan, nan)
  else
    let s = normalize_adv samples and ag = l.ag in
    let surrogate s =
      let ratio = exp (action_log_prob ag s.obs s.act -. s.logp) in
      let clipped = Float.max (Float.min ratio (1. +. clip)) (1. -. clip) in
      Float.min (ratio *. s.adv) (clipped *. s.adv)
    in
    ( -.mean_of surrogate s,
      mean_of (fun s -> (value ag s.obs -. s.ret) ** 2.) s )

(* the mean over [flips] of [f g s] for every sample [s] *)
let over_flips f samples =
  List.fold_left (fun acc g -> acc +. mean_of (f g) samples) 0. flips
  /. float (List.length flips)

let symmetry_loss l samples =
  if Array.length samples = 0 then (nan, nan)
  else
    let ag = l.ag in
    let policy g s =
      let a = mean ag (flip_obs g s.obs) and b = flip_act g (mean ag s.obs) in
      (((a.(0) -. b.(0)) ** 2.) +. ((a.(1) -. b.(1)) ** 2.)) /. 2.
    in
    let value g s = (value ag (flip_obs g s.obs) -. value ag s.obs) ** 2. in
    (over_flips policy samples, over_flips value samples)

(* ---------- saving ---------- *)

let write oc ag =
  Mlp.write oc ag.policy;
  Mlp.write oc ag.value;
  Printf.fprintf oc "log_std %h %h\n" ag.log_std.(0) ag.log_std.(1)

let last l = List.nth l (List.length l - 1)

let read ic =
  let policy = Mlp.read ic in
  let value = Mlp.read ic in
  let log_std = Scanf.bscanf ic " log_std %h %h" (fun a b -> [| a; b |]) in
  let ps = Mlp.sizes policy and vs = Mlp.sizes value in
  if List.hd ps <> 4 || last ps <> 2 || List.hd vs <> 4 || last vs <> 1 then
    failwith "Ppo.read: networks have the wrong input or output widths";
  { policy; value; log_std }
