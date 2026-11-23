module N = Mlp.N
module AD = Mlp.AD

(* ---------- hyperparameters ---------- *)

(* Width of [World.default] in pixels. Positions are divided by this so the
   networks get inputs around 0..1. *)
let arena = 400.

(* Discount per step. A reward k steps ahead counts gamma^k as much as one
   now. Steps are [Trainer.dt] = 1/20 s apart, so the effective horizon is
   about 1 / (1 - gamma) = 100 steps = 5 game seconds. Raise toward 1 (e.g.
   0.995 is about 10 s, 0.999 about 50 s) to make later events like the
   timeout win matter more. *)
let gamma = 0.99

(* GAE lambda: how far advantages look ahead before trusting the value
   network. 0 = one-step TD (low variance, biased by a bad critic); 1 = full
   Monte Carlo return (unbiased, noisy). 0.95 is the usual middle. *)
let lambda = 0.95

(* PPO clip range. In one update the new/old action probability ratio only
   counts inside [1 - clip, 1 + clip], so the policy cannot move too far
   from the one that collected the data. Smaller = more cautious updates. *)
let clip = 0.2

(* Adam learning rate for both the policy and value networks. Too high and
   training swings or diverges; too low and it barely moves. *)
let lr = 3e-4

(* Passes over the same batch of samples per update. More reuses data
   harder but drifts further from the data-collecting policy (clip only
   partly guards against that). *)
let epochs = 4

(* Samples per gradient step. One round of 20 s is about 400 samples per
   side, so --batch=10 rounds gives roughly 16 steps per epoch. Smaller =
   more, noisier steps. *)
let minibatch = 256

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
    value = Mlp.init rng [ 4; 5; 5; 1 ];
    log_std = [| 0.; 0. |];
  }

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
  Array.map (fun v -> v /. arena) [| me.x; me.y; other.x; other.y |]

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

let action_log_prob ag o a =
  log_prob ~mean:(Mlp.forward ag.policy o) ~log_std:ag.log_std a

let act rng ag o =
  let mean = Mlp.forward ag.policy o in
  let a = Array.mapi (fun i m -> m +. (exp ag.log_std.(i) *. normal rng)) mean in
  (a, log_prob ~mean ~log_std:ag.log_std a)

let value ag o = (Mlp.forward ag.value o).(0)

(* ---------- advantages ---------- *)

let gae ~gamma ~lambda ~rewards ~values =
  let n = Array.length rewards in
  if Array.length values <> n then invalid_arg "Ppo.gae: length mismatch";
  let adv = Array.make n 0. in
  let next_adv = ref 0. and next_v = ref 0. in
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

(* Adam state for a list of parameter arrays *)
type adam = { m : Mlp.arr list; v : Mlp.arr list; steps : int }

let adam_init params =
  let z = List.map (fun p -> N.zeros (N.shape p)) params in
  { m = z; v = z; steps = 0 }

(* Adam's standard settings. beta1: decay of the running mean of gradients
   (momentum). beta2: decay of the running mean of squared gradients (the
   per-parameter step scale). eps: keeps the division away from 0. Rarely
   worth changing. *)
let beta1 = 0.9
let beta2 = 0.999
let eps = 1e-8

(* one Adam step: the new parameters and the new state *)
let adam_step st params grads =
  let steps = st.steps + 1 in
  let m =
    List.map2
      (fun m g -> N.add (N.mul_scalar m beta1) (N.mul_scalar g (1. -. beta1)))
      st.m grads
  in
  let v =
    List.map2
      (fun v g ->
        N.add (N.mul_scalar v beta2) (N.mul_scalar (N.sqr g) (1. -. beta2)))
      st.v grads
  in
  let c1 = 1. -. (beta1 ** float steps) and c2 = 1. -. (beta2 ** float steps) in
  let params =
    List.map2
      (fun p (m, v) ->
        let m_hat = N.mul_scalar m (1. /. c1) in
        let v_hat = N.mul_scalar v (1. /. c2) in
        N.sub p
          (N.mul_scalar (N.div m_hat (N.add_scalar (N.sqrt v_hat) eps)) lr))
      params (List.combine m v)
  in
  (params, { m; v; steps })

(* the gradient of [loss] with respect to each array in [params] *)
let grads loss params =
  let tag = AD.tag () in
  let ps = List.map (fun p -> AD.make_reverse (AD.pack_arr p) tag) params in
  AD.reverse_prop (AD._f 1.) (loss ps);
  List.map (fun p -> AD.unpack_arr (AD.adjval p)) ps

type learner = { ag : agent; policy_opt : adam; value_opt : adam }

let row v = N.of_array v [| 1; Array.length v |]
let policy_params ag = Mlp.params ag.policy @ [ row ag.log_std ]

let learner ag =
  {
    ag;
    policy_opt = adam_init (policy_params ag);
    value_opt = adam_init (Mlp.params ag.value);
  }

let agent l = l.ag

(* a [rows x cols] constant from one row per element of [xs] *)
let matrix f cols xs =
  let rows = Array.length xs in
  AD.pack_arr (N.of_array (Array.concat (Array.to_list (Array.map f xs))) [| rows; cols |])

(* the negated clipped surrogate objective; the last param is log_std *)
let policy_loss batch ps =
  let n = Array.length batch in
  let mlp_ps = List.filteri (fun i _ -> i < List.length ps - 1) ps in
  let log_std = List.nth ps (List.length ps - 1) in
  let x = matrix (fun s -> s.obs) 4 batch in
  let a = matrix (fun s -> s.act) 2 batch in
  let old_logp = matrix (fun s -> [| s.logp |]) 1 batch in
  let adv = matrix (fun s -> [| s.adv |]) 1 batch in
  let const c = AD.pack_arr (N.create [| n; 1 |] c) in
  let open AD.Maths in
  let mu = Mlp.forward_ad mlp_ps x in
  let z = (a - mu) / exp log_std in
  let logp =
    sum_reduce ~axis:[| 1 |] ((AD._f (-0.5) * sqr z) - log_std)
    - AD._f log_2pi
  in
  let ratio = exp (logp - old_logp) in
  let clipped = max2 (min2 ratio (const (1. +. clip))) (const (1. -. clip)) in
  neg (mean (min2 (ratio * adv) (clipped * adv)))

let value_loss batch ps =
  let x = matrix (fun s -> s.obs) 4 batch in
  let ret = matrix (fun s -> [| s.ret |]) 1 batch in
  AD.Maths.(mean (sqr (Mlp.forward_ad ps x - ret)))

let minibatch_step l batch =
  let pp = policy_params l.ag in
  let pp, policy_opt = adam_step l.policy_opt pp (grads (policy_loss batch) pp) in
  let vp = Mlp.params l.ag.value in
  let vp, value_opt = adam_step l.value_opt vp (grads (value_loss batch) vp) in
  let k = List.length pp - 1 in
  let ag =
    {
      policy = Mlp.with_params l.ag.policy (List.filteri (fun i _ -> i < k) pp);
      value = Mlp.with_params l.ag.value vp;
      log_std = Mlp.to_array (List.nth pp k);
    }
  in
  { ag; policy_opt; value_opt }

let normalize_adv samples =
  let n = float (Array.length samples) in
  let mu = Array.fold_left (fun acc s -> acc +. s.adv) 0. samples /. n in
  let var =
    Array.fold_left (fun acc s -> acc +. ((s.adv -. mu) ** 2.)) 0. samples /. n
  in
  let sd = sqrt var +. 1e-8 in
  Array.map (fun s -> { s with adv = (s.adv -. mu) /. sd }) samples

let shuffle rng a =
  let a = Array.copy a in
  for i = Array.length a - 1 downto 1 do
    let j = Random.State.int rng (i + 1) in
    let t = a.(i) in
    a.(i) <- a.(j);
    a.(j) <- t
  done;
  a

let update rng l samples =
  if Array.length samples = 0 then l
  else
    let samples = normalize_adv samples in
    let n = Array.length samples in
    let rec epoch e l =
      if e = 0 then l
      else
        let s = shuffle rng samples in
        let rec go start l =
          if start >= n then l
          else
            let len = min minibatch (n - start) in
            go (start + len) (minibatch_step l (Array.sub s start len))
        in
        epoch (e - 1) (go 0 l)
    in
    epoch epochs l

let losses l samples =
  if Array.length samples = 0 then (nan, nan)
  else
    let s = normalize_adv samples in
    let eval loss ps = AD.unpack_flt (loss s (List.map AD.pack_arr ps)) in
    (eval policy_loss (policy_params l.ag), eval value_loss (Mlp.params l.ag.value))

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
