module N = Mlp.N
module AD = Mlp.AD

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

(* Weight of the entropy bonus: [update] subtracts this times the policy's
   mean entropy from its loss, so it keeps trying other moves instead of
   locking onto one early. Entropy is at most log 9 (about 2.2) when every
   move is equally likely. Raise it if the printed entropy falls toward 0
   while survival is still poor; 0 turns it off. *)
let entropy_coef = 0.01

(* ---------- moves ---------- *)

(* The 9 moves, like the keyboard player's: move [i] steps [i mod 3 - 1] in
   x and [i / 3 - 1] in y, so 4 is staying still and 0 is up-left.
   Diagonals are not faster: [Physics.move] normalizes. *)
let n_moves = 9

let move i =
  if i < 0 || i >= n_moves then invalid_arg "Ppo.move: no such move";
  Vec2d.{ x = float (i mod 3 - 1); y = float (i / 3 - 1) }

type agent = { policy : Mlp.t; value : Mlp.t }

(* Layer widths: 4 inputs (see [observe]), then two hidden layers of 16 and
   one score (logit) per move for the policy; three hidden layers of 16 and
   1 output (the expected return) for the value network. Wider/deeper can
   learn more but trains slower. *)
let init rng =
  { policy = Mlp.init rng [ 4; 16; 16; n_moves ]; value = Mlp.init rng [ 4; 16; 16; 16; 1 ] }

(* ---------- symmetry ---------- *)

(* The three mirrorings of the map other than doing nothing, as the sign
   they multiply x and y by: left-right, top-bottom, and both. Observations
   are measured from the middle of the arena, so that is all they do. *)
let flips = [ (-1., 1.); (1., -1.); (-1., -1.) ]
let flip_obs (sx, sy) o = [| sx *. o.(0); sy *. o.(1); sx *. o.(2); sy *. o.(3) |]

let flip_act (sx, sy) i =
  let c = i mod 3 and r = i / 3 in
  let c = if sx < 0. then 2 - c else c and r = if sy < 0. then 2 - r else r in
  (3 * r) + c

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

(* log of the softmax of [logits]; subtracting the largest first keeps exp
   from overflowing *)
let log_softmax logits =
  let m = Array.fold_left Float.max neg_infinity logits in
  let lse = m +. log (Array.fold_left (fun acc l -> acc +. exp (l -. m)) 0. logits) in
  Array.map (fun l -> l -. lse) logits

let log_probs ag o = log_softmax (Mlp.forward ag.policy o)
let action_log_prob ag o i = (log_probs ag o).(i)

let act rng ag o =
  let lp = log_probs ag o in
  (* walk the cumulative probabilities until they pass a uniform draw; the
     last move catches rounding *)
  let u = Random.State.float rng 1. in
  let rec pick i acc =
    let acc = acc +. exp lp.(i) in
    if u < acc || i = n_moves - 1 then i else pick (i + 1) acc
  in
  let i = pick 0 0. in
  (i, lp.(i))

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
  act : int;
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

let policy_params ag = Mlp.params ag.policy

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

(* row-wise log softmax of a [batch x moves] matrix of logits. owl-base has
   no log_sum_exp, so it is built by hand; each row's largest logit is
   subtracted first (as a constant: the result does not depend on it) so
   exp cannot overflow *)
let log_softmax_ad y =
  let v = AD.unpack_arr (AD.primal' y) in
  let rows = (N.shape v).(0) and cols = (N.shape v).(1) in
  let a = Mlp.to_array v in
  let m =
    Array.init rows (fun r ->
        Array.fold_left Float.max neg_infinity (Array.sub a (r * cols) cols))
  in
  let m = AD.pack_arr (N.of_array m [| rows; 1 |]) in
  AD.Maths.(
    let z = y - m in
    z - log (sum_reduce ~axis:[| 1 |] (exp z)))

(* the [moves x moves] matrix that reorders a row of per-move numbers as
   [flip_act g] mirrors the moves: column [j] picks entry [flip_act g j],
   so a symmetric policy has [lp (flip_obs g o) = lp o *@ perm g] *)
let perm g =
  let p = N.zeros [| n_moves; n_moves |] in
  for j = 0 to n_moves - 1 do
    N.set p [| flip_act g j; j |] 1.
  done;
  AD.pack_arr p

(* the mean, over [flips] and every output of every sample, of the squared
   difference between [f] of the network [ps] at the mirrored observation
   and [mirror g] of [f] of it at the original one *)
let asymmetry ~f ~mirror batch ps =
  let x = matrix (fun s -> s.obs) 4 batch in
  let y = f (Mlp.forward_ad ps x) in
  let term g =
    let gx = matrix (fun s -> flip_obs g s.obs) 4 batch in
    AD.Maths.(mean (sqr (f (Mlp.forward_ad ps gx) - mirror g y)))
  in
  let open AD.Maths in
  List.fold_left (fun acc g -> acc + term g) (AD._f 0.) flips
  / AD._f (float (List.length flips))

(* the policy compares log probabilities (logits can all shift together
   without changing the moves), with the moves mirrored; the value compares
   as is *)
let policy_asymmetry =
  asymmetry ~f:log_softmax_ad ~mirror:(fun g y -> AD.Maths.(y *@ perm g))

let value_asymmetry = asymmetry ~f:Fun.id ~mirror:(fun _ y -> y)

(* the mean entropy of the policy [ps] at the observations of [batch] *)
let policy_entropy batch ps =
  let lp = log_softmax_ad (Mlp.forward_ad ps (matrix (fun s -> s.obs) 4 batch)) in
  AD.Maths.(neg (mean (sum_reduce ~axis:[| 1 |] (exp lp * lp))))

(* the negated clipped surrogate objective *)
let policy_loss batch ps =
  let n = Array.length batch in
  let x = matrix (fun s -> s.obs) 4 batch in
  let chosen =
    matrix (fun s -> Array.init n_moves (fun j -> if j = s.act then 1. else 0.)) n_moves batch
  in
  let old_logp = matrix (fun s -> [| s.logp |]) 1 batch in
  let adv = matrix (fun s -> [| s.adv |]) 1 batch in
  let const c = AD.pack_arr (N.create [| n; 1 |] c) in
  let open AD.Maths in
  let logp = sum_reduce ~axis:[| 1 |] (chosen * log_softmax_ad (Mlp.forward_ad ps x)) in
  let ratio = exp (logp - old_logp) in
  let clipped = max2 (min2 ratio (const (1. +. clip))) (const (1. -. clip)) in
  neg (mean (min2 (ratio * adv) (clipped * adv)))

let value_loss batch ps =
  let x = matrix (fun s -> s.obs) 4 batch in
  let ret = matrix (fun s -> [| s.ret |]) 1 batch in
  AD.Maths.(mean (sqr (Mlp.forward_ad ps x - ret)))

(* what [update] minimizes: the PPO loss, minus the entropy bonus for the
   policy, plus the symmetry penalty *)
let policy_objective batch ps =
  AD.Maths.(
    policy_loss batch ps
    - (AD._f entropy_coef * policy_entropy batch ps)
    + (AD._f symmetry_coef * policy_asymmetry batch ps))

let value_objective batch ps =
  AD.Maths.(value_loss batch ps + (AD._f symmetry_coef * value_asymmetry batch ps))

let minibatch_step l batch =
  let pp = policy_params l.ag in
  let pp, policy_opt = adam_step l.policy_opt pp (grads (policy_objective batch) pp) in
  let vp = Mlp.params l.ag.value in
  let vp, value_opt = adam_step l.value_opt vp (grads (value_objective batch) vp) in
  let ag = { policy = Mlp.with_params l.ag.policy pp; value = Mlp.with_params l.ag.value vp } in
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

let symmetry_loss l samples =
  if Array.length samples = 0 then (nan, nan)
  else
    let eval loss ps = AD.unpack_flt (loss samples (List.map AD.pack_arr ps)) in
    ( eval policy_asymmetry (Mlp.params l.ag.policy),
      eval value_asymmetry (Mlp.params l.ag.value) )

let entropy l samples =
  if Array.length samples = 0 then nan
  else AD.unpack_flt (policy_entropy samples (List.map AD.pack_arr (Mlp.params l.ag.policy)))

(* ---------- saving ---------- *)

let write oc ag =
  Mlp.write oc ag.policy;
  Mlp.write oc ag.value

let last l = List.nth l (List.length l - 1)

let read ic =
  let policy = Mlp.read ic in
  let value = Mlp.read ic in
  let ps = Mlp.sizes policy and vs = Mlp.sizes value in
  if List.hd ps <> 4 || last ps <> n_moves || List.hd vs <> 4 || last vs <> 1 then
    failwith
      "Ppo.read: networks have the wrong input or output widths (files saved \
       before the 9 discrete moves have 2 policy outputs; retrain them)";
  { policy; value }
