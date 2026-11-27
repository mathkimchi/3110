(** Proximal Policy Optimization for one side of the game: a policy network
    that picks a direction and a value network that guesses how good a
    position is. Both are [Mlp]s of widths 4-5-5-2 (policy) and 4-5-5-1
    (value), as in issue #29.

    Hyperparameters: discount halflife 20 s, GAE lambda halflife 5 s,
    clip 0.2, Adam with learning rate 3e-3, 4 epochs of minibatches of 256
    per update, symmetry penalty weight 1. *)

val arena : float
(** [400.], the size of [World.default]; positions are measured from the
    middle of the arena and divided by it so the networks see numbers in
    [-0.5, 0.5]. *)

val halflife : float
(** [20.], the discount halflife in game seconds. *)

val gamma : dt:float -> float
(** [gamma ~dt] is [0.5 ** (dt /. halflife)], the per-step discount to pass
    to [gae] for steps [dt] game seconds apart, so a reward [halflife]
    seconds ahead counts half whatever [dt] is. *)

val lambda_halflife : float
(** [5.], the GAE lambda halflife in game seconds. *)

val lambda : dt:float -> float
(** [lambda ~dt] is [0.5 ** (dt /. lambda_halflife)], the GAE lambda to pass
    to [gae] for steps [dt] game seconds apart. *)

val symmetry_coef : float
(** [1.], the weight of the symmetry penalty [update] adds to each loss
    (see [symmetry_loss]). *)

(** {2 Symmetry}

    The map looks the same mirrored left-right or top-bottom, and
    observations are measured from its middle, so mirroring a position
    negates some coordinates. *)

val flips : (float * float) list
(** [flips] is the three mirrorings other than doing nothing, as the signs
    [(sx, sy)] they multiply x and y by: [(-1., 1.)] (left-right),
    [(1., -1.)] (top-bottom) and [(-1., -1.)] (both). *)

val flip_obs : float * float -> float array -> float array
(** [flip_obs (sx, sy) o] is observation [o] (see [observe]) of the mirrored
    position: both x coordinates times [sx], both y coordinates times [sy].
    Requires: [o] has 4 entries. *)

val flip_act : float * float -> float array -> float array
(** [flip_act (sx, sy) a] is the move [a] mirrored: [[| sx *. a.(0); sy *. a.(1) |]].
    Requires: [a] has 2 entries. *)

type agent
(** A policy network, a value network and a log standard deviation per
    direction component.
    AF: in a state with observation [o], the agent moves in direction [a]
    drawn from a Gaussian with mean [Mlp.forward policy o] and standard
    deviation [exp log_std] (independent per component), and expects a
    return of [Mlp.forward value o].
    RI: policy widths are 4-...-2, value widths are 4-...-1, and [log_std]
    has 2 entries. *)

val init : Random.State.t -> agent
(** [init rng] is a fresh 4-5-5-2 / 4-5-5-1 agent with weights drawn from
    [rng] and standard deviation [1.]. *)

val observe : World.t -> Entity.t -> float array
(** [observe w e] is [[| x; y; ox; oy |]]: the center of [e] and the center
    of the closest living entity of the other role, each measured from the
    middle of the arena ([(arena /. 2., arena /. 2.)]) and divided by
    [arena]. If there is no such entity, [(ox, oy)] is [e]'s own center. *)

val log_prob : mean:float array -> log_std:float array -> float array -> float
(** [log_prob ~mean ~log_std a] is the log density of [a] under independent
    Gaussians with means [mean] and standard deviations [exp log_std].
    Requires: all three arrays have the same length. *)

val mean : agent -> float array -> float array
(** [mean ag o] is the mean of the direction [ag]'s policy picks at
    observation [o]: [Mlp.forward] of its policy network. *)

val action_log_prob : agent -> float array -> float array -> float
(** [action_log_prob ag o a] is the log probability density of [ag]'s policy
    choosing direction [a] at observation [o]. *)

val act : Random.State.t -> agent -> float array -> float array * float
(** [act rng ag o] samples a direction [a] for observation [o] from [ag]'s
    policy using [rng], and is [(a, log_prob a)]. *)

val value : agent -> float array -> float
(** [value ag o] is [ag]'s value network on observation [o]. *)

val gae :
  ?bootstrap:float ->
  gamma:float ->
  lambda:float ->
  rewards:float array ->
  values:float array ->
  unit ->
  float array * float array
(** [gae ~bootstrap ~gamma ~lambda ~rewards ~values ()] is
    [(advantages, returns)] for one episode whose value after its last step
    is [bootstrap]: [0.] (the default) if the episode really ended there, or
    the value network's guess for the state it stopped in if it was cut off
    by a time limit:
    [delta_t = r_t + gamma * v_(t+1) - v_t],
    [adv_t = delta_t + gamma * lambda * adv_(t+1)] and
    [ret_t = adv_t + v_t].
    Raises [Invalid_argument] if the arrays differ in length. *)

type sample = {
  obs : float array;
  act : float array;
  logp : float;  (** log probability of [act] when it was taken *)
  adv : float;
  ret : float;
}
(** One step of experience, ready for [update]. *)

type learner
(** An agent together with its Adam optimizer state. *)

val learner : agent -> learner
(** [learner ag] starts training [ag] with fresh optimizer state. *)

val agent : learner -> agent
(** [agent l] is the agent [l] is training. *)

val update : Random.State.t -> learner -> sample array -> learner
(** [update rng l samples] runs the PPO update on [samples]: advantages are
    normalized, then for each epoch the samples are shuffled with [rng] and
    split into minibatches, and each minibatch takes one Adam step on the
    clipped policy loss (policy and [log_std]) and one on the squared error
    of the value network, each plus [symmetry_coef] times its part of
    [symmetry_loss]. [l] unchanged if [samples] is empty. *)

val losses : learner -> sample array -> float * float
(** [losses l samples] is [(policy, value)]: the clipped policy loss and the
    value network's mean squared error that [update] minimizes (without the
    symmetry penalty), for [l]'s
    current networks on all of [samples] (advantages normalized as in
    [update]). A value loss that stays high while the policy loss settles
    hints that the value network is too small for the returns.
    [(nan, nan)] if [samples] is empty. *)

val symmetry_loss : learner -> sample array -> float * float
(** [symmetry_loss l samples] is [(policy, value)]: how far [l]'s networks
    are from mirror-symmetric at the observations of [samples]. For the
    policy it is the mean, over [flips] [g], samples [o] and both move
    components, of the square of [mean (flip_obs g o) - flip_act g (mean o)];
    for the value, of [value (flip_obs g o) - value o]. [0.] for networks
    that are exactly symmetric. [(nan, nan)] if [samples] is empty. *)

val write : out_channel -> agent -> unit
(** [write oc ag] writes [ag]'s networks and [log_std] to [oc] as text. *)

val read : Scanf.Scanning.in_channel -> agent
(** [read ic] reads back an agent written by [write].
    Raises [Failure] or [Scanf.Scan_failure] if [ic] does not hold one, or
    if its widths break the representation invariant. *)
