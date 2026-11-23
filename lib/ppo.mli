(** Proximal Policy Optimization for one side of the game: a policy network
    that picks a direction and a value network that guesses how good a
    position is. Both are [Mlp]s of widths 4-5-5-2 (policy) and 4-5-5-1
    (value), as in issue #29.

    Hyperparameters: discount 0.99, GAE lambda 0.95, clip 0.2, Adam with
    learning rate 3e-4, 4 epochs of minibatches of 256 per update. *)

val arena : float
(** [400.], the size of [World.default]; positions are divided by it so the
    networks see numbers around [0., 1.]. *)

val gamma : float
(** [0.99], the discount per step that [update]'s caller should pass to
    [gae]. *)

val lambda : float
(** [0.95], the GAE lambda to pass to [gae]. *)

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
    of the closest living entity of the other role, each divided by
    [arena]. If there is no such entity, [(ox, oy)] is [e]'s own center. *)

val log_prob : mean:float array -> log_std:float array -> float array -> float
(** [log_prob ~mean ~log_std a] is the log density of [a] under independent
    Gaussians with means [mean] and standard deviations [exp log_std].
    Requires: all three arrays have the same length. *)

val action_log_prob : agent -> float array -> float array -> float
(** [action_log_prob ag o a] is the log probability density of [ag]'s policy
    choosing direction [a] at observation [o]. *)

val act : Random.State.t -> agent -> float array -> float array * float
(** [act rng ag o] samples a direction [a] for observation [o] from [ag]'s
    policy using [rng], and is [(a, log_prob a)]. *)

val value : agent -> float array -> float
(** [value ag o] is [ag]'s value network on observation [o]. *)

val gae :
  gamma:float ->
  lambda:float ->
  rewards:float array ->
  values:float array ->
  float array * float array
(** [gae ~gamma ~lambda ~rewards ~values] is [(advantages, returns)] for one
    episode that ends after its last step (the value after it is [0.]):
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
    of the value network. [l] unchanged if [samples] is empty. *)

val losses : learner -> sample array -> float * float
(** [losses l samples] is [(policy, value)]: the clipped policy loss and the
    value network's mean squared error that [update] minimizes, for [l]'s
    current networks on all of [samples] (advantages normalized as in
    [update]). A value loss that stays high while the policy loss settles
    hints that the value network is too small for the returns.
    [(nan, nan)] if [samples] is empty. *)

val write : out_channel -> agent -> unit
(** [write oc ag] writes [ag]'s networks and [log_std] to [oc] as text. *)

val read : Scanf.Scanning.in_channel -> agent
(** [read ic] reads back an agent written by [write].
    Raises [Failure] or [Scanf.Scan_failure] if [ic] does not hold one, or
    if its widths break the representation invariant. *)
