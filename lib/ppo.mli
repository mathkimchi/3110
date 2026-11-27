(** Proximal Policy Optimization for one side of the game: a policy network
    that picks one of 9 moves, like the keyboard player (issue #37), and a
    value network that guesses how good a position is. Both are [Mlp]s, of
    widths 4-16-16-9 (policy) and 4-16-16-16-1 (value).

    Hyperparameters: discount halflife 20 s, GAE lambda halflife 5 s,
    clip 0.2, Adam with learning rate 3e-3, 4 epochs of minibatches of 256
    per update, symmetry penalty weight 1, entropy bonus weight 0.01. *)

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

val entropy_coef : float
(** [0.01], the weight of the entropy bonus [update] subtracts from the
    policy loss (see [entropy]). *)

(** {2 Moves} *)

val n_moves : int
(** [9]: staying still and the 8 directions. *)

val move : int -> Vec2d.t
(** [move i] is the direction of move [i]: [i mod 3 - 1] in x and
    [i / 3 - 1] in y, so [move 4] is [Vec2d.zero] (stay), [move 0] is
    up-left and [move 5] is right. Same speed every way once
    [Physics.move] normalizes it.
    Raises [Invalid_argument] unless [0 <= i < n_moves]. *)

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

val flip_act : float * float -> int -> int
(** [flip_act (sx, sy) i] is the mirrored move: the [j] with
    [move j = { x = sx *. (move i).x; y = sy *. (move i).y }].
    Requires: [0 <= i < n_moves]. *)

type agent
(** A policy network and a value network.
    AF: in a state with observation [o], the agent picks move [i] with
    probability [softmax (Mlp.forward policy o)].(i), and expects a return
    of [Mlp.forward value o].
    RI: policy widths are 4-...-9 ([n_moves]) and value widths are
    4-...-1. *)

val init : Random.State.t -> agent
(** [init rng] is a fresh 4-16-16-9 / 4-16-16-16-1 agent with weights drawn
    from [rng]. *)

val observe : World.t -> Entity.t -> float array
(** [observe w e] is [[| x; y; ox; oy |]]: the center of [e] and the center
    of the closest living entity of the other role, each measured from the
    middle of the arena ([(arena /. 2., arena /. 2.)]) and divided by
    [arena]. If there is no such entity, [(ox, oy)] is [e]'s own center. *)

val log_probs : agent -> float array -> float array
(** [log_probs ag o] is, for each move, the log probability that [ag]'s
    policy picks it at observation [o]; [n_moves] entries whose
    exponentials sum to 1. *)

val action_log_prob : agent -> float array -> int -> float
(** [action_log_prob ag o i] is [(log_probs ag o).(i)]. *)

val act : Random.State.t -> agent -> float array -> int * float
(** [act rng ag o] samples a move [i] for observation [o] from [ag]'s
    policy using [rng], and is [(i, action_log_prob ag o i)]. *)

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
  act : int;  (** the move taken, see [move] *)
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
    clipped policy loss minus [entropy_coef] times [entropy], and one on
    the squared error of the value network, each plus [symmetry_coef] times
    its part of [symmetry_loss]. [l] unchanged if [samples] is empty. *)

val losses : learner -> sample array -> float * float
(** [losses l samples] is [(policy, value)]: the clipped policy loss and the
    value network's mean squared error that [update] minimizes (without the
    entropy bonus and symmetry penalty), for [l]'s
    current networks on all of [samples] (advantages normalized as in
    [update]). A value loss that stays high while the policy loss settles
    hints that the value network is too small for the returns.
    [(nan, nan)] if [samples] is empty. *)

val symmetry_loss : learner -> sample array -> float * float
(** [symmetry_loss l samples] is [(policy, value)]: how far [l]'s networks
    are from mirror-symmetric at the observations of [samples]. For the
    policy it is the mean, over [flips] [g], samples [o] and moves [i], of
    the square of
    [(log_probs (flip_obs g o)).(i) - (log_probs o).(flip_act g i)];
    for the value, of [value (flip_obs g o) - value o]. [0.] for networks
    that are exactly symmetric. [(nan, nan)] if [samples] is empty. *)

val entropy : learner -> sample array -> float
(** [entropy l samples] is the mean, over the observations of [samples], of
    the entropy of [l]'s move probabilities there, in nats: [log 9] (about
    2.2) when every move is equally likely, [0.] when one move is certain.
    [nan] if [samples] is empty. *)

val write : out_channel -> agent -> unit
(** [write oc ag] writes [ag]'s networks to [oc] as text. *)

val read : Scanf.Scanning.in_channel -> agent
(** [read ic] reads back an agent written by [write].
    Raises [Failure] or [Scanf.Scan_failure] if [ic] does not hold one, or
    if its widths break the representation invariant. *)
