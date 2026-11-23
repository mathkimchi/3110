(** Headless rounds for the training binary: play games without a window and
    count who wins. *)

val dt : float
(** The fixed step every training round uses: [1. /. 20.] seconds. *)

val play : Game.t -> Game.outcome * float
(** [play g] steps [g] by [dt] until the outcome is not [Ongoing], and is that
    outcome with how many game seconds passed. Never [Ongoing]. *)

type tally
(** How many rounds each side has won so far. *)

val empty : tally
(** No rounds played. *)

val record : tally -> Game.outcome -> tally
(** [record t o] is [t] with one more win for the side [o] names.
    Raises [Invalid_argument] if [o] is [Ongoing]. *)

val rounds : tally -> int
(** [rounds t] is how many rounds [t] has recorded. *)

val hider_wins : tally -> int
val hunter_wins : tally -> int

val hider_rate : tally -> float
(** [hider_rate t] is the fraction of rounds in [t] the hiders won, in
    [0., 1.]; [0.] if there are no rounds. *)

val hunter_rate : tally -> float
(** [hunter_rate t] is the fraction of rounds in [t] the hunters won, in
    [0., 1.]; [0.] if there are no rounds. *)

(** {2 PPO rounds} *)

val step_reward : World.t -> Entity.role -> float
(** [step_reward w r] is the reward for side [r] after one step that ended in
    [w]: the distance between the centers of the first hider and the first
    hunter of [w], divided by [Ppo.arena] and multiplied by [dt]; positive for
    [Hider] and negative for [Hunter].
    Requires: [w] has at least one hider and one hunter. *)

val terminal_value : float
(** The size of [end_reward] for a finished round. *)

val end_reward : Game.outcome -> Entity.role -> float
(** [end_reward o r] is [terminal_value] if side [r] won outcome [o],
    [-. terminal_value] if it lost,
    and [0.] if [o] is [Ongoing]. *)

type experience = { hider : Ppo.sample array; hunter : Ppo.sample array }
(** The steps each PPO side took in a round, in order, with advantages and
    returns from [Ppo.gae ~gamma:(Ppo.gamma ~dt) ~lambda:(Ppo.lambda ~dt)]. A side
    played by [Greedy_strategy] has no samples. *)

val play_ppo :
  ?on_step:(Game.t -> Game.outcome -> unit) ->
  Random.State.t ->
  time_limit:float ->
  hider:Ppo.agent option ->
  hunter:Ppo.agent option ->
  World.t ->
  Game.outcome * float * experience
(** [play_ppo rng ~time_limit ~hider ~hunter w] plays one round on [w] like
    [play]. The hider is driven by agent [hider] (sampling its moves with
    [rng]) or, if [None], by [Greedy_strategy]; likewise the hunter. Each
    step a PPO side moves in earns it [step_reward], and its last step also
    earns [end_reward] of the outcome. Is [(outcome, seconds, experience)].
    After every step, [on_step g o] (default: does nothing) is called with
    the game and outcome after that step, e.g. to draw it.
    Raises [Invalid_argument] if [w] does not have exactly one hider and one
    hunter, or if [time_limit <= 0.]. *)

val check_files :
  hider_ppo:bool -> hunter_ppo:bool -> out:string option -> input:string option -> unit
(** [check_files ~hider_ppo ~hunter_ppo ~out ~input] checks the training
    options: saving to [out] or loading from [input] needs at least one PPO
    side. Raises [Invalid_argument] if neither side is PPO but [out] or
    [input] is given. *)
