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
