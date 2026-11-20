(** Game rules: moving everyone and deciding who gets caught. *)

val step : World.t -> Vec2d.t -> float -> World.t
(** [step w dir dt] moves the player in direction [dir] for [dt] seconds,
    then kills the player if they overlap any living hunter. A dead player
    does not move. Hunters do not move yet. *)

val is_over : World.t -> bool
(** [is_over w] is true once the player is dead. *)
