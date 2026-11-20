(** Game rules: moving everyone and deciding who gets caught. *)

type outcome = Victory | Defeat | Mid_game
(** [Victory] is not produced yet: the win condition is undecided. *)

val step :
  (module Strategy.S) -> World.t -> Vec2d.t -> float -> World.t * outcome
(** [step strat w dir dt] moves the player in direction [dir] for [dt]
    seconds, then moves each hunter in the direction [strat] chooses, then
    kills the player if they overlap any living hunter. A dead player does not
    move and the outcome stays [Defeat]. *)
