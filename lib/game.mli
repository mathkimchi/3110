(** Game rules: moving everyone and deciding who gets caught. *)

type outcome = Victory | Defeat | Mid_game
(** [Victory] is not produced yet: the win condition is undecided. *)

val step :
  (module Strategy.S) -> World.t -> Vec2d.t -> float -> World.t * outcome
(** [step strat w dir dt] moves every living hider in direction [dir] for
    [dt] seconds, then moves each hunter in the direction [strat] chooses, then
    kills each hider that overlaps any living hunter. Dead hiders do not move.
    The outcome is [Defeat] once no hider is alive. *)
