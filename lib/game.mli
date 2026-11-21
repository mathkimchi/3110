(** Game rules: moving everyone and deciding who gets caught. *)

type outcome = Victory | Defeat | Mid_game
(** [Victory] is not produced yet: the win condition is undecided. *)

type t
(** A game in progress: the world plus the strategy controlling each entity. *)

val init : World.t -> (module Strategy.S) list -> t
(** [init w strats] is a game on [w] where the [i]th strategy in [strats]
    controls the [i]th entity of [World.entities w]. The game never reorders
    or removes entities, so this pairing holds for the whole game.
    Raises [Invalid_argument] if [strats] and the entities differ in length. *)

val world : t -> World.t
(** [world g] is the current world of [g]. *)

val step : t -> float -> t * outcome
(** [step g dt] advances [g] by [dt] seconds: every living hider moves in the
    direction its strategy chooses, then every living hunter does (seeing
    where the hiders now are), then each hider that overlaps any living hunter
    dies. Dead entities do not move. The outcome is [Defeat] once no hider is
    alive. *)
