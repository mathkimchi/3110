(** Game rules: moving everyone and deciding who gets caught. *)

type outcome = Hunters_win | Hiders_win | Ongoing
(** Who has won, by role; [Game] does not know which side the human is on.
    [Hiders_win] is not produced yet: the hiders' win condition is undecided. *)

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
(** [step g dt] advances [g] by [dt] seconds: every living entity's strategy
    decides a direction from the same snapshot of the world, then they all
    move for [dt] seconds, then each hider that overlaps any living hunter
    dies. Dead entities do not move. The outcome is [Hunters_win] once no
    hider is alive, otherwise [Ongoing]. *)
