(** Game rules: moving everyone and deciding who gets caught. *)

type outcome = Hunters_win | Hiders_win | Ongoing
(** Who has won, by role; [Game] does not know which side the human is on.
    Hunters win by catching every hider; hiders win by surviving until the
    time limit. *)

type t
(** A game in progress: the world, the strategy controlling each entity, and
    how much game time has passed. *)

val init : time_limit:float -> World.t -> (module Strategy.S) list -> t
(** [init ~time_limit w strats] is a game on [w] that the hiders win if any of
    them survives [time_limit] seconds, where the [i]th strategy in [strats]
    controls the [i]th entity of [World.entities w]. The game never reorders
    or removes entities, so this pairing holds for the whole game.
    Raises [Invalid_argument] if [time_limit <= 0.] or if [strats] and the
    entities differ in length. *)

val world : t -> World.t
(** [world g] is the current world of [g]. *)

val time_left : t -> float
(** [time_left g] is how many seconds the hiders still have to survive;
    never below [0.]. *)

val step : t -> float -> t * outcome
(** [step g dt] advances [g] by [dt] seconds: every living entity's strategy
    decides a direction from the same snapshot of the world, then they all
    move for [dt] seconds, then each hider that overlaps any living hunter
    dies. Dead entities do not move.

    The outcome is [Hunters_win] once no hider is alive. Otherwise, if the
    time had already run out before this step, nobody moves and the outcome
    is [Hiders_win]; so the step that uses up the time still plays out, and a
    catch during it is a hunter win. Otherwise the outcome is [Ongoing]. *)
