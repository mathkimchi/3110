(** What the game window shows: [render] is the text, [draw] draws a frame. *)
(** Text output for the prototype: the lines main draws in the window. *)



val render : Game.t -> Game.outcome -> string
(** [render g o] lists each entity's role, id and coordinates, then the
    seconds left in [g] (one decimal), then the outcome [o], one item per
    line. *)

type sprites
(** The camel and cactus pictures.
    AF: each side has either a loaded picture or none; a side with no
    picture is drawn as a colored square.
    RI: every picture was loaded after the window opened. *)

val load_sprites : unit -> sprites
(** [load_sprites ()] loads [assets/camel.png] and [assets/cactus.png],
    skipping any file that does not exist.
    Requires: the Raylib window is open. *)

val unload_sprites : sprites -> unit
(** [unload_sprites s] frees the pictures in [s].
    Requires: the Raylib window is still open. *)

val draw : sprites -> Game.t -> Game.outcome -> unit
(** [draw s g o] draws one frame showing [g]: the walls in brown, the camel
    and cactus as their pictures from [s] (or red and green squares if a
    picture is missing), with the text of [render g o] on top.
    Requires: the Raylib window is open. *)