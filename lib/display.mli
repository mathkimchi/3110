(** Text output for the prototype: the lines main draws in the window. *)

val render : Game.t -> Game.outcome -> string
(** [render g o] lists each entity's role, id and coordinates, then the
    seconds left in [g] (one decimal), then the outcome [o], one item per
    line. *)

val draw_world : Game.t -> unit
(** [draw_world g] draws the walls of the game world. *)

val draw_entities : Game.t -> unit
(** [draw_entities g] draws the living entities in the game world. *)
