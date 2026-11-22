(** Text output for the prototype: the lines main draws in the window. *)

val render : Game.t -> Game.outcome -> string
(** [render g o] lists each entity's role, id and coordinates, then the
    seconds left in [g] (one decimal), then the outcome [o], one item per
    line. *)
