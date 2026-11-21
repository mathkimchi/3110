(** Text output for the terminal prototype. *)

val render : World.t -> Game.outcome -> string
(** [render w o] lists each entity's role, id and coordinates, then the
    outcome [o], one item per line. *)
