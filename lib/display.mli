(** Text output for the terminal prototype. *)

val render : World.t -> Game.outcome -> string
(** [render w o] lists the hider's and each hunter's coordinates, then the
    outcome [o], one item per line. *)
