(** A snapshot of the game at one moment: the map and everyone on it.
    Coordinates are screen-style: x grows rightward, y grows downward. *)

type t

val init : walls:Rect.t list -> player:Entity.t -> hunters:Entity.t list -> t
(** [init ~walls ~player ~hunters] is the world containing exactly these. *)

val walls : t -> Rect.t list
val player : t -> Entity.t
val hunters : t -> Entity.t list

val with_player : t -> Entity.t -> t
(** [with_player w p] is [w] with its player replaced by [p]. *)

val with_hunters : t -> Entity.t list -> t
(** [with_hunters w hs] is [w] with its hunters replaced by [hs]. *)

val default : t
(** A small hard-coded map for the prototype: a walled 400x400 arena with one
    wall in the middle, the player in the top-left and one hunter in the
    bottom-right. *)
