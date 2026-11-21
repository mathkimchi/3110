(** A snapshot of the game at one moment: the map and everyone on it.
    Coordinates are screen-style: x grows rightward, y grows downward. *)

type t

val init : walls:Rect.t list -> hider:Entity.t -> hunters:Entity.t list -> t
(** [init ~walls ~hider ~hunters] is the world containing exactly these. *)

val walls : t -> Rect.t list
val hider : t -> Entity.t
val hunters : t -> Entity.t list

val with_hider : t -> Entity.t -> t
(** [with_hider w p] is [w] with its hider replaced by [p]. *)

val with_hunters : t -> Entity.t list -> t
(** [with_hunters w hs] is [w] with its hunters replaced by [hs]. *)

val default : t
(** A small hard-coded map for the prototype: a walled 400x400 arena with one
    wall in the middle, the hider in the top-left and one hunter in the
    bottom-right. *)
