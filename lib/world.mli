(** A snapshot of the game at one moment: the map and everyone on it.
    Coordinates are screen-style: x grows rightward, y grows downward. *)

type t

val init : walls:Rect.t list -> entities:Entity.t list -> t
(** [init ~walls ~entities] is the world containing exactly these, keeping the
    order of [entities]. *)

val walls : t -> Rect.t list

val entities : t -> Entity.t list
(** [entities w] is every entity in [w], hiders and hunters alike, in order. *)

val entities_by_role : t -> Entity.role -> Entity.t list
(** [entities_by_role w r] is the entities in [w] whose role is [r], in order. *)

val hiders : t -> Entity.t list
(** [hiders w] is [entities_by_role w Hider]. *)

val hunters : t -> Entity.t list
(** [hunters w] is [entities_by_role w Hunter]. *)

val with_entities : t -> Entity.t list -> t
(** [with_entities w es] is [w] with its entities replaced by [es]. *)

val default : t
(** A small hard-coded map for the prototype: a walled 400x400 arena with one
    wall in the middle, the hider in the top-left and one hunter in the
    bottom-right. The hunter (speed 90) is slower than the hider (speed 120). *)
