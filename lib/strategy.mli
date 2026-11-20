(** strategy.mli *)
val decide : Rect.t list -> Entity.t list -> Entity.t -> Vec2d.t
(** Direction our AI camel wants to move. Returns Vec2d.zero for now. *)
