(** logic.mli *)
val decide : Rect.t list -> Entity.t list -> Entity.t -> Vec.t
(** Direction our AI camel wants to move. Returns Vec.zero for now. *)