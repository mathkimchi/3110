val move : Rect.t list -> Entity.t -> Vec2d.t -> float -> Entity.t
(** [move walls e dir dt] moves [e] in direction [dir] (any length, normalized
    inside) for [dt] seconds, stopping flush against walls and sliding along them. *)

  
