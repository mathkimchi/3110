type t
val init : Rect.t list -> Entity.t list -> t
val step : t -> (int * Vec2d.t) list -> float -> t
val entities : t -> Entity.t list
val walls : t -> Rect.t list
val is_over : t -> bool
