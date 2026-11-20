(** [pos] is the 2d coordinate of the bottom left corner *)
type t = { pos: Vec2d.t; w : float; h : float }
let overlaps a b =
  a.pos.x < b.pos.x +. b.w && b.pos.x < a.pos.x +. a.w && a.pos.y < b.pos.y +. b.h && b.pos.y < a.pos.y +. a.h
