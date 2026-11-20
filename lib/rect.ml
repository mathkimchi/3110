(** [pos] is the top-left corner; x grows rightward, y grows downward *)
type t = { pos: Vec2d.t; w : float; h : float }
let overlaps a b =
  a.pos.x < b.pos.x +. b.w && b.pos.x < a.pos.x +. a.w && a.pos.y < b.pos.y +. b.h && b.pos.y < a.pos.y +. a.h
