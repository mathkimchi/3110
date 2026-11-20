type t = { x : float; y : float; w : float; h : float }
let overlaps a b =
  a.x < b.x +. b.w && b.x < a.x +. a.w && a.y < b.y +. b.h && b.y < a.y +. a.h