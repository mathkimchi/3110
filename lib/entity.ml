type role = Hunter | Hider
type t = { id : int; role : role; pos : Vec.t; size : float; speed : float; alive : bool }
let hitbox e = Rect.{ x = e.pos.x; y = e.pos.y; w = e.size; h = e.size }