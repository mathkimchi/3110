type role = Hunter | Hider
type t = { id : int; role : role; pos : Vec2d.t; size : float; speed : float; alive : bool }
let hitbox (e: t) = Rect.{ pos = e.pos; w = e.size; h = e.size }
