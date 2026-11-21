type t = { walls : Rect.t list; entities : Entity.t list }

let init ~walls ~entities = { walls; entities }
let walls w = w.walls
let entities w = w.entities

let entities_by_role w role =
  List.filter (fun (e : Entity.t) -> e.role = role) w.entities

let hiders w = entities_by_role w Entity.Hider
let hunters w = entities_by_role w Entity.Hunter
let with_entities w entities = { w with entities }

let rect x y w h = Rect.{ pos = Vec2d.{ x; y }; w; h }

let default =
  let size = 400. and thick = 20. in
  let walls =
    [
      rect 0. 0. size thick; (* top *)
      rect 0. (size -. thick) size thick; (* bottom *)
      rect 0. 0. thick size; (* left *)
      rect (size -. thick) 0. thick size; (* right *)
      rect 190. 100. 20. 200.; (* middle *)
    ]
  in
  let entity id role x y : Entity.t =
    { id; role; pos = Vec2d.{ x; y }; size = 20.; speed = 120.; alive = true }
  in
  {
    walls;
    entities =
      [ entity 0 Entity.Hider 50. 50.; entity 1 Entity.Hunter 330. 330. ];
  }
