type t = { walls : Rect.t list; hider : Entity.t; hunters : Entity.t list }

let init ~walls ~hider ~hunters = { walls; hider; hunters }
let walls w = w.walls
let hider w = w.hider
let hunters w = w.hunters
let with_hider w hider = { w with hider }
let with_hunters w hunters = { w with hunters }

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
    hider = entity 0 Entity.Hider 50. 50.;
    hunters = [ entity 1 Entity.Hunter 330. 330. ];
  }
