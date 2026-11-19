let resolve_x walls (e : Entity.t) dx : Entity.t = 
  let new_pos = { e.pos with x = e.pos.x +. dx } in

  let new_e = { e with pos = new_pos } in
  match List.find_opt (fun w -> Rect.overlaps w (Entity.hitbox new_e)) walls with
  | None -> new_e
  | Some wall ->
      let snapped_x =
        if dx > 0. then wall.x -. e.size
        else wall.x +. wall.w in
      { new_e with pos = {e.pos with x = snapped_x }}

 
let resolve_y walls (e : Entity.t) dy : Entity.t = 
  let new_pos = { e.pos with y = e.pos.y +. dy } in
  let new_e = { e with pos = new_pos } in
  match List.find_opt (fun w -> Rect.overlaps w (Entity.hitbox new_e)) walls with
  | None -> new_e
  | Some wall ->
      let snapped_y =
        if dy > 0. then wall.y -. e.size
        else wall.y +. wall.h in
      { new_e with pos = {e.pos with y = snapped_y }}

let move walls (e : Entity.t) dir dt = 
  let d = Vec.scale (e.speed *. dt) (Vec.normalize dir) in
  let e_resolvedx = resolve_x walls e d.x in
  resolve_y walls e_resolvedx d.y