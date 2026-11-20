(* game.ml *)

(* true if [p] overlaps any living hunter *)
let caught hunters (p : Entity.t) =
  List.exists
    (fun (h : Entity.t) ->
      h.alive && Rect.overlaps (Entity.hitbox h) (Entity.hitbox p))
    hunters

let step w dir dt =
  let p = World.player w in
  if not p.alive then w
  else
    let moved = Physics.move (World.walls w) p dir dt in
    let p' =
      if caught (World.hunters w) moved then { moved with alive = false }
      else moved
    in
    World.with_player w p'

let is_over w = not (World.player w).alive
