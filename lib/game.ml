(* game.ml *)

type outcome = Victory | Defeat | Mid_game

(* true if [p] overlaps any living hunter *)
let caught hunters (p : Entity.t) =
  List.exists
    (fun (h : Entity.t) ->
      h.alive && Rect.overlaps (Entity.hitbox h) (Entity.hitbox p))
    hunters

let step strat w dir dt =
  let module S = (val strat : Strategy.S) in
  let p = World.hider w in
  if not p.alive then (w, Defeat)
  else
    let walls = World.walls w in
    (* hider moves first, so hunters decide based on where the hider is now *)
    let w = World.with_hider w (Physics.move walls p dir dt) in
    let move_hunter h = Physics.move walls h (S.decide w h) dt in
    let w = World.with_hunters w (List.map move_hunter (World.hunters w)) in
    (* check after everyone has moved, so a hunter walking into the hider counts *)
    let p = World.hider w in
    if caught (World.hunters w) p then
      (World.with_hider w { p with alive = false }, Defeat)
    else (w, Mid_game)
