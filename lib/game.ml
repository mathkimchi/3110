(* game.ml *)

type outcome = Victory | Defeat | Mid_game

(* true if [e] overlaps any living hunter *)
let caught hunters (e : Entity.t) =
  List.exists
    (fun (h : Entity.t) ->
      h.alive && Rect.overlaps (Entity.hitbox h) (Entity.hitbox e))
    hunters

let any_alive es = List.exists (fun (e : Entity.t) -> e.alive) es

let step strat w dir dt =
  let module S = (val strat : Strategy.S) in
  if not (any_alive (World.hiders w)) then (w, Defeat)
  else
    let walls = World.walls w in
    let update f w = World.with_entities w (List.map f (World.entities w)) in
    (* hiders move first, so hunters decide based on where the hiders are now *)
    let w =
      update
        (fun (e : Entity.t) ->
          if e.role = Hider && e.alive then Physics.move walls e dir dt else e)
        w
    in
    let w =
      update
        (fun (e : Entity.t) ->
          if e.role = Hunter then Physics.move walls e (S.decide w e) dt else e)
        w
    in
    (* check after everyone has moved, so a hunter walking into a hider counts *)
    let hunters = World.hunters w in
    let w =
      update
        (fun (e : Entity.t) ->
          if e.role = Hider && e.alive && caught hunters e then
            { e with alive = false }
          else e)
        w
    in
    if any_alive (World.hiders w) then (w, Mid_game) else (w, Defeat)
