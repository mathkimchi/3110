(* game.ml *)

type outcome = Victory | Defeat | Mid_game

(* true if [e] overlaps any living hunter *)
let caught hunters (e : Entity.t) =
  List.exists
    (fun (h : Entity.t) ->
      h.alive && Rect.overlaps (Entity.hitbox h) (Entity.hitbox e))
    hunters

let any_alive es = List.exists (fun (e : Entity.t) -> e.alive) es

type t = { world : World.t; strategies : (module Strategy.S) list }

let init world strategies =
  if List.length strategies <> List.length (World.entities world) then
    invalid_arg "Game.init: need exactly one strategy per entity";
  { world; strategies }

let world g = g.world

let step g dt =
  let w = g.world in
  if not (any_alive (World.hiders w)) then (g, Defeat)
  else
    let walls = World.walls w in
    (* moves the living entities of [role], each by its own strategy, all
       deciding from [w] *)
    let move role w =
      World.with_entities w
        (List.map2
           (fun (e : Entity.t) strat ->
             let module S = (val strat : Strategy.S) in
             if e.role = role && e.alive then
               Physics.move walls e (S.decide w e) dt
             else e)
           (World.entities w) g.strategies)
    in
    (* hiders move first, so hunters decide based on where the hiders are now *)
    let w = move Hunter (move Hider w) in
    (* check after everyone has moved, so a hunter walking into a hider counts *)
    let hunters = World.hunters w in
    let w =
      World.with_entities w
        (List.map
           (fun (e : Entity.t) ->
             if e.role = Hider && e.alive && caught hunters e then
               { e with alive = false }
             else e)
           (World.entities w))
    in
    let g = { g with world = w } in
    if any_alive (World.hiders w) then (g, Mid_game) else (g, Defeat)
