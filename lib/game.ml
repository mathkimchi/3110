(* game.ml *)
type t = { walls : Rect.t list; entities : Entity.t list }

let init walls entities = { walls; entities }
let entities st = st.entities
let walls st = st.walls

(* direction for entity [id], or Vec2d.zero if it got no input *)
let input_for inputs id = match List.assoc_opt id inputs with
| None -> Vec2d.zero
| Some direction -> direction

(* move one entity; dead entities stay put *)
let move_one walls inputs dt (e : Entity.t) = if not e.alive then e 
else Physics.move walls e (input_for inputs e.id) dt

(* true if this hider overlaps any living hunter *)
let caught hunters (e : Entity.t) = 
  List.exists (fun x -> Rect.overlaps (Entity.hitbox x) (Entity.hitbox e)) hunters

(* mark every caught hider as not alive *)
let apply_kills entities = 
  let hunters =
    List.filter (fun (e : Entity.t) -> e.role = Entity.Hunter && e.alive) entities
  in
  List.map
    (fun (e : Entity.t) ->
      if e.role = Entity.Hider && caught hunters e then { e with alive = false }
      else e)
    entities

let step st inputs dt =
  let moved = List.map (move_one st.walls inputs dt) st.entities in
  { st with entities = apply_kills moved }

(* game ends when no hider is alive *)
let is_over st =
  List.for_all (fun (e : Entity.t) -> e.role = Entity.Hunter || not e.alive) st.entities
