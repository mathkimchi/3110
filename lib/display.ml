let string_of_outcome = function
  | Game.Victory -> "Victory"
  | Game.Defeat -> "Defeat"
  | Game.Mid_game -> "Mid_game"

let coords (e : Entity.t) = Printf.sprintf "(%.1f, %.1f)" e.pos.x e.pos.y

let render w o =
  let hider = "Hider: " ^ coords (World.hider w) in
  let hunters =
    List.map
      (fun (h : Entity.t) -> Printf.sprintf "Hunter %d: %s" h.id (coords h))
      (World.hunters w)
  in
  let status = "Status: " ^ string_of_outcome o in
  String.concat "\n" ((hider :: hunters) @ [ status ])
