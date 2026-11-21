let string_of_outcome = function
  | Game.Victory -> "Victory"
  | Game.Defeat -> "Defeat"
  | Game.Mid_game -> "Mid_game"

let coords (e : Entity.t) = Printf.sprintf "(%.1f, %.1f)" e.pos.x e.pos.y

let string_of_role = function
  | Entity.Hider -> "Hider"
  | Entity.Hunter -> "Hunter"

let render w o =
  let line (e : Entity.t) =
    Printf.sprintf "%s %d: %s" (string_of_role e.role) e.id (coords e)
  in
  let status = "Status: " ^ string_of_outcome o in
  String.concat "\n" (List.map line (World.entities w) @ [ status ])
