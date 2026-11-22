let string_of_outcome = function
  | Game.Hunters_win -> "Hunters_win"
  | Game.Hiders_win -> "Hiders_win"
  | Game.Ongoing -> "Ongoing"

let coords (e : Entity.t) = Printf.sprintf "(%.1f, %.1f)" e.pos.x e.pos.y

let string_of_role = function
  | Entity.Hider -> "Hider"
  | Entity.Hunter -> "Hunter"

let render g o =
  let line (e : Entity.t) =
    Printf.sprintf "%s %d: %s" (string_of_role e.role) e.id (coords e)
  in
  let time = Printf.sprintf "Time left: %.1f" (Game.time_left g) in
  let status = "Status: " ^ string_of_outcome o in
  String.concat "\n"
    (List.map line (World.entities (Game.world g)) @ [ time; status ])
