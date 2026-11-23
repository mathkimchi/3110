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

let draw_world g =
  let w = Game.world g in
  let draw_wall (r : Rect.t) =
    Raylib.draw_rectangle
      (int_of_float r.pos.x)
      (int_of_float r.pos.y)
      (int_of_float r.w)
      (int_of_float r.h)
      Raylib.Color.gray
  in
  List.iter draw_wall (World.walls w)

let draw_entities g =
  let w = Game.world g in
  let draw_entity (e : Entity.t) =
    if e.alive then
      let color =
        match e.role with
        | Entity.Hider -> Raylib.Color.blue
        | Entity.Hunter -> Raylib.Color.red
      in
      Raylib.draw_rectangle
        (int_of_float e.pos.x)
        (int_of_float e.pos.y)
        (int_of_float e.size)
        (int_of_float e.size)
        color
  in
  List.iter draw_entity (World.entities w)

let draw_hud g outcome =
  let time_text = Printf.sprintf "Time: %.1f" (Game.time_left g)
  in
  let outcome_text =
    match outcome with
    | Game.Ongoing -> "Ongoing"
    | Game.Hunters_win -> "Hunters win"
    | Game.Hiders_win -> "Hiders win"
  in
  Raylib.draw_text time_text 10 10 20 Raylib.Color.black;
  Raylib.draw_text outcome_text 10 35 20 Raylib.Color.black
