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


let wall_color = Raylib.Color.create 192 136 88 255
(* [draw_rect color r] draws rectangle [r] filled with [color]. *)
let draw_rect color (r : Rect.t) =
  Raylib.draw_rectangle_rec (Raylib.Rectangle.create r.pos.x r.pos.y r.w r.h) color

let camel_color = Raylib.Color.create 192 48 48 255 (*red*)
let cactus_color = Raylib.Color.create 0 128 0 255 (*green*)


(* [color_of_role r] is the color that side [r] is drawn in. This is mostly placeholder until assets are imported/backup if assets don't load*)
let color_of_role = function
  | Entity.Hunter -> camel_color
  | Entity.Hider -> cactus_color

type sprites = { camel : Raylib.Texture.t option; cactus : Raylib.Texture.t option }

(* [load_png path] is the picture in file [path], or [None] if there is no
   such file. *)
let load_png path =
  if Sys.file_exists path then Some (Raylib.load_texture path) else None

let load_sprites () =
  { camel = load_png "assets/camel.png"; cactus = load_png "assets/cactus.png" }

let unload_sprites s =
  Option.iter Raylib.unload_texture s.camel;
  Option.iter Raylib.unload_texture s.cactus

(* [sprite_of_role s r] is the picture for side [r], if [s] has one. *)
let sprite_of_role s = function
  | Entity.Hunter -> s.camel
  | Entity.Hider -> s.cactus

(* [draw_png tex r] draws the whole picture [tex] stretched to fill [r]. *)
let draw_png tex (r : Rect.t) =
  let w = float_of_int (Raylib.Texture.width tex) in
  let h = float_of_int (Raylib.Texture.height tex) in
  let whole_picture = Raylib.Rectangle.create 0. 0. w h in
  let target = Raylib.Rectangle.create r.pos.x r.pos.y r.w r.h in
  Raylib.draw_texture_pro tex whole_picture target (Raylib.Vector2.zero ()) 0.
    Raylib.Color.white

(* [draw_entity s e] draws [e] as its picture from [s], or as a square in its
   side's color if [s] has no picture for it. *)
let draw_entity s (e : Entity.t) =
  match sprite_of_role s e.role with
  | Some tex -> draw_png tex (Entity.hitbox e)
  | None -> draw_rect (color_of_role e.role) (Entity.hitbox e)



let draw sprites g outcome =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.raywhite;
  List.iter (draw_rect wall_color) (World.walls (Game.world g));
    List.iter (draw_entity sprites) (World.entities (Game.world g));
  Raylib.draw_text (render g outcome) 10 10 20 Raylib.Color.black;
  Raylib.end_drawing ()    



