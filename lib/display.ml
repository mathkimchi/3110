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

type sprites = { camel : Raylib.Texture.t option; cactus : Raylib.Texture.t option; overlay:  Raylib.Texture.t option}

(* [load_png path] is the picture in file [path], or [None] if there is no
   such file. *)
let load_png path =
  if Sys.file_exists path then begin
    let tex = Raylib.load_texture path in
    Raylib.set_texture_filter tex Raylib.TextureFilter.Bilinear;
    Some tex
  end
  else None
let load_sprites () =
  { camel = load_png "assets/camel.png"; cactus = load_png "assets/cactus.png";  overlay = load_png "assets/overlay.png"; }

let unload_sprites s =
  Option.iter Raylib.unload_texture s.camel;
  Option.iter Raylib.unload_texture s.cactus;
  Option.iter Raylib.unload_texture s.overlay
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
(* top-left corner of the see-through game box in overlay.png *)
let box_origin = Vec2d.{ x = 468.; y = 152. }

(* canvas pixels per world unit: the 400-wide world fills the 800-wide box *)
let world_scale = 2.

(* [on_screen r] is world rectangle [r] moved and stretched into the box. *)
let on_screen (r : Rect.t) : Rect.t =
  {
    pos = Vec2d.add box_origin (Vec2d.scale world_scale r.pos);
    w = world_scale *. r.w;
    h = world_scale *. r.h;
  }
(* [draw_entity s e] draws [e] as its picture from [s], or as a square in its
   side's color if [s] has no picture for it. *)
let draw_entity s (e : Entity.t) =
  match sprite_of_role s e.role with
  | Some tex -> draw_png tex (on_screen (Entity.hitbox e))
  | None -> draw_rect (color_of_role e.role) (on_screen (Entity.hitbox e))


(* the size of overlay.png; everything is placed in these pixels *)
let canvas_w = 1324.
let canvas_h = 1080.

(* [fit_camera ()] shrinks or grows the whole canvas to fit the window,
   centered, without stretching it. *)
let fit_camera () =
  let win_w = float_of_int (Raylib.get_screen_width ()) in
  let win_h = float_of_int (Raylib.get_screen_height ()) in
  let zoom = Float.min (win_w /. canvas_w) (win_h /. canvas_h) in
  let offset =
    Raylib.Vector2.create
      ((win_w -. (canvas_w *. zoom)) /. 2.)
      ((win_h -. (canvas_h *. zoom)) /. 2.)
  in
  Raylib.Camera2D.create offset (Raylib.Vector2.zero ()) 0. zoom
let ink = Raylib.Color.create 0 0 128 255 (* navy, like the overlay text *)

(* [draw_stats lines] writes [lines] down the STATS panel, one per row. *)
let draw_stats lines =
  List.iteri
    (fun i line -> Raylib.draw_text line 64 (248 + (34 * i)) 24 ink)
    lines

let draw sprites g outcome =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.raywhite;
  Raylib.begin_mode_2d (fit_camera ());
  List.iter (fun w -> draw_rect wall_color (on_screen w)) (World.walls (Game.world g));
  List.iter (draw_entity sprites) (World.entities (Game.world g));
  Option.iter (fun tex -> Raylib.draw_texture tex 0 0 Raylib.Color.white) sprites.overlay;
  let time_text = Printf.sprintf "Time: %.1f" (Game.time_left g) in
  let status_text = "Status: " ^ string_of_outcome outcome in
  draw_stats [ time_text; status_text ];
  Raylib.end_mode_2d ();
  Raylib.end_drawing ()