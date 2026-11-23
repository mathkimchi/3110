open Hideseek

let draw g outcome =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.raywhite;
  Display.draw_world g;
  Display.draw_entities g;
  Display.draw_hud g outcome;
  Raylib.end_drawing ()

(* one iteration per frame; once the game is over the world stops changing *)
let rec loop g outcome =
  if not (Raylib.window_should_close ()) then begin
    let g, outcome =
      if outcome = Game.Ongoing then Game.step g (Raylib.get_frame_time ())
      else (g, outcome)
    in
    draw g outcome;
    loop g outcome
  end

(* seconds the hiders must survive; settable with --time *)
let time_limit = ref 20.

let set_time t =
  if t <= 0. then raise (Arg.Bad "--time must be greater than 0");
  time_limit := t

let speclist =
  [
    ( "--time",
      Arg.Float set_time,
      "SECONDS  how long the hiders must survive to win (default 20)" );
  ]

let usage = "game [--time=SECONDS]"

let () =
  Arg.parse speclist
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    usage;
  (* a fixed-size window (min = max size) never shows up under some tiling
     compositors via xwayland-satellite; resizable windows do *)
  Raylib.set_config_flags Raylib.ConfigFlags.window_resizable;
  Raylib.init_window 400 400 "hideseek";
  Raylib.set_target_fps 60;
  (* Raylib quits on Esc by default; Q matches the old terminal prototype *)
  Raylib.set_exit_key Raylib.Key.Q;
  (* World.default is [ hider 0; hunter 1 ]: the keyboard drives the hider
     and the hunter chases it greedily *)
  let g =
    Game.init ~time_limit:!time_limit World.default
      [ (module User_input_strategy); (module Greedy_strategy) ]
  in
  loop g Game.Ongoing;
  Raylib.close_window ()
