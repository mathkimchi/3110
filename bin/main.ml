open Hideseek

let draw w outcome =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.raywhite;
  Raylib.draw_text (Display.render w outcome) 10 10 20 Raylib.Color.black;
  Raylib.end_drawing ()

(* one iteration per frame; once the game is over the world stops changing *)
let rec loop g outcome =
  if not (Raylib.window_should_close ()) then begin
    let g, outcome =
      if outcome = Game.Ongoing then Game.step g (Raylib.get_frame_time ())
      else (g, outcome)
    in
    draw (Game.world g) outcome;
    loop g outcome
  end

let () =
  (* a fixed-size window (min = max size) never shows up under some tiling
     compositors via xwayland-satellite; resizable windows do *)
  Raylib.set_config_flags Raylib.ConfigFlags.window_resizable;
  Raylib.init_window 400 400 "hideseek";
  Raylib.set_target_fps 60;
  (* Raylib quits on Esc by default; Q matches the old terminal prototype *)
  Raylib.set_exit_key Raylib.Key.Q;
  (* World.default is [ hider 0; hunter 1 ]: the keyboard drives the hider *)
  let g =
    Game.init ~time_limit:20. World.default
      [ (module User_input_strategy); (module Strategy.Stay) ]
  in
  loop g Game.Ongoing;
  Raylib.close_window ()
