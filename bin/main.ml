open Hideseek

let draw w outcome =
  Raylib.begin_drawing ();
  Raylib.clear_background Raylib.Color.raywhite;
  Raylib.draw_text (Display.render w outcome) 10 10 20 Raylib.Color.black;
  Raylib.end_drawing ()

(* one iteration per frame; once the game is over the world stops changing *)
let rec loop w outcome =
  if not (Raylib.window_should_close ()) then begin
    let w, outcome =
      if outcome = Game.Mid_game then
        Game.step
          (module Strategy.Stay)
          w
          (User_input_strategy.direction_of_keys Raylib.is_key_down)
          (Raylib.get_frame_time ())
      else (w, outcome)
    in
    draw w outcome;
    loop w outcome
  end

let () =
  (* a fixed-size window (min = max size) never shows up under some tiling
     compositors via xwayland-satellite; resizable windows do *)
  Raylib.set_config_flags Raylib.ConfigFlags.window_resizable;
  Raylib.init_window 400 400 "hideseek";
  Raylib.set_target_fps 60;
  (* Raylib quits on Esc by default; Q matches the old terminal prototype *)
  Raylib.set_exit_key Raylib.Key.Q;
  loop World.default Game.Mid_game;
  Raylib.close_window ()
