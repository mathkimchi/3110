open Hideseek

(* each key press advances the game by 1/12 s; at speed 120 that's 10 px *)
let dt = 1. /. 12.

let direction_of_char = function
  | 'w' -> Vec2d.{ x = 0.; y = -1. }
  | 'a' -> Vec2d.{ x = -1.; y = 0. }
  | 's' -> Vec2d.{ x = 0.; y = 1. }
  | 'd' -> Vec2d.{ x = 1.; y = 0. }
  | _ -> Vec2d.zero

(* "wd" -> up + right; Physics.move normalizes, so diagonals aren't faster *)
let direction_of_key key =
  String.fold_left
    (fun acc c -> Vec2d.add acc (direction_of_char c))
    Vec2d.zero key

let rec loop w =
  print_string "> ";
  match String.trim (read_line ()) with
  | exception End_of_file -> () (* Ctrl-D *)
  | "q" -> ()
  | key ->
      let w, outcome =
        Game.step (module Strategy.Stay) w (direction_of_key key) dt
      in
      print_endline (Display.render w outcome);
      if outcome = Game.Mid_game then loop w

let () =
  print_endline
    "w/a/s/d + Enter to move (combine for diagonals, e.g. wd), Enter to wait, \
     q to quit";
  print_endline (Display.render World.default Game.Mid_game);
  loop World.default
