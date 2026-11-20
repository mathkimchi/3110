open OUnit2
open Hideseek

(* ---------- helpers ---------- *)

(* speed 600 and dt 1/60 means one call to move covers exactly 10 px *)
let dt = 1. /. 60.
let step_len = 10.
let diag = step_len /. sqrt 2.

let mk ?(id = 0) ?(role = Entity.Hider) ?(size = 10.) ?(speed = 600.) x y =
  Entity.{ id; role; pos = Vec2d.{ x; y }; size; speed; alive = true }

let v x y = Vec2d.{ x; y }
let right = v 1. 0.
let left = v (-1.) 0.
let up = v 0. (-1.)
let down = v 0. 1.

let feq a b = Float.abs (a -. b) < 1e-6

let assert_pos ~x ~y (e : Entity.t) =
  assert_equal ~cmp:feq ~printer:string_of_float ~msg:"x" x e.pos.x;
  assert_equal ~cmp:feq ~printer:string_of_float ~msg:"y" y e.pos.y

(* ---------- Physics.move ---------- *)

let wall = Rect.{ pos= { x = 100.; y = 0.;}; w = 20.; h = 200. } (* vertical, x 100..120 *)
let floor = Rect.{ pos= { x = 0.; y = 100.;}; w = 200.; h = 20. } (* horizontal, y 100..120 *)

(* same x range as [wall] but far below the entity *)
let far_wall = Rect.{ pos = {x = 100.; y = 300.;}; w = 20.; h = 100. }

let move_test name walls e dir ~x ~y =
  name >:: fun _ -> assert_pos ~x ~y (Physics.move walls e dir dt)

let physics_tests =
  [
    (* partition: direction, open space *)
    move_test "right, no walls" [] (mk 0. 0.) right ~x:10. ~y:0.;
    move_test "left, no walls" [] (mk 0. 0.) left ~x:(-10.) ~y:0.;
    move_test "up, no walls" [] (mk 0. 0.) up ~x:0. ~y:(-10.);
    move_test "down, no walls" [] (mk 0. 0.) down ~x:0. ~y:10.;
    move_test "zero dir stays put" [] (mk 5. 5.) Vec2d.zero ~x:5. ~y:5.;
    move_test "diagonal is not faster" [] (mk 0. 0.) (v 1. 1.) ~x:diag ~y:diag;
    move_test "long dir is normalized" [] (mk 0. 0.) (v 5. 0.) ~x:10. ~y:0.;
    (* partition: which side of the wall you hit; should end flush *)
    move_test "right into wall" [ wall ] (mk 85. 50.) right ~x:90. ~y:50.;
    move_test "left into wall" [ wall ] (mk 125. 50.) left ~x:120. ~y:50.;
    move_test "down into floor" [ floor ] (mk 50. 85.) down ~x:50. ~y:90.;
    move_test "up into floor" [ floor ] (mk 50. 125.) up ~x:50. ~y:120.;
    (* boundary cases *)
    move_test "already flush, push in" [ wall ] (mk 90. 50.) right ~x:90. ~y:50.;
    move_test "already flush, move away" [ wall ] (mk 90. 50.) left ~x:80. ~y:50.;
    move_test "wall in x range but not y" [ far_wall ] (mk 95. 50.) right ~x:105.
      ~y:50.;
    (* both axes at once *)
    move_test "slide along wall" [ wall ] (mk 85. 50.) (v 1. 1.) ~x:90.
      ~y:(50. +. diag);
    move_test "stop in corner" [ wall; floor ] (mk 85. 85.) (v 1. 1.) ~x:90. ~y:90.;
  ]

(* ---------- Game.step (these fail until step is written) ---------- *)

let find id st = List.find (fun (e : Entity.t) -> e.id = id) (Game.entities st)
let hunter = mk ~id:1 ~role:Entity.Hunter 50. 50.

let game_tests =
  [
    ( "no input means no movement" >:: fun _ ->
      let st = Game.step (Game.init [] [ hunter ]) [] dt in
      assert_pos ~x:50. ~y:50. (find 1 st) );
    ( "input moves only that entity" >:: fun _ ->
      let st = Game.step (Game.init [] [ mk 0. 0.; hunter ]) [ (0, right) ] dt in
      assert_pos ~x:10. ~y:0. (find 0 st);
      assert_pos ~x:50. ~y:50. (find 1 st) );
    ( "hider overlapping hunter dies" >:: fun _ ->
      let st = Game.step (Game.init [] [ mk 55. 55.; hunter ]) [] dt in
      assert_bool "hider should be dead" (not (find 0 st).alive);
      assert_bool "hunter should be alive" (find 1 st).alive );
    ( "hider moving into hunter dies" >:: fun _ ->
      let st = Game.step (Game.init [] [ mk 35. 50.; hunter ]) [ (0, right) ] dt in
      assert_bool "hider should be dead" (not (find 0 st).alive) );
    ( "flush against hunter is not a touch" >:: fun _ ->
      let st = Game.step (Game.init [] [ mk 40. 50.; hunter ]) [] dt in
      assert_bool "hider should be alive" (find 0 st).alive );
    ( "hiders can overlap each other" >:: fun _ ->
      let st = Game.step (Game.init [] [ mk 0. 0.; mk ~id:2 5. 5. ]) [] dt in
      assert_bool "both alive" ((find 0 st).alive && (find 2 st).alive) );
    ( "dead hider does not move" >:: fun _ ->
      let dead = { (mk 0. 0.) with alive = false } in
      let st = Game.step (Game.init [] [ dead ]) [ (0, right) ] dt in
      assert_pos ~x:0. ~y:0. (find 0 st) );
  ]

let suite =
  "hideseek" >::: [ "physics" >::: physics_tests; "game" >::: game_tests ]

let () = run_test_tt_main suite
