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

(* ---------- Game.step ---------- *)

let hunter = mk ~id:1 ~role:Entity.Hunter 50. 50.
let world ?(walls = []) ?(hunters = [ hunter ]) hider =
  World.init ~walls ~entities:(hider :: hunters)

(* the first hider; most tests have exactly one *)
let first_hider w = List.hd (World.hiders w)

(* a test-only strategy: every hunter always walks left *)
module Left : Strategy.S = struct
  let decide _ _ = left
end

let step ?(strat = (module Strategy.Stay : Strategy.S)) w dir =
  Game.step strat w dir dt

let string_of_outcome = function
  | Game.Victory -> "Victory"
  | Game.Defeat -> "Defeat"
  | Game.Mid_game -> "Mid_game"

let assert_outcome expected actual =
  assert_equal ~printer:string_of_outcome expected actual

let game_tests =
  [
    ( "no input means no movement" >:: fun _ ->
      let w, o = step (world (mk 0. 0.)) Vec2d.zero in
      assert_pos ~x:0. ~y:0. (first_hider w);
      assert_outcome Game.Mid_game o );
    ( "input moves the hider, Stay hunter stays" >:: fun _ ->
      let w, _ = step (world (mk 0. 0.)) right in
      assert_pos ~x:10. ~y:0. (first_hider w);
      assert_pos ~x:50. ~y:50. (List.hd (World.hunters w)) );
    ( "step uses the world's walls" >:: fun _ ->
      let w, _ = step (world ~walls:[ wall ] ~hunters:[] (mk 85. 50.)) right in
      assert_pos ~x:90. ~y:50. (first_hider w) );
    ( "hider overlapping hunter dies" >:: fun _ ->
      let w, o = step (world (mk 55. 55.)) Vec2d.zero in
      assert_bool "hider should be dead" (not (first_hider w).alive);
      assert_outcome Game.Defeat o );
    ( "hider moving into hunter dies" >:: fun _ ->
      let _, o = step (world (mk 35. 50.)) right in
      assert_outcome Game.Defeat o );
    ( "flush against hunter is not a touch" >:: fun _ ->
      let w, o = step (world (mk 40. 50.)) Vec2d.zero in
      assert_bool "hider should be alive" (first_hider w).alive;
      assert_outcome Game.Mid_game o );
    ( "touching any one of several hunters kills" >:: fun _ ->
      let far = mk ~id:2 ~role:Entity.Hunter 300. 300. in
      let _, o = step (world ~hunters:[ far; hunter ] (mk 55. 55.)) Vec2d.zero in
      assert_outcome Game.Defeat o );
    ( "dead hider does not move and stays defeated" >:: fun _ ->
      let dead = { (mk 0. 0.) with alive = false } in
      let w, o = step (world ~hunters:[] dead) right in
      assert_pos ~x:0. ~y:0. (first_hider w);
      assert_outcome Game.Defeat o );
    ( "hunters move using the strategy" >:: fun _ ->
      let w, _ = step ~strat:(module Left) (world (mk 0. 200.)) Vec2d.zero in
      assert_pos ~x:40. ~y:50. (List.hd (World.hunters w)) );
    ( "hunter walking into a standing hider kills" >:: fun _ ->
      let _, o = step ~strat:(module Left) (world (mk 35. 50.)) Vec2d.zero in
      assert_outcome Game.Defeat o );
    ( "input moves every hider" >:: fun _ ->
      let h2 = mk ~id:2 0. 100. in
      let w = World.init ~walls:[] ~entities:[ mk 0. 0.; h2 ] in
      let w, _ = step w right in
      match World.hiders w with
      | [ a; b ] -> assert_pos ~x:10. ~y:0. a; assert_pos ~x:10. ~y:100. b
      | _ -> assert_failure "expected two hiders" );
    ( "one of two hiders caught: game goes on" >:: fun _ ->
      let safe = mk ~id:2 300. 300. in
      let w = World.init ~walls:[] ~entities:[ mk 55. 55.; safe; hunter ] in
      let w, o = step w Vec2d.zero in
      let alive = List.map (fun (e : Entity.t) -> e.alive) (World.hiders w) in
      assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_bool l)) [ false; true ] alive;
      assert_outcome Game.Mid_game o );
    ( "all hiders caught is defeat" >:: fun _ ->
      let h2 = mk ~id:2 45. 45. in
      let w = World.init ~walls:[] ~entities:[ mk 55. 55.; h2; hunter ] in
      let _, o = step w Vec2d.zero in
      assert_outcome Game.Defeat o );
    ( "default world starts in play" >:: fun _ ->
      let _, o = step World.default Vec2d.zero in
      assert_outcome Game.Mid_game o );
  ]

(* ---------- World ---------- *)

let world_tests =
  let h0 = mk ~id:0 0. 0. and u1 = mk ~id:1 ~role:Entity.Hunter 0. 0. in
  let h2 = mk ~id:2 0. 0. in
  let w = World.init ~walls:[] ~entities:[ h0; u1; h2 ] in
  let ids es = List.map (fun (e : Entity.t) -> e.id) es in
  let ids_test name expected es =
    name >:: fun _ ->
    assert_equal ~printer:(fun l -> String.concat "," (List.map string_of_int l))
      expected (ids es)
  in
  [
    ids_test "entities keeps order" [ 0; 1; 2 ] (World.entities w);
    ids_test "hiders" [ 0; 2 ] (World.hiders w);
    ids_test "hunters" [ 1 ] (World.hunters w);
    ids_test "entities_by_role Hider" [ 0; 2 ] (World.entities_by_role w Entity.Hider);
    ids_test "no hunters" [] (World.hunters (World.init ~walls:[] ~entities:[ h0 ]));
  ]

(* ---------- Strategy ---------- *)

let strategy_tests =
  [
    ( "Stay never moves" >:: fun _ ->
      let w = World.default in
      let dir = Strategy.Stay.decide w (List.hd (World.hunters w)) in
      assert_equal ~cmp:feq ~printer:string_of_float ~msg:"x" 0. dir.x;
      assert_equal ~cmp:feq ~printer:string_of_float ~msg:"y" 0. dir.y );
  ]

(* ---------- User_input_strategy ---------- *)

(* [keys_test name held ~x ~y]: holding exactly the keys [held] gives (x, y) *)
let keys_test name held ~x ~y =
  name >:: fun _ ->
  let dir = User_input_strategy.direction_of_keys (fun k -> List.mem k held) in
  assert_equal ~cmp:feq ~printer:string_of_float ~msg:"x" x dir.x;
  assert_equal ~cmp:feq ~printer:string_of_float ~msg:"y" y dir.y

let user_input_strategy_tests =
  Raylib.Key.
    [
      keys_test "no keys" [] ~x:0. ~y:0.;
      keys_test "W is up" [ W ] ~x:0. ~y:(-1.);
      keys_test "A is left" [ A ] ~x:(-1.) ~y:0.;
      keys_test "S is down" [ S ] ~x:0. ~y:1.;
      keys_test "D is right" [ D ] ~x:1. ~y:0.;
      keys_test "W+D is up-right" [ W; D ] ~x:1. ~y:(-1.);
      keys_test "A+D cancel" [ A; D ] ~x:0. ~y:0.;
      keys_test "all four cancel" [ W; A; S; D ] ~x:0. ~y:0.;
      keys_test "other keys ignored" [ Up; Left ] ~x:0. ~y:0.;
    ]

let suite =
  "hideseek" >::: [ "physics" >::: physics_tests; "game" >::: game_tests;
    "world" >::: world_tests; "strategy" >::: strategy_tests;
    "user_input_strategy" >::: user_input_strategy_tests ]

let () = run_test_tt_main suite
