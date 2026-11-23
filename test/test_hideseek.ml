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

(* ---------- Vec2d ---------- *)

let assert_vec ~x ~y (p : Vec2d.t) =
  assert_equal ~cmp:feq ~printer:string_of_float ~msg:"x" x p.x;
  assert_equal ~cmp:feq ~printer:string_of_float ~msg:"y" y p.y

let vec2d_tests =
  [
    ("sub" >:: fun _ -> assert_vec ~x:(-2.) ~y:5. (Vec2d.sub (v 1. 7.) (v 3. 2.)));
    ("sub self is zero" >:: fun _ -> assert_vec ~x:0. ~y:0. (Vec2d.sub (v 4. 4.) (v 4. 4.)));
    ( "dist 3-4-5" >:: fun _ ->
      assert_equal ~cmp:feq ~printer:string_of_float 5. (Vec2d.dist (v 1. 1.) (v 4. 5.)) );
    ( "dist is symmetric" >:: fun _ ->
      assert_equal ~cmp:feq ~printer:string_of_float 5. (Vec2d.dist (v 4. 5.) (v 1. 1.)) );
    ( "dist to self is 0" >:: fun _ ->
      assert_equal ~cmp:feq ~printer:string_of_float 0. (Vec2d.dist (v 2. 3.) (v 2. 3.)) );
  ]

(* ---------- Rect ---------- *)

let rect_tests =
  [
    ( "center of a square" >:: fun _ ->
      assert_vec ~x:15. ~y:25. (Rect.center Rect.{ pos = v 10. 20.; w = 10.; h = 10. }) );
    ( "center of a non-square" >:: fun _ ->
      assert_vec ~x:20. ~y:3. (Rect.center Rect.{ pos = v 0. 0.; w = 40.; h = 6. }) );
  ]

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
(* long enough that no test runs out of time unless it means to *)
let long = 1000.

let world ?(walls = []) ?(hunters = [ hunter ]) hider =
  World.init ~walls ~entities:(hider :: hunters)

(* the first hider; most tests have exactly one *)
let first_hider w = List.hd (World.hiders w)

(* a test-only strategy: every hunter always walks left *)
module Left : Strategy.S = struct
  let decide _ _ = left
end

(* a test-only strategy that always heads in [dir] *)
let toward dir : (module Strategy.S) =
  (module struct
    let decide _ _ = dir
  end)

(* one step where every hider heads in [dir] and every hunter uses [strat] *)
let step ?(strat = (module Strategy.Stay : Strategy.S)) w dir =
  let strat_of (e : Entity.t) =
    match e.role with Entity.Hider -> toward dir | Entity.Hunter -> strat
  in
  let g = Game.init ~time_limit:long w (List.map strat_of (World.entities w)) in
  let g, o = Game.step g dt in
  (Game.world g, o)

let string_of_outcome = function
  | Game.Hunters_win -> "Hunters_win"
  | Game.Hiders_win -> "Hiders_win"
  | Game.Ongoing -> "Ongoing"

let assert_outcome expected actual =
  assert_equal ~printer:string_of_outcome expected actual

let game_tests =
  [
    ( "no input means no movement" >:: fun _ ->
      let w, o = step (world (mk 0. 0.)) Vec2d.zero in
      assert_pos ~x:0. ~y:0. (first_hider w);
      assert_outcome Game.Ongoing o );
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
      assert_outcome Game.Hunters_win o );
    ( "hider moving into hunter dies" >:: fun _ ->
      let _, o = step (world (mk 35. 50.)) right in
      assert_outcome Game.Hunters_win o );
    ( "flush against hunter is not a touch" >:: fun _ ->
      let w, o = step (world (mk 40. 50.)) Vec2d.zero in
      assert_bool "hider should be alive" (first_hider w).alive;
      assert_outcome Game.Ongoing o );
    ( "touching any one of several hunters kills" >:: fun _ ->
      let far = mk ~id:2 ~role:Entity.Hunter 300. 300. in
      let _, o = step (world ~hunters:[ far; hunter ] (mk 55. 55.)) Vec2d.zero in
      assert_outcome Game.Hunters_win o );
    ( "dead hider does not move and hunters have won" >:: fun _ ->
      let dead = { (mk 0. 0.) with alive = false } in
      let w, o = step (world ~hunters:[] dead) right in
      assert_pos ~x:0. ~y:0. (first_hider w);
      assert_outcome Game.Hunters_win o );
    ( "hunters move using the strategy" >:: fun _ ->
      let w, _ = step ~strat:(module Left) (world (mk 0. 200.)) Vec2d.zero in
      assert_pos ~x:40. ~y:50. (List.hd (World.hunters w)) );
    ( "hunter walking into a standing hider kills" >:: fun _ ->
      let _, o = step ~strat:(module Left) (world (mk 35. 50.)) Vec2d.zero in
      assert_outcome Game.Hunters_win o );
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
      assert_outcome Game.Ongoing o );
    ( "all hiders caught: hunters win" >:: fun _ ->
      let h2 = mk ~id:2 45. 45. in
      let w = World.init ~walls:[] ~entities:[ mk 55. 55.; h2; hunter ] in
      let _, o = step w Vec2d.zero in
      assert_outcome Game.Hunters_win o );
    ( "each entity uses its own strategy" >:: fun _ ->
      let h2 = mk ~id:2 ~role:Entity.Hunter 200. 200. in
      let w = World.init ~walls:[] ~entities:[ mk 0. 0.; hunter; h2 ] in
      let g =
        Game.init ~time_limit:long w [ toward down; (module Left); (module Strategy.Stay) ]
      in
      let g, _ = Game.step g dt in
      match World.entities (Game.world g) with
      | [ a; b; c ] ->
          assert_pos ~x:0. ~y:10. a;
          assert_pos ~x:40. ~y:50. b;
          assert_pos ~x:200. ~y:200. c
      | _ -> assert_failure "expected three entities" );
    ( "init rejects too few strategies" >:: fun _ ->
      assert_raises
        (Invalid_argument "Game.init: need exactly one strategy per entity")
        (fun () -> Game.init ~time_limit:long (world (mk 0. 0.)) [ toward right ]) );
    ( "init rejects too many strategies" >:: fun _ ->
      let s = (module Strategy.Stay : Strategy.S) in
      assert_raises
        (Invalid_argument "Game.init: need exactly one strategy per entity")
        (fun () -> Game.init ~time_limit:long (world ~hunters:[] (mk 0. 0.)) [ s; s ]) );
    ( "everyone decides from the same snapshot" >:: fun _ ->
      (* the hunter heads for where the first hider is; if it saw the hider
         after its move it would head diagonally instead of straight down *)
      let chase : (module Strategy.S) =
        (module struct
          let decide w (e : Entity.t) =
            let h = List.hd (World.hiders w) in
            Vec2d.add h.pos (Vec2d.scale (-1.) e.pos)
        end)
      in
      let u = mk ~id:1 ~role:Entity.Hunter 0. 0. in
      let w = World.init ~walls:[] ~entities:[ mk 0. 100.; u ] in
      let g, _ = Game.step (Game.init ~time_limit:long w [ toward right; chase ]) dt in
      assert_pos ~x:0. ~y:10. (List.hd (World.hunters (Game.world g))) );
    ( "dead hunter does not move" >:: fun _ ->
      let dead = { hunter with alive = false } in
      let w, _ = step ~strat:(module Left) (world ~hunters:[ dead ] (mk 0. 200.)) Vec2d.zero in
      assert_pos ~x:50. ~y:50. (List.hd (World.hunters w)) );
    ( "default world starts in play" >:: fun _ ->
      let _, o = step World.default Vec2d.zero in
      assert_outcome Game.Ongoing o );
  ]

(* ---------- Game timer ---------- *)

(* a game where every hider heads in [dir] and every hunter stays *)
let timed ~time_limit ?(dir = Vec2d.zero) w =
  let strat_of (e : Entity.t) =
    match e.role with
    | Entity.Hider -> toward dir
    | Entity.Hunter -> (module Strategy.Stay : Strategy.S)
  in
  Game.init ~time_limit w (List.map strat_of (World.entities w))

let assert_time expected g =
  assert_equal ~cmp:feq ~printer:string_of_float ~msg:"time_left" expected
    (Game.time_left g)

let timer_tests =
  [
    ( "time_left starts at the limit" >:: fun _ ->
      assert_time 5. (timed ~time_limit:5. (world (mk 0. 0.))) );
    ( "time_left counts down by dt" >:: fun _ ->
      let g, _ = Game.step (timed ~time_limit:5. (world (mk 0. 0.))) dt in
      assert_time (5. -. dt) g );
    ( "time_left never goes below 0" >:: fun _ ->
      let g, _ = Game.step (timed ~time_limit:(dt /. 2.) (world (mk 0. 0.))) dt in
      assert_time 0. g );
    ( "step that uses up the time still reports Ongoing" >:: fun _ ->
      let _, o = Game.step (timed ~time_limit:dt (world (mk 0. 0.))) dt in
      assert_outcome Game.Ongoing o );
    ( "hiders win on the step after time runs out" >:: fun _ ->
      let g, _ = Game.step (timed ~time_limit:dt (world (mk 0. 0.))) dt in
      let _, o = Game.step g dt in
      assert_outcome Game.Hiders_win o );
    ( "nobody moves once time is up" >:: fun _ ->
      let g = timed ~time_limit:dt ~dir:right (world ~hunters:[] (mk 0. 0.)) in
      let g, _ = Game.step g dt in
      let g, _ = Game.step g dt in
      assert_pos ~x:10. ~y:0. (first_hider (Game.world g)) );
    ( "catch on the step that uses up the time: hunters win" >:: fun _ ->
      let _, o = Game.step (timed ~time_limit:dt (world (mk 55. 55.))) dt in
      assert_outcome Game.Hunters_win o );
    ( "hunters who already won stay winners after time is up" >:: fun _ ->
      let g, _ = Game.step (timed ~time_limit:dt (world (mk 55. 55.))) dt in
      let _, o = Game.step g dt in
      assert_outcome Game.Hunters_win o );
    ( "init rejects a zero time limit" >:: fun _ ->
      assert_raises (Invalid_argument "Game.init: time_limit must be positive")
        (fun () -> timed ~time_limit:0. (world (mk 0. 0.))) );
    ( "init rejects a negative time limit" >:: fun _ ->
      assert_raises (Invalid_argument "Game.init: time_limit must be positive")
        (fun () -> timed ~time_limit:(-1.) (world (mk 0. 0.))) );
  ]

(* ---------- Display ---------- *)

let display_tests =
  let g = timed ~time_limit:5. (World.init ~walls:[] ~entities:[ mk 1. 2. ]) in
  [
    ( "render shows entities, time left, then status" >:: fun _ ->
      assert_equal ~printer:Fun.id "Hider 0: (1.0, 2.0)\nTime left: 5.0\nStatus: Ongoing"
        (Display.render g Game.Ongoing) );
    ( "time left is shown with one decimal" >:: fun _ ->
      let g, _ = Game.step g 1.3 in
      assert_equal ~printer:Fun.id "Hider 0: (1.0, 2.0)\nTime left: 3.7\nStatus: Ongoing"
        (Display.render g Game.Ongoing) );
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
    ( "default hunter is slower than the hider" >:: fun _ ->
      let w = World.default in
      let hider = List.hd (World.hiders w) and hunter = List.hd (World.hunters w) in
      assert_bool "hunter speed < hider speed" (hunter.speed < hider.speed) );
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

(* ---------- Spawn ---------- *)

let positions w = List.map (fun (e : Entity.t) -> (e.pos.x, e.pos.y)) (World.entities w)
let spawn seed = Spawn.randomize (Random.State.make [| seed |]) World.default
let seeds = List.init 200 Fun.id

(* [for_all_spawns f] checks [f] on the default world spawned from many seeds *)
let for_all_spawns f = List.iter (fun seed -> f seed (spawn seed)) seeds

let center (e : Entity.t) = Rect.center (Entity.hitbox e)

let spawn_tests =
  [
    ( "same seed gives the same positions" >:: fun _ ->
      assert_equal (positions (spawn 7)) (positions (spawn 7)) );
    ( "different seeds give different positions" >:: fun _ ->
      assert_bool "positions differ" (positions (spawn 1) <> positions (spawn 2)) );
    ( "keeps every field but pos, in order" >:: fun _ ->
      let strip (e : Entity.t) = { e with pos = Vec2d.zero } in
      assert_equal
        (List.map strip (World.entities World.default))
        (List.map strip (World.entities (spawn 3))) );
    ( "keeps the walls" >:: fun _ ->
      assert_equal (World.walls World.default) (World.walls (spawn 3)) );
    ( "nobody spawns in a wall" >:: fun _ ->
      for_all_spawns (fun seed w ->
          List.iter
            (fun e ->
              List.iter
                (fun wall ->
                  assert_bool (Printf.sprintf "seed %d" seed)
                    (not (Rect.overlaps wall (Entity.hitbox e))))
                (World.walls w))
            (World.entities w)) );
    ( "everyone spawns inside the arena" >:: fun _ ->
      for_all_spawns (fun seed w ->
          List.iter
            (fun (e : Entity.t) ->
              assert_bool (Printf.sprintf "seed %d" seed)
                (e.pos.x >= 0. && e.pos.y >= 0.
                && e.pos.x +. e.size <= 400. && e.pos.y +. e.size <= 400.))
            (World.entities w)) );
    ( "players spawn at least min_gap apart" >:: fun _ ->
      for_all_spawns (fun seed w ->
          match World.entities w with
          | [ a; b ] ->
              assert_bool (Printf.sprintf "seed %d" seed)
                (Vec2d.dist (center a) (center b) >= 100.)
          | _ -> assert_failure "expected two entities") );
    ( "custom min_gap is respected" >:: fun _ ->
      List.iter
        (fun seed ->
          let w = Spawn.randomize ~min_gap:250. (Random.State.make [| seed |]) World.default in
          match World.entities w with
          | [ a; b ] -> assert_bool "gap" (Vec2d.dist (center a) (center b) >= 250.)
          | _ -> assert_failure "expected two entities")
        seeds );
    ( "impossible gap raises Failure" >:: fun _ ->
      match Spawn.randomize ~min_gap:10_000. (Random.State.make [| 0 |]) World.default with
      | _ -> assert_failure "expected Failure"
      | exception Failure _ -> () );
  ]

(* ---------- Trainer ---------- *)

let tally_of = List.fold_left Trainer.record Trainer.empty
let assert_rate expected got =
  assert_equal ~cmp:feq ~printer:string_of_float expected got

let trainer_tests =
  let greedy = (module Greedy_strategy : Strategy.S)
  and stay = (module Strategy.Stay : Strategy.S) in
  let hunter = mk ~id:1 ~role:Entity.Hunter ~speed:100. 200. 0. in
  [
    ( "dt is 20 steps per second" >:: fun _ -> assert_rate 0.05 Trainer.dt );
    ( "hunter catches a hider that stays" >:: fun _ ->
      let w = World.init ~walls:[] ~entities:[ mk 0. 0.; hunter ] in
      let o, secs = Trainer.play (Game.init ~time_limit:20. w [ stay; greedy ]) in
      assert_outcome Game.Hunters_win o;
      (* 200 px at 100 px/s, minus the 10 px the boxes need to touch *)
      assert_bool "about 1.9 s" (secs > 1.8 && secs < 2.0) );
    ( "hider walled off survives the whole time" >:: fun _ ->
      let wall = Rect.{ pos = Vec2d.{ x = 100.; y = -1000. }; w = 10.; h = 2000. } in
      let w = World.init ~walls:[ wall ] ~entities:[ mk 0. 0.; hunter ] in
      let o, secs = Trainer.play (Game.init ~time_limit:3. w [ stay; greedy ]) in
      assert_outcome Game.Hiders_win o;
      assert_rate 3. secs );
    ( "empty tally has no rounds" >:: fun _ ->
      assert_equal 0 (Trainer.rounds Trainer.empty);
      assert_rate 0. (Trainer.hider_rate Trainer.empty);
      assert_rate 0. (Trainer.hunter_rate Trainer.empty) );
    ( "tally counts each side" >:: fun _ ->
      let t = tally_of Game.[ Hiders_win; Hunters_win; Hiders_win; Hiders_win ] in
      assert_equal ~printer:string_of_int 4 (Trainer.rounds t);
      assert_equal ~printer:string_of_int 3 (Trainer.hider_wins t);
      assert_equal ~printer:string_of_int 1 (Trainer.hunter_wins t);
      assert_rate 0.75 (Trainer.hider_rate t);
      assert_rate 0.25 (Trainer.hunter_rate t) );
    ( "record rejects Ongoing" >:: fun _ ->
      assert_raises (Invalid_argument "Trainer.record: game is still ongoing")
        (fun () -> Trainer.record Trainer.empty Game.Ongoing) );
  ]

(* ---------- Greedy_strategy ---------- *)

(* directions may have any length, so compare them normalized *)
let assert_dir ~x ~y d =
  let want = Vec2d.normalize (v x y) in
  assert_vec ~x:want.x ~y:want.y (Vec2d.normalize d)

(* [greedy_test name entities ~x ~y]: the first entity in [entities] decides
   on a direction parallel to (x, y) *)
let greedy_test name entities ~x ~y =
  name >:: fun _ ->
  let w = World.init ~walls:[] ~entities in
  assert_dir ~x ~y (Greedy_strategy.decide w (List.hd entities))

let hunter_at ?(id = 1) ?size x y = mk ~id ~role:Entity.Hunter ?size x y
let hider_at ?(id = 0) ?size x y = mk ~id ?size x y
let dead (e : Entity.t) = { e with alive = false }

let greedy_strategy_tests =
  [
    greedy_test "hunter heads for the hider" [ hunter_at 0. 0.; hider_at 30. 40. ] ~x:3. ~y:4.;
    greedy_test "hunter picks the closest hider"
      [ hunter_at 0. 0.; hider_at ~id:0 100. 0.; hider_at ~id:2 0. (-50.) ] ~x:0. ~y:(-1.);
    greedy_test "hunter ignores dead hiders"
      [ hunter_at 0. 0.; dead (hider_at ~id:0 10. 0.); hider_at ~id:2 0. 100. ] ~x:0. ~y:1.;
    greedy_test "hunter ignores other hunters"
      [ hunter_at 0. 0.; hunter_at ~id:2 10. 0.; hider_at 0. 100. ] ~x:0. ~y:1.;
    greedy_test "hider runs straight away" [ hider_at 0. 0.; hunter_at 30. 40. ] ~x:(-3.) ~y:(-4.);
    greedy_test "hider flees the closest hunter"
      [ hider_at 0. 0.; hunter_at ~id:1 100. 0.; hunter_at ~id:2 0. 50. ] ~x:0. ~y:(-1.);
    greedy_test "hider ignores dead hunters"
      [ hider_at 0. 0.; dead (hunter_at ~id:1 10. 0.); hunter_at ~id:2 0. 100. ] ~x:0. ~y:(-1.);
    greedy_test "hider ignores other hiders"
      [ hider_at 0. 0.; hider_at ~id:2 10. 0.; hunter_at 0. 100. ] ~x:0. ~y:(-1.);
    greedy_test "tie goes to the first in order"
      [ hunter_at 0. 0.; hider_at ~id:0 50. 0.; hider_at ~id:2 0. 50. ] ~x:1. ~y:0.;
    (* hunter center (5, 5), hider center (15, 35); top-left corners would give (0, 20) *)
    greedy_test "measures between centers"
      [ hunter_at ~size:10. 0. 0.; hider_at ~size:30. 0. 20. ] ~x:10. ~y:30.;
    greedy_test "hunter with no living hider stays"
      [ hunter_at 0. 0.; dead (hider_at 10. 0.) ] ~x:0. ~y:0.;
    greedy_test "hider with no hunters stays" [ hider_at 0. 0. ] ~x:0. ~y:0.;
    greedy_test "same center as target stays" [ hunter_at 5. 5.; hider_at 5. 5. ] ~x:0. ~y:0.;
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
      keys_test "Up is up" [ Up ] ~x:0. ~y:(-1.);
      keys_test "Left is left" [ Left ] ~x:(-1.) ~y:0.;
      keys_test "Down is down" [ Down ] ~x:0. ~y:1.;
      keys_test "Right is right" [ Right ] ~x:1. ~y:0.;
      keys_test "arrow and letter mix" [ W; Right ] ~x:1. ~y:(-1.);
      keys_test "other keys ignored" [ Q; Space ] ~x:0. ~y:0.;
    ]

(* ---------- Mlp ---------- *)

let arr2 rows cols xs = Mlp.N.of_array xs [| rows; cols |]

(* 2 -> 2 -> 1 with identity first layer and summing output layer, bias 0.5 *)
let small_mlp () =
  Mlp.with_params
    (Mlp.init (Random.State.make [| 0 |]) [ 2; 2; 1 ])
    [
      arr2 2 2 [| 1.; 0.; 0.; 1. |];
      arr2 1 2 [| 0.; 0. |];
      arr2 2 1 [| 1.; 1. |];
      arr2 1 1 [| 0.5 |];
    ]

(* writes [n] to a temp file and reads it back *)
let round_trip n =
  let file = Filename.temp_file "mlp" ".txt" in
  let oc = open_out file in
  Mlp.write oc n;
  close_out oc;
  let ic = Scanf.Scanning.open_in file in
  let n' = Mlp.read ic in
  Scanf.Scanning.close_in ic;
  Sys.remove file;
  n'

let float_array_printer a =
  "[" ^ String.concat "; " (Array.to_list (Array.map string_of_float a)) ^ "]"

let mlp_tests =
  [
    ( "init keeps sizes" >:: fun _ ->
      assert_equal [ 4; 5; 5; 2 ]
        (Mlp.sizes (Mlp.init (Random.State.make [| 1 |]) [ 4; 5; 5; 2 ])) );
    ( "forward output length" >:: fun _ ->
      let n = Mlp.init (Random.State.make [| 1 |]) [ 4; 5; 5; 2 ] in
      assert_equal 2 (Array.length (Mlp.forward n [| 0.1; 0.2; 0.3; 0.4 |])) );
    ( "forward: tanh hidden, linear output" >:: fun _ ->
      assert_equal ~cmp:feq ~printer:string_of_float
        (tanh 1. +. tanh (-2.) +. 0.5)
        (Mlp.forward (small_mlp ()) [| 1.; -2. |]).(0) );
    ( "forward wrong input length" >:: fun _ ->
      assert_raises (Invalid_argument "Mlp.forward: input has the wrong length")
        (fun () -> Mlp.forward (small_mlp ()) [| 1. |]) );
    ( "init rejects one layer" >:: fun _ ->
      assert_raises
        (Invalid_argument "Mlp.init: need at least two positive layer widths")
        (fun () -> Mlp.init (Random.State.make [| 1 |]) [ 4 ]) );
    ( "with_params rejects a bad shape" >:: fun _ ->
      assert_raises (Invalid_argument "Mlp.with_params: shape mismatch")
        (fun () ->
          Mlp.with_params (small_mlp ())
            [ arr2 1 2 [| 1.; 1. |]; arr2 1 2 [| 0.; 0. |]; arr2 2 1 [| 1.; 1. |]; arr2 1 1 [| 0. |] ]) );
    ( "same seed, same network" >:: fun _ ->
      let mk () = Mlp.init (Random.State.make [| 7 |]) [ 4; 5; 1 ] in
      assert_equal ~printer:float_array_printer
        (Mlp.forward (mk ()) [| 1.; 2.; 3.; 4. |])
        (Mlp.forward (mk ()) [| 1.; 2.; 3.; 4. |]) );
    ( "write then read is exact" >:: fun _ ->
      let n = Mlp.init (Random.State.make [| 3 |]) [ 4; 5; 5; 2 ] in
      let n' = round_trip n in
      assert_equal [ 4; 5; 5; 2 ] (Mlp.sizes n');
      assert_equal ~printer:float_array_printer
        (Mlp.forward n [| 0.3; -0.1; 0.9; 0.5 |])
        (Mlp.forward n' [| 0.3; -0.1; 0.9; 0.5 |]) );
  ]

(* ---------- Ppo ---------- *)

let half_log_2pi = 0.5 *. log (2. *. Float.pi)

let ppo_world =
  World.init ~walls:[]
    ~entities:
      [
        mk ~id:0 ~role:Entity.Hider 30. 70.;
        mk ~id:1 ~role:Entity.Hunter 190. 390.;
      ]

let float_arrays_eq a b =
  Array.length a = Array.length b && Array.for_all2 feq a b

let assert_floats expected actual =
  assert_equal ~cmp:float_arrays_eq ~printer:float_array_printer expected actual

(* the action [ag] samples (seed 0) at a fixed obs, with the given [adv] and
   [ret] *)
let one_sample ag ~adv ~ret =
  let obs = [| 0.1; 0.2; 0.8; 0.9 |] in
  let act, logp = Ppo.act (Random.State.make [| 0 |]) ag obs in
  { Ppo.obs; act; logp; adv; ret }

(* [n] updates of [l] on copies of [s] *)
let rec train_on n l s =
  if n = 0 then l
  else train_on (n - 1) (Ppo.update (Random.State.make [| n |]) l (Array.make 8 s)) s

let ppo_tests =
  [
    ( "log_prob at the mean, std 1" >:: fun _ ->
      assert_equal ~cmp:feq ~printer:string_of_float (-2. *. half_log_2pi)
        (Ppo.log_prob ~mean:[| 1.; 2. |] ~log_std:[| 0.; 0. |] [| 1.; 2. |]) );
    ( "log_prob one std away, std e" >:: fun _ ->
      assert_equal ~cmp:feq ~printer:string_of_float
        (-0.5 -. 1. -. half_log_2pi)
        (Ppo.log_prob ~mean:[| 0. |] ~log_std:[| 1. |] [| exp 1. |]) );
    ( "gae with lambda 1 is the discounted return" >:: fun _ ->
      let adv, ret =
        Ppo.gae ~gamma:0.5 ~lambda:1. ~rewards:[| 1.; 2.; 4. |]
          ~values:[| 1.; 1.; 1. |]
      in
      (* returns: 4, then 2 + 0.5*4 = 4, then 1 + 0.5*4 = 3 *)
      assert_floats [| 3.; 4.; 4. |] ret;
      assert_floats [| 2.; 3.; 3. |] adv );
    ( "gae with lambda 0 is one-step TD" >:: fun _ ->
      let adv, ret =
        Ppo.gae ~gamma:0.5 ~lambda:0. ~rewards:[| 1.; 2. |] ~values:[| 2.; 4. |]
      in
      (* delta_0 = 1 + 0.5*4 - 2, delta_1 = 2 + 0 - 4 *)
      assert_floats [| 1.; -2. |] adv;
      assert_floats [| 3.; 2. |] ret );
    ( "gae length mismatch" >:: fun _ ->
      assert_raises (Invalid_argument "Ppo.gae: length mismatch") (fun () ->
          Ppo.gae ~gamma:0.9 ~lambda:0.9 ~rewards:[| 1. |] ~values:[||]) );
    ( "observe: own center then opponent center, scaled" >:: fun _ ->
      let hider = List.hd (World.entities ppo_world) in
      assert_floats [| 35. /. 400.; 75. /. 400.; 195. /. 400.; 395. /. 400. |]
        (Ppo.observe ppo_world hider) );
    ( "observe: no living opponent uses own center" >:: fun _ ->
      let hider = List.hd (World.entities ppo_world) in
      let w =
        World.with_entities ppo_world
          (List.map
             (fun (e : Entity.t) -> if e.role = Hunter then { e with alive = false } else e)
             (World.entities ppo_world))
      in
      assert_floats [| 35. /. 400.; 75. /. 400.; 35. /. 400.; 75. /. 400. |]
        (Ppo.observe w hider) );
    ( "act is reproducible from the seed" >:: fun _ ->
      let ag = Ppo.init (Random.State.make [| 4 |]) in
      let o = [| 0.1; 0.2; 0.3; 0.4 |] in
      let a1, l1 = Ppo.act (Random.State.make [| 9 |]) ag o in
      let a2, l2 = Ppo.act (Random.State.make [| 9 |]) ag o in
      assert_floats a1 a2;
      assert_equal ~cmp:feq l1 l2 );
    ( "update moves value toward the return" >:: fun _ ->
      let ag = Ppo.init (Random.State.make [| 5 |]) in
      let s = one_sample ag ~adv:1. ~ret:3. in
      let before = Float.abs (Ppo.value ag s.obs -. 3.) in
      let ag' = Ppo.agent (train_on 30 (Ppo.learner ag) s) in
      let after = Float.abs (Ppo.value ag' s.obs -. 3.) in
      assert_bool (Printf.sprintf "%f -> %f" before after) (after < before) );
    ( "update makes a good action more likely" >:: fun _ ->
      let ag = Ppo.init (Random.State.make [| 5 |]) in
      let s = one_sample ag ~adv:1. ~ret:0. in
      (* a lone sample normalizes to advantage 0, so mix in a bad action *)
      let bad = { s with act = Array.map (fun x -> -.x) s.act; adv = -1. } in
      let bad = { bad with logp = Ppo.action_log_prob ag bad.obs bad.act } in
      let l =
        List.fold_left
          (fun l i -> Ppo.update (Random.State.make [| i |]) l [| s; bad; s; bad |])
          (Ppo.learner ag) (List.init 30 Fun.id)
      in
      let before = Ppo.action_log_prob ag s.obs s.act in
      let after = Ppo.action_log_prob (Ppo.agent l) s.obs s.act in
      assert_bool (Printf.sprintf "%f -> %f" before after) (after > before) );
    ( "action_log_prob matches act" >:: fun _ ->
      let ag = Ppo.init (Random.State.make [| 8 |]) in
      let o = [| 0.4; 0.4; 0.6; 0.1 |] in
      let a, l = Ppo.act (Random.State.make [| 3 |]) ag o in
      assert_equal ~cmp:feq ~printer:string_of_float l (Ppo.action_log_prob ag o a) );
    ( "update with an empty batch changes nothing" >:: fun _ ->
      let ag = Ppo.init (Random.State.make [| 5 |]) in
      let ag' = Ppo.agent (Ppo.update (Random.State.make [| 1 |]) (Ppo.learner ag) [||]) in
      let o = [| 0.5; 0.5; 0.1; 0.1 |] in
      assert_equal ~cmp:feq (Ppo.value ag o) (Ppo.value ag' o) );
    ( "write then read is exact" >:: fun _ ->
      let ag = Ppo.init (Random.State.make [| 6 |]) in
      let file = Filename.temp_file "ppo" ".txt" in
      let oc = open_out file in
      Ppo.write oc ag;
      close_out oc;
      let ic = Scanf.Scanning.open_in file in
      let ag' = Ppo.read ic in
      Scanf.Scanning.close_in ic;
      Sys.remove file;
      let o = [| 0.3; 0.6; 0.2; 0.9 |] in
      assert_equal ~cmp:feq (Ppo.value ag o) (Ppo.value ag' o);
      let a, l = Ppo.act (Random.State.make [| 2 |]) ag o in
      let a', l' = Ppo.act (Random.State.make [| 2 |]) ag' o in
      assert_floats a a';
      assert_equal ~cmp:feq l l' );
  ]

(* ---------- Trainer PPO rounds ---------- *)

let ppo_round ?(time_limit = 2.) ~hider ~hunter () =
  let ag seed = Some (Ppo.init (Random.State.make [| seed |])) in
  Trainer.play_ppo (Random.State.make [| 0 |]) ~time_limit
    ~hider:(if hider then ag 1 else None)
    ~hunter:(if hunter then ag 2 else None)
    World.default

let check ~h ~u ~out ~input () =
  Trainer.check_files ~hider_ppo:h ~hunter_ppo:u ~out ~input

let trainer_ppo_tests =
  [
    ( "step_reward is +-dist/arena*dt" >:: fun _ ->
      (* centers (35, 75) and (195, 395): dist = sqrt (160^2 + 320^2) *)
      let r = sqrt ((160. ** 2.) +. (320. ** 2.)) /. 400. *. Trainer.dt in
      assert_equal ~cmp:feq ~printer:string_of_float r
        (Trainer.step_reward ppo_world Entity.Hider);
      assert_equal ~cmp:feq ~printer:string_of_float (-.r)
        (Trainer.step_reward ppo_world Entity.Hunter) );
    ( "end_reward" >:: fun _ ->
      assert_equal 1. (Trainer.end_reward Game.Hiders_win Entity.Hider);
      assert_equal (-1.) (Trainer.end_reward Game.Hiders_win Entity.Hunter);
      assert_equal 1. (Trainer.end_reward Game.Hunters_win Entity.Hunter);
      assert_equal (-1.) (Trainer.end_reward Game.Hunters_win Entity.Hider);
      assert_equal 0. (Trainer.end_reward Game.Ongoing Entity.Hider) );
    ( "play_ppo both ppo: one sample per step each" >:: fun _ ->
      let o, secs, ex = ppo_round ~hider:true ~hunter:true () in
      assert_bool "finished" (o <> Game.Ongoing);
      let steps = int_of_float (Float.round (secs /. Trainer.dt)) in
      assert_equal ~printer:string_of_int steps (Array.length ex.hunter);
      assert_equal ~printer:string_of_int steps (Array.length ex.hider) );
    ( "play_ppo greedy hunter has no samples" >:: fun _ ->
      let _, _, ex = ppo_round ~hider:true ~hunter:false () in
      assert_equal 0 (Array.length ex.hunter);
      assert_bool "hider has samples" (Array.length ex.hider > 0) );
    ( "play_ppo greedy hider has no samples" >:: fun _ ->
      let _, _, ex = ppo_round ~hider:false ~hunter:true () in
      assert_equal 0 (Array.length ex.hider);
      assert_bool "hunter has samples" (Array.length ex.hunter > 0) );
    ( "play_ppo is reproducible" >:: fun _ ->
      let o1, s1, e1 = ppo_round ~hider:true ~hunter:true () in
      let o2, s2, e2 = ppo_round ~hider:true ~hunter:true () in
      assert_equal o1 o2;
      assert_equal ~cmp:feq s1 s2;
      assert_floats
        (Array.map (fun (s : Ppo.sample) -> s.ret) e1.hider)
        (Array.map (fun (s : Ppo.sample) -> s.ret) e2.hider) );
    ( "play_ppo calls on_step once per step" >:: fun _ ->
      let calls = ref 0 and last = ref Game.Ongoing in
      let o, _, _ =
        Trainer.play_ppo
          ~on_step:(fun _ o -> incr calls; last := o)
          (Random.State.make [| 0 |]) ~time_limit:1. ~hider:None ~hunter:None
          World.default
      in
      assert_bool "called" (!calls > 0);
      assert_equal o !last );
    ( "play_ppo rejects two hiders" >:: fun _ ->
      let w =
        World.init ~walls:[]
          ~entities:[ mk ~role:Entity.Hider 0. 0.; mk ~id:1 ~role:Entity.Hider 50. 50. ]
      in
      assert_raises
        (Invalid_argument "Trainer.play_ppo: need exactly one hider and one hunter")
        (fun () ->
          Trainer.play_ppo (Random.State.make [| 0 |]) ~time_limit:1. ~hider:None
            ~hunter:None w) );
    ( "check_files: neither ppo with --out" >:: fun _ ->
      assert_raises
        (Invalid_argument "--out and --in need --hider=ppo or --hunter=ppo")
        (check ~h:false ~u:false ~out:(Some "f") ~input:None) );
    ( "check_files: neither ppo with --in" >:: fun _ ->
      assert_raises
        (Invalid_argument "--out and --in need --hider=ppo or --hunter=ppo")
        (check ~h:false ~u:false ~out:None ~input:(Some "f")) );
    ( "check_files: neither ppo, no files is fine" >:: fun _ ->
      check ~h:false ~u:false ~out:None ~input:None () );
    ( "check_files: one ppo side with files is fine" >:: fun _ ->
      check ~h:false ~u:true ~out:(Some "f") ~input:(Some "g") () );
  ]

(* ---------- Weights ---------- *)

let with_temp f =
  let file = Filename.temp_file "weights" ".txt" in
  Fun.protect ~finally:(fun () -> Sys.remove file) (fun () -> f file)

let same_agent a b =
  let o = [| 0.2; 0.7; 0.5; 0.1 |] in
  feq (Ppo.value a o) (Ppo.value b o)
  && feq (Ppo.action_log_prob a o [| 0.3; -0.4 |]) (Ppo.action_log_prob b o [| 0.3; -0.4 |])

let weights_tests =
  [
    ( "save then load, both sides" >:: fun _ ->
      with_temp (fun file ->
          let h = Ppo.init (Random.State.make [| 1 |])
          and u = Ppo.init (Random.State.make [| 2 |]) in
          Weights.save file { seed = 42; hider = Some h; hunter = Some u };
          let w = Weights.load file in
          assert_equal ~printer:string_of_int 42 w.seed;
          assert_bool "hider" (same_agent h (Option.get w.hider));
          assert_bool "hunter" (same_agent u (Option.get w.hunter))) );
    ( "save then load, hunter only" >:: fun _ ->
      with_temp (fun file ->
          let u = Ppo.init (Random.State.make [| 2 |]) in
          Weights.save file { seed = 7; hider = None; hunter = Some u };
          let w = Weights.load file in
          assert_equal 7 w.seed;
          assert_bool "no hider" (w.hider = None);
          assert_bool "hunter" (same_agent u (Option.get w.hunter))) );
    ( "load rejects junk" >:: fun _ ->
      with_temp (fun file ->
          let oc = open_out file in
          output_string oc "hello\n";
          close_out oc;
          match Weights.load file with
          | _ -> assert_failure "loaded junk"
          | exception Failure _ -> ()) );
    ( "load rejects a cut-off file" >:: fun _ ->
      with_temp (fun file ->
          let oc = open_out file in
          output_string oc "seed 3\nhider\nmlp 4 4 5 5 2\n 0x1p+0";
          close_out oc;
          match Weights.load file with
          | _ -> assert_failure "loaded a cut-off file"
          | exception Failure _ -> ()) );
  ]

let suite =
  "hideseek" >::: [ "vec2d" >::: vec2d_tests; "rect" >::: rect_tests; "physics" >::: physics_tests; "game" >::: game_tests;
    "timer" >::: timer_tests; "display" >::: display_tests;
    "world" >::: world_tests; "strategy" >::: strategy_tests;
    "spawn" >::: spawn_tests; "trainer" >::: trainer_tests;
    "greedy_strategy" >::: greedy_strategy_tests;
    "user_input_strategy" >::: user_input_strategy_tests;
    "mlp" >::: mlp_tests; "ppo" >::: ppo_tests;
    "trainer_ppo" >::: trainer_ppo_tests;
    "weights" >::: weights_tests ]

let () = run_test_tt_main suite
