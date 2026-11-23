(* Training runner: plays rounds on the default map in a window at --speed
   times real time (> and < in the window change it), each from random
   start positions, trains the PPO sides every --batch rounds and prints
   the running win rate. Closing the window
   quits right away without saving. *)
open Hideseek

let rounds = ref 100
let time_limit = ref 100.
let seed = ref None
let batch = ref 10
let hider_ppo = ref true
let hunter_ppo = ref true
let speed = ref 1.
let out = ref None
let input = ref None

let positive name set x =
  if x <= 0 then raise (Arg.Bad (name ^ " must be greater than 0"));
  set x

(* --hider and --hunter: [set true] for ppo, [set false] for greedy *)
let side set = Arg.Symbol ([ "ppo"; "greedy" ], fun s -> set (s = "ppo"))

let speclist =
  [
    ( "--rounds",
      Arg.Int (positive "--rounds" (fun n -> rounds := n)),
      " how many rounds to play, e.g. --rounds=100 (the default)" );
    ( "--time",
      Arg.Float
        (fun t ->
          if t <= 0. then raise (Arg.Bad "--time must be greater than 0");
          time_limit := t),
      " seconds before a round is cut off, e.g. --time=100 (the default); \
       keep it well above Ppo.halflife so time left barely matters" );
    ( "--seed",
      Arg.Int (fun s -> seed := Some s),
      " seed for start positions and PPO, e.g. --seed=1 (default: random)" );
    ( "--hider",
      side (fun b -> hider_ppo := b),
      " who plays the hider, e.g. --hider=ppo (the default) or --hider=greedy" );
    ( "--hunter",
      side (fun b -> hunter_ppo := b),
      " who plays the hunter, e.g. --hunter=ppo (the default) or --hunter=greedy" );
    ( "--speed",
      Arg.Float
        (fun x ->
          (* 0 and inf both mean no waiting *)
          if Float.is_nan x || x < 0. then
            raise (Arg.Bad "--speed must be 0 (as fast as possible) or more");
          speed := if x = 0. then infinity else x),
      " game seconds per real second in the window, e.g. --speed=1 (the \
       default), --speed=5, or --speed=0 / --speed=inf for as fast as \
       possible" );
    ( "--batch",
      Arg.Int (positive "--batch" (fun n -> batch := n)),
      " rounds between PPO updates, e.g. --batch=10 (the default)" );
    ( "--out",
      Arg.String (fun f -> out := Some f),
      " file to save the trained PPO sides and seed to, e.g. --out=nn.txt \
       (default: save nothing)" );
    ( "--in",
      Arg.String (fun f -> input := Some f),
      " file saved by --out to start the PPO sides from, e.g. --in=nn.txt \
       (default: fresh networks)" );
  ]

let usage =
  "train [--rounds=N] [--time=SECONDS] [--seed=N] [--hider=ppo|greedy] \
   [--hunter=ppo|greedy] [--speed=X] [--batch=N] [--out=FILE] [--in=FILE]"

let outcome_name = function
  | Game.Hiders_win -> "hiders win "
  | Game.Hunters_win -> "hunters win"
  | Game.Ongoing -> "ongoing    "

let pct r = 100. *. r

(* how many game seconds passed per wall-clock second; infinite if the clock
   did not move *)
let speedup ~game ~wall = if wall > 0. then game /. wall else infinity

(* the longest the window goes without a new frame: 60 frames a second *)
let frame = 1. /. 60.

(* [show sprites t0 time_limit] is the [on_step] for a round that started at
   wall-clock time [t0]: it waits until the game is no more than --speed
   times ahead of the wall clock, then draws a frame if one is due. After
   each frame, > and < step --speed through [Trainer.speeds]. Quits the
   program if the window was closed. *)
let show sprites t0 time_limit =
  let last_draw = ref neg_infinity in
  (* the wall-clock time and game seconds played when the pacing restarted:
     at the start of the round and whenever the speed changes *)
  let anchor = ref (t0, 0.) in
  fun g o ->
    if Raylib.window_should_close () then begin
      Display.unload_sprites sprites;
      Raylib.close_window ();
      exit 0
    end;
    let played = time_limit -. Game.time_left g in
    let wall0, played0 = !anchor in
    let ahead = wall0 +. ((played -. played0) /. !speed) -. Unix.gettimeofday () in
    if ahead > 0. then Unix.sleepf ahead;
    let now = Unix.gettimeofday () in
    if now -. !last_draw >= frame || o <> Game.Ongoing then begin
      Display.draw sprites g o;
      last_draw := now;
      (* Raylib reads the keyboard while drawing, so a key press is seen
         once per frame; > and < are shift+period and shift+comma *)
      let s =
        if Raylib.is_key_pressed Raylib.Key.Period then Trainer.faster !speed
        else if Raylib.is_key_pressed Raylib.Key.Comma then Trainer.slower !speed
        else !speed
      in
      if s <> !speed then begin
        speed := s;
        anchor := (Unix.gettimeofday (), played);
        Printf.printf "speed x%g\n%!" s
      end
    end

let fail msg =
  prerr_endline ("train: " ^ msg);
  prerr_endline usage;
  exit 2

(* the agent a side starts from: [None] if it is greedy, otherwise the one in
   [loaded] or a fresh one *)
let start_agent rng name ~ppo ~loaded =
  match (ppo, loaded) with
  | false, None -> None
  | false, Some _ ->
      Printf.printf "note: %s is greedy, ignoring its network in --in\n" name;
      None
  | true, Some ag -> Some ag
  | true, None ->
      if !input <> None then
        Printf.printf "note: --in has no %s network, starting it fresh\n" name;
      Some (Ppo.init rng)

let () =
  Arg.parse (Arg.align speclist)
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    usage;
  (try
     Trainer.check_files ~hider_ppo:!hider_ppo ~hunter_ppo:!hunter_ppo ~out:!out
       ~input:!input
   with Invalid_argument m -> fail m);
  let loaded =
    match !input with
    | None -> None
    | Some f -> (
        try Some (Weights.load f) with Sys_error m | Failure m -> fail m)
  in
  let seed =
    match !seed with
    | Some s -> s
    | None -> Random.State.bits (Random.State.make_self_init ())
  in
  let side_name b = if b then "ppo" else "greedy" in
  let file_opt name = function None -> "" | Some f -> Printf.sprintf " --%s=%s" name f in
  (* the options as given on the command line, so the run can be repeated *)
  Printf.printf
    "train --rounds=%d --time=%g --seed=%d --hider=%s --hunter=%s --speed=%g \
     --batch=%d%s%s  (dt %g s)\n\
     %!"
    !rounds !time_limit seed (side_name !hider_ppo) (side_name !hunter_ppo) !speed !batch
    (file_opt "out" !out) (file_opt "in" !input) Trainer.dt;
  Option.iter
    (fun (w : Weights.t) -> Printf.printf "loaded %s (saved by a run with --seed=%d)\n" (Option.get !input) w.seed)
    loaded;
  (* same window setup as bin/main.ml, without a target FPS so drawing never
     waits; [show] does the pacing *)
  Raylib.set_config_flags Raylib.ConfigFlags.window_resizable;
  Raylib.init_window 400 400 "hideseek training";
  Raylib.set_exit_key Raylib.Key.Q;
  let sprites = Display.load_sprites () in
  let rng = Random.State.make [| seed |] in
  let from_file f = Option.bind loaded f in
  let hider =
    start_agent rng "hider" ~ppo:!hider_ppo ~loaded:(from_file (fun w -> w.hider))
  in
  let hunter =
    start_agent rng "hunter" ~ppo:!hunter_ppo ~loaded:(from_file (fun w -> w.hunter))
  in
  let hider = Option.map Ppo.learner hider and hunter = Option.map Ppo.learner hunter in
  (* [hider_xp] and [hunter_xp] are the experience since the last update,
     [survived] the lengths of the rounds since then *)
  let rec loop i hider hunter hider_xp hunter_xp survived tally game_total wall_total =
    if i > !rounds then (hider, hunter, tally, game_total, wall_total)
    else
      let w = Spawn.randomize rng World.default in
      let t0 = Unix.gettimeofday () in
      let o, secs, (ex : Trainer.experience) =
        Trainer.play_ppo
          ~on_step:(show sprites t0 !time_limit)
          rng ~time_limit:!time_limit
          ~hider:(Option.map Ppo.agent hider) ~hunter:(Option.map Ppo.agent hunter) w
      in
      let wall = Unix.gettimeofday () -. t0 in
      let tally = Trainer.record tally o in
      Printf.printf
        "round %d/%d: %s in %5.2f s | hiders %5.1f%% hunters %5.1f%% | x%.1f\n%!"
        i !rounds (outcome_name o) secs
        (pct (Trainer.hider_rate tally))
        (pct (Trainer.hunter_rate tally))
        (speedup ~game:secs ~wall);
      let hider_xp = ex.hider :: hider_xp and hunter_xp = ex.hunter :: hunter_xp in
      let survived = secs :: survived in
      if i mod !batch = 0 || i = !rounds then begin
        let n = List.length survived in
        Printf.printf "  rounds %d-%d: average survival %.2f s\n%!" (i - n + 1) i
          (List.fold_left ( +. ) 0. survived /. float n);
        (* the updated learner and its losses on the batch it learned from *)
        let update l xp =
          Option.map
            (fun l ->
              let samples = Array.concat (List.rev xp) in
              let l = Ppo.update rng l samples in
              (l, Ppo.losses l samples))
            l
        in
        let t0 = Unix.gettimeofday () in
        let hider_u = update hider hider_xp and hunter_u = update hunter hunter_xp in
        let hider = Option.map fst hider_u and hunter = Option.map fst hunter_u in
        if hider <> None || hunter <> None then begin
          Printf.printf "  updated after round %d in %.2f s\n%!" i
            (Unix.gettimeofday () -. t0);
          let report name =
            Option.iter (fun (_, (p, v)) ->
                Printf.printf "    %-6s policy loss %9.4f | value loss %12.4f\n%!" name p v)
          in
          report "hider" hider_u;
          report "hunter" hunter_u
        end;
        loop (i + 1) hider hunter [] [] [] tally (game_total +. secs) (wall_total +. wall)
      end
      else
        loop (i + 1) hider hunter hider_xp hunter_xp survived tally (game_total +. secs)
          (wall_total +. wall)
  in
  let hider, hunter, tally, game_total, wall_total =
    loop 1 hider hunter [] [] [] Trainer.empty 0. 0.
  in
  Printf.printf
    "after %d rounds: hiders %d (%.1f%%), hunters %d (%.1f%%) | average \
     survival %.2f s | x%.1f overall\n"
    (Trainer.rounds tally) (Trainer.hider_wins tally)
    (pct (Trainer.hider_rate tally))
    (Trainer.hunter_wins tally)
    (pct (Trainer.hunter_rate tally))
    (game_total /. float (Trainer.rounds tally))
    (speedup ~game:game_total ~wall:wall_total);
  Display.unload_sprites sprites;
  Raylib.close_window ();
  match !out with
  | Some f ->
      Weights.save f
        { seed; hider = Option.map Ppo.agent hider; hunter = Option.map Ppo.agent hunter };
      Printf.printf "saved to %s\n" f
  | None ->
      if !hider_ppo || !hunter_ppo then
        print_endline "no --out given, so the trained networks were not saved"
