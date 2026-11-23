(* Training runner: plays rounds on the default map without a window, each
   from random start positions, and prints the running win rate. *)
open Hideseek

let rounds = ref 100
let time_limit = ref 20.
let seed = ref None

let positive name set x =
  if x <= 0 then raise (Arg.Bad (name ^ " must be greater than 0"));
  set x

let speclist =
  [
    ( "--rounds",
      Arg.Int (positive "--rounds" (fun n -> rounds := n)),
      "N  how many rounds to play (default 100)" );
    ( "--time",
      Arg.Float
        (fun t ->
          if t <= 0. then raise (Arg.Bad "--time must be greater than 0");
          time_limit := t),
      "SECONDS  how long the hiders must survive to win (default 20)" );
    ( "--seed",
      Arg.Int (fun s -> seed := Some s),
      "N  seed for the start positions (default: random)" );
  ]

let usage = "train [--rounds=N] [--time=SECONDS] [--seed=N]"

(* World.default is [ hider 0; hunter 1 ]; both play greedily for now *)
let strategies : (module Strategy.S) list =
  [ (module Greedy_strategy); (module Greedy_strategy) ]

let outcome_name = function
  | Game.Hiders_win -> "hiders win "
  | Game.Hunters_win -> "hunters win"
  | Game.Ongoing -> "ongoing    "

let pct r = 100. *. r

(* how many game seconds passed per wall-clock second; infinite if the clock
   did not move *)
let speedup ~game ~wall = if wall > 0. then game /. wall else infinity

let () =
  Arg.parse speclist
    (fun a -> raise (Arg.Bad ("unexpected argument " ^ a)))
    usage;
  let seed =
    match !seed with
    | Some s -> s
    | None -> Random.State.bits (Random.State.make_self_init ())
  in
  Printf.printf "seed %d, %d rounds, %g s each, dt %g s\n%!" seed !rounds
    !time_limit Trainer.dt;
  let rng = Random.State.make [| seed |] in
  let rec loop i tally game_total wall_total =
    if i > !rounds then (tally, game_total, wall_total)
    else
      let w = Spawn.randomize rng World.default in
      let g = Game.init ~time_limit:!time_limit w strategies in
      let t0 = Unix.gettimeofday () in
      let o, secs = Trainer.play g in
      let wall = Unix.gettimeofday () -. t0 in
      let tally = Trainer.record tally o in
      Printf.printf
        "round %d/%d: %s in %5.2f s | hiders %5.1f%% hunters %5.1f%% | x%.1f\n%!"
        i !rounds (outcome_name o) secs
        (pct (Trainer.hider_rate tally))
        (pct (Trainer.hunter_rate tally))
        (speedup ~game:secs ~wall);
      loop (i + 1) tally (game_total +. secs) (wall_total +. wall)
  in
  let tally, game_total, wall_total = loop 1 Trainer.empty 0. 0. in
  Printf.printf
    "after %d rounds: hiders %d (%.1f%%), hunters %d (%.1f%%) | x%.1f overall\n"
    (Trainer.rounds tally) (Trainer.hider_wins tally)
    (pct (Trainer.hider_rate tally))
    (Trainer.hunter_wins tally)
    (pct (Trainer.hunter_rate tally))
    (speedup ~game:game_total ~wall:wall_total)
