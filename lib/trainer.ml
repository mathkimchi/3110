(* Game seconds per step, so 20 decisions per second. The discount and GAE
   lambda are set in seconds ([Ppo.halflife], [Ppo.lambda_halflife]), so
   changing dt does not change how far ahead the agents look. *)
let dt = 1. /. 20.

let play g =
  let start = Game.time_left g in
  let rec go g =
    match Game.step g dt with
    | g, Game.Ongoing -> go g
    | g, o -> (o, start -. Game.time_left g)
  in
  go g

type tally = { hider_wins : int; hunter_wins : int }

let empty = { hider_wins = 0; hunter_wins = 0 }

let record t = function
  | Game.Hiders_win -> { t with hider_wins = t.hider_wins + 1 }
  | Game.Hunters_win -> { t with hunter_wins = t.hunter_wins + 1 }
  | Game.Ongoing -> invalid_arg "Trainer.record: game is still ongoing"

let rounds t = t.hider_wins + t.hunter_wins
let hider_wins t = t.hider_wins
let hunter_wins t = t.hunter_wins
let rate wins t = if rounds t = 0 then 0. else float wins /. float (rounds t)
let hider_rate t = rate t.hider_wins t
let hunter_rate t = rate t.hunter_wins t

(* ---------- PPO rounds ---------- *)

(* Paid each step the hider is still alive afterwards: 1 - gamma to the
   hider, the negative to the hunter. Nothing is paid on the catching step
   or after it, so a hider caught after surviving n steps has return
   (1 - gamma)(1 + gamma + ... + gamma^(n-1)) = 1 - gamma^n
   = 1 - 0.5^(seconds / Ppo.halflife): 0 if caught at once, 0.5 after one
   halflife, toward 1 if never caught. The value network predicts that, so
   returns stay in [0, 1]. *)
let survival_reward (role : Entity.role) =
  let r = 1. -. Ppo.gamma ~dt in
  match role with Hider -> r | Hunter -> -.r

type experience = { hider : Ppo.sample array; hunter : Ppo.sample array }

(* one decision a PPO side made; [reward] is filled in after the step *)
type step = {
  obs : float array;
  act : float array;
  logp : float;
  value : float;
  mutable reward : float;
}

(* the decisions of one side, newest first *)
type log = { mutable steps : step list; mutable count : int }

let recorder rng ag log : (module Strategy.S) =
  (module struct
    let decide w e =
      let obs = Ppo.observe w e in
      let act, logp = Ppo.act rng ag obs in
      log.steps <- { obs; act; logp; value = Ppo.value ag obs; reward = 0. } :: log.steps;
      log.count <- log.count + 1;
      Vec2d.{ x = act.(0); y = act.(1) }
  end)

let samples ~bootstrap log =
  let steps = Array.of_list (List.rev log.steps) in
  let adv, ret =
    Ppo.gae ~bootstrap ~gamma:(Ppo.gamma ~dt) ~lambda:(Ppo.lambda ~dt)
      ~rewards:(Array.map (fun s -> s.reward) steps)
      ~values:(Array.map (fun s -> s.value) steps) ()
  in
  Array.mapi
    (fun i (s : step) ->
      { Ppo.obs = s.obs; act = s.act; logp = s.logp; adv = adv.(i); ret = ret.(i) })
    steps

let play_ppo ?(on_step = fun _ _ -> ()) rng ~time_limit ~hider ~hunter w =
  if List.length (World.hiders w) <> 1 || List.length (World.hunters w) <> 1
  then invalid_arg "Trainer.play_ppo: need exactly one hider and one hunter";
  let side agent =
    let log = { steps = []; count = 0 } in
    let strat =
      match agent with
      | Some ag -> recorder rng ag log
      | None -> (module Greedy_strategy : Strategy.S)
    in
    (log, strat)
  in
  let hider_log, hider_strat = side hider and hunter_log, hunter_strat = side hunter in
  let strategies =
    List.map
      (fun (e : Entity.t) -> match e.role with Hider -> hider_strat | Hunter -> hunter_strat)
      (World.entities w)
  in
  let g = Game.init ~time_limit w strategies in
  let start = Game.time_left g in
  let rec go g =
    let before = (hider_log.count, hunter_log.count) in
    let g, o = Game.step g dt in
    (* only a step the side decided in, that the hider lived through *)
    let credit log role count_before =
      match log.steps with
      | s :: _ when log.count > count_before && o = Game.Ongoing ->
          s.reward <- s.reward +. survival_reward role
      | _ -> ()
    in
    credit hider_log Entity.Hider (fst before);
    credit hunter_log Entity.Hunter (snd before);
    on_step g o;
    if o = Game.Ongoing then go g else (o, start -. Game.time_left g, g)
  in
  let o, secs, final = go g in
  (* a catch really ends the round (value 0 after it); running out of time
     only cuts it off, so the value of where the side stopped stands in for
     the rewards it would have kept getting *)
  let bootstrap agent pick =
    match (o, agent) with
    | Game.Hiders_win, Some ag ->
        Ppo.value ag (Ppo.observe (Game.world final) (List.hd (pick (Game.world final))))
    | _ -> 0.
  in
  ( o,
    secs,
    {
      hider = samples ~bootstrap:(bootstrap hider World.hiders) hider_log;
      hunter = samples ~bootstrap:(bootstrap hunter World.hunters) hunter_log;
    } )

let check_files ~hider_ppo ~hunter_ppo ~out ~input =
  if (not (hider_ppo || hunter_ppo)) && (out <> None || input <> None) then
    invalid_arg "--out and --in need --hider=ppo or --hunter=ppo"

let speeds = [ 1.; 5.; 20.; infinity ]

let faster s =
  match List.find_opt (fun x -> x > s) speeds with Some x -> x | None -> s

let slower s =
  match List.find_opt (fun x -> x < s) (List.rev speeds) with Some x -> x | None -> s
