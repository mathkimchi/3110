(* Game seconds per step, so 20 decisions per second. The discount is set
   in seconds ([Ppo.halflife]), so changing dt does not change how far
   ahead the agents look; [Ppo.lambda] is still per step, though. *)
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

let center e = Rect.center (Entity.hitbox e)

(* Reward shaping each step: the hider gets distance / arena * dt, the
   hunter the negative. Per game second that is distance / arena, e.g. 0.5
   at 200 px apart (up to about 1.4 on the diagonal), so a 20 s round sums
   to about 10, or about 7 discounted from the start ([Ppo.halflife] 20 s).
   With [terminal_value] at 1 that outweighs winning and the agents mostly
   learn "keep distance" / "close distance"; a bigger [terminal_value] makes
   the outcome dominate. *)
let step_reward w (role : Entity.role) =
  let d =
    Vec2d.dist (center (List.hd (World.hiders w))) (center (List.hd (World.hunters w)))
  in
  let r = d /. Ppo.arena *. dt in
  match role with Hider -> r | Hunter -> -.r

(* What the winner gets (and the loser loses) on the last step. The same no
   matter when the catch happens; only discounting makes an early catch
   cost more. Compare with the shaping total in [step_reward]. *)
let terminal_value = 100.

let end_reward o (role : Entity.role) =
  match (o, role) with
  | Game.Ongoing, _ -> 0.
  | Game.Hiders_win, Hider | Game.Hunters_win, Hunter -> terminal_value
  | Game.Hiders_win, Hunter | Game.Hunters_win, Hider -> -. terminal_value

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

let samples log =
  let steps = Array.of_list (List.rev log.steps) in
  let adv, ret =
    Ppo.gae ~gamma:(Ppo.gamma ~dt) ~lambda:Ppo.lambda
      ~rewards:(Array.map (fun s -> s.reward) steps)
      ~values:(Array.map (fun s -> s.value) steps)
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
    let credit log role count_before =
      match log.steps with
      | s :: _ ->
          if log.count > count_before then
            s.reward <- s.reward +. step_reward (Game.world g) role;
          s.reward <- s.reward +. end_reward o role
      | [] -> ()
    in
    credit hider_log Entity.Hider (fst before);
    credit hunter_log Entity.Hunter (snd before);
    on_step g o;
    if o = Game.Ongoing then go g else (o, start -. Game.time_left g)
  in
  let o, secs = go g in
  (o, secs, { hider = samples hider_log; hunter = samples hunter_log })

let check_files ~hider_ppo ~hunter_ppo ~out ~input =
  if (not (hider_ppo || hunter_ppo)) && (out <> None || input <> None) then
    invalid_arg "--out and --in need --hider=ppo or --hunter=ppo"
