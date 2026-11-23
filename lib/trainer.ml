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
