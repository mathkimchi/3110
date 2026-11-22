type t = { x : float; y : float }
let zero = { x = 0.; y = 0. }
let add a b = { x = a.x +. b.x; y = a.y +. b.y }
let scale k v = { x = k *. v.x; y = k *. v.y }
let len v = sqrt ((v.x *. v.x) +. (v.y *. v.y))
let normalize v = let l = len v in if l = 0. then zero else scale (1. /. l) v
let sub a b = { x = a.x -. b.x; y = a.y -. b.y }
let dist a b = len (sub a b)
