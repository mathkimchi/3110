let up = Vec2d.{ x = 0.; y = -1. }
let left = Vec2d.{ x = -1.; y = 0. }
let down = Vec2d.{ x = 0.; y = 1. }
let right = Vec2d.{ x = 1.; y = 0. }

let key_directions =
  Raylib.Key.
    [
      (W, up); (A, left); (S, down); (D, right);
      (Up, up); (Left, left); (Down, down); (Right, right);
    ]

(* summing lets diagonals combine; Physics.move normalizes, so they aren't faster *)
let direction_of_keys down =
  List.fold_left
    (fun acc (key, dir) -> if down key then Vec2d.add acc dir else acc)
    Vec2d.zero key_directions

let decide _ _ = direction_of_keys Raylib.is_key_down
