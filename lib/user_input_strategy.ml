let key_directions =
  Raylib.Key.
    [
      (W, Vec2d.{ x = 0.; y = -1. });
      (A, Vec2d.{ x = -1.; y = 0. });
      (S, Vec2d.{ x = 0.; y = 1. });
      (D, Vec2d.{ x = 1.; y = 0. });
    ]

(* summing lets diagonals combine; Physics.move normalizes, so they aren't faster *)
let direction_of_keys down =
  List.fold_left
    (fun acc (key, dir) -> if down key then Vec2d.add acc dir else acc)
    Vec2d.zero key_directions

let decide _ _ = direction_of_keys Raylib.is_key_down
