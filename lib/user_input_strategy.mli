(** The keyboard as a strategy: the entity it controls moves the way the human
    is pressing W/A/S/D. Reads live key state from Raylib, so a Raylib window
    must be open when [decide] is called. *)

include Strategy.S
(** [decide w e] ignores [w] and [e] and is the direction of the W/A/S/D keys
    currently held down. *)

val direction_of_keys : (Raylib.Key.t -> bool) -> Vec2d.t
(** [direction_of_keys down] is the sum of the directions of the W/A/S/D keys
    for which [down] is true: W up, A left, S down, D right (y grows downward).
    Opposite keys cancel out; no keys gives [Vec2d.zero]. *)
