(** Entity behaviours. Each strategy is a module matching [S], so different
    controllers (AIs or the keyboard, see [User_input_strategy]) can be
    swapped in without changing [Game]. *)

module type S = sig
  val decide : World.t -> Entity.t -> Vec2d.t
  (** [decide w e] is the direction entity [e] wants to move in world [w].
      The direction may have any length;
      [Physics.move] normalizes it. *)
end

module Stay : S
(** Never moves. Placeholder.*)
