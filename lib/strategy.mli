(** Hunter behaviours. Each strategy is a module matching [S], so different
    hunter AIs can be swapped in without changing [Game]. *)

module type S = sig
  val decide : World.t -> Entity.t -> Vec2d.t
  (** [decide w h] is the direction hunter [h] wants to move in world [w].
      The direction may have any length;
      [Physics.move] normalizes it. *)
end

module Stay : S
(** Never moves. Placeholder.*)
