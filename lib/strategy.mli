(** Hunter behaviours. Each strategy is a module matching [S], so different
    hunter AIs can be swapped in without changing [Game]. *)

module type S = sig
  type t
  (** Whatever the strategy remembers between steps. *)

  val init : World.t -> t
  (** [init w] is the strategy's starting memory for world [w]. *)

  val decide : t -> World.t -> Entity.t -> t * Vec2d.t
  (** [decide mem w h] is the updated memory and the direction hunter [h]
      wants to move in world [w]. The direction may have any length;
      [Physics.move] normalizes it. *)
end

module Stay : S
(** Never moves. Placeholder for the prototype. *)
