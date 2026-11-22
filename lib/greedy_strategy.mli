(** A strategy that does whatever looks best right now, ignoring walls:
    hunters chase the closest hider and hiders run from the closest hunter. *)

include Strategy.S
(** [decide w e] looks at the living entities of [w] with the other role from
    [e] and picks the one whose center is closest to [e]'s center, the first
    in [World.entities w] order on a tie. A hunter heads straight for that
    entity's center; a hider heads straight away from it. Is [Vec2d.zero] if
    there is no such entity or its center is the same as [e]'s. *)
