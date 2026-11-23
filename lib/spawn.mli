(** Random start positions, so each training round starts somewhere new. *)

val randomize : ?min_gap:float -> Random.State.t -> World.t -> World.t
(** [randomize ~min_gap rng w] is [w] with every entity moved to a random
    position drawn from [rng], keeping the entities' order and every field
    except [pos]. Each entity is placed uniformly at random so that its hitbox
    lies inside the bounding box of [w]'s walls, overlaps no wall and no
    entity placed before it, and its center is at least [min_gap] (default
    [100.]) from the centers of the entities placed before it.
    Raises [Failure] if some entity still has no such position after 10000
    tries. Requires: [w] has at least one wall. *)
