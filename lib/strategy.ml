module type S = sig
  type t

  val init : World.t -> t
  val decide : t -> World.t -> Entity.t -> t * Vec2d.t
end

module Stay : S = struct
  type t = unit

  let init _ = ()
  let decide () _ _ = ((), Vec2d.zero)
end
