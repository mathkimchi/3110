module type S = sig
  val decide : World.t -> Entity.t -> Vec2d.t
end

module Stay : S = struct
  let decide _ _ = Vec2d.zero
end
