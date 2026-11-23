let max_tries = 10_000

(* the smallest rectangle around every wall, as (min x, min y, max x, max y) *)
let bounds walls =
  List.fold_left
    (fun (x0, y0, x1, y1) (r : Rect.t) ->
      ( Float.min x0 r.pos.x,
        Float.min y0 r.pos.y,
        Float.max x1 (r.pos.x +. r.w),
        Float.max y1 (r.pos.y +. r.h) ))
    (infinity, infinity, neg_infinity, neg_infinity)
    walls

let center e = Rect.center (Entity.hitbox e)

(* [e] at a random spot that fits; [placed] are the entities already placed *)
let place rng ~min_gap walls (x0, y0, x1, y1) placed (e : Entity.t) =
  let fits (c : Entity.t) =
    let box = Entity.hitbox c in
    List.for_all (fun w -> not (Rect.overlaps w box)) walls
    && List.for_all
         (fun p ->
           (not (Rect.overlaps (Entity.hitbox p) box))
           && Vec2d.dist (center p) (center c) >= min_gap)
         placed
  in
  let rec try_ n =
    if n = 0 then failwith "Spawn.randomize: no room for an entity"
    else
      let x = x0 +. Random.State.float rng (Float.max 0. (x1 -. x0 -. e.size))
      and y = y0 +. Random.State.float rng (Float.max 0. (y1 -. y0 -. e.size)) in
      let c = { e with pos = Vec2d.{ x; y } } in
      if fits c then c else try_ (n - 1)
  in
  try_ max_tries

let randomize ?(min_gap = 100.) rng w =
  let walls = World.walls w in
  let box = bounds walls in
  let placed =
    List.fold_left
      (fun placed e -> place rng ~min_gap walls box placed e :: placed)
      [] (World.entities w)
  in
  World.with_entities w (List.rev placed)
