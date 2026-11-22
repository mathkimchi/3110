let center e = Rect.center (Entity.hitbox e)

let opponent_role : Entity.role -> Entity.role = function
  | Hunter -> Hider
  | Hider -> Hunter

(* strict < keeps the earlier entity on a tie *)
let closest from (targets : Entity.t list) =
  List.fold_left
    (fun best (t : Entity.t) ->
      let d = Vec2d.dist from (center t) in
      match best with
      | Some (_, best_d) when best_d <= d -> best
      | _ -> Some (t, d))
    None targets

let decide w (e : Entity.t) =
  let me = center e in
  let targets =
    World.entities_by_role w (opponent_role e.role)
    |> List.filter (fun (t : Entity.t) -> t.alive)
  in
  match closest me targets with
  | None -> Vec2d.zero
  | Some (t, _) -> (
      let toward = Vec2d.sub (center t) me in
      match e.role with
      | Hunter -> toward
      | Hider -> Vec2d.scale (-1.) toward)
