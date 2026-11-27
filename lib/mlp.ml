(* a layer: [w] is [n_in x n_out] row-major, [b] has [n_out] entries *)
type layer = { n_in : int; n_out : int; w : float array; b : float array }
type t = { sizes : int list; layers : layer list }

(* consecutive pairs: [1; 2; 3] -> [(1, 2); (2, 3)] *)
let rec pairs = function
  | a :: (b :: _ as rest) -> (a, b) :: pairs rest
  | _ -> []

let init rng sizes =
  if List.length sizes < 2 || List.exists (fun n -> n <= 0) sizes then
    invalid_arg "Mlp.init: need at least two positive layer widths";
  let layer (n_in, n_out) =
    let bound = 1. /. sqrt (float n_in) in
    let w =
      Array.init (n_in * n_out) (fun _ ->
          Random.State.float rng (2. *. bound) -. bound)
    in
    { n_in; n_out; w; b = Array.make n_out 0. }
  in
  { sizes; layers = List.map layer (pairs sizes) }

let sizes n = n.sizes
let params n = List.concat_map (fun l -> [ Array.copy l.w; Array.copy l.b ]) n.layers

let with_params n ps =
  let rec go layers ps =
    match (layers, ps) with
    | [], [] -> []
    | l :: layers, w :: b :: ps ->
        if Array.length w <> Array.length l.w || Array.length b <> Array.length l.b
        then invalid_arg "Mlp.with_params: shape mismatch";
        { l with w = Array.copy w; b = Array.copy b } :: go layers ps
    | _ -> invalid_arg "Mlp.with_params: wrong number of arrays"
  in
  { n with layers = go n.layers ps }

(* [x w + b] for one input row [x] *)
let affine l x =
  Array.init l.n_out (fun j ->
      let s = ref l.b.(j) in
      for i = 0 to l.n_in - 1 do
        s := !s +. (x.(i) *. l.w.((i * l.n_out) + j))
      done;
      !s)

let forward n x =
  if Array.length x <> List.hd n.sizes then
    invalid_arg "Mlp.forward: input has the wrong length";
  let rec go x = function
    | [] -> x
    | [ l ] -> affine l x
    | l :: ls -> go (Array.map tanh (affine l x)) ls
  in
  go x n.layers

let write oc n =
  Printf.fprintf oc "mlp %d" (List.length n.sizes);
  List.iter (Printf.fprintf oc " %d") n.sizes;
  output_char oc '\n';
  List.iter
    (fun a ->
      Array.iter (Printf.fprintf oc " %h") a;
      output_char oc '\n')
    (params n)

let read ic =
  let count = Scanf.bscanf ic " mlp %d" Fun.id in
  let sizes = List.init count (fun _ -> Scanf.bscanf ic " %d" Fun.id) in
  if count < 2 || List.exists (fun n -> n <= 0) sizes then
    failwith "Mlp.read: bad layer widths";
  let read_arr len = Array.init len (fun _ -> Scanf.bscanf ic " %h" Fun.id) in
  let layers =
    List.map
      (fun (n_in, n_out) ->
        let w = read_arr (n_in * n_out) in
        { n_in; n_out; w; b = read_arr n_out })
      (pairs sizes)
  in
  { sizes; layers }
