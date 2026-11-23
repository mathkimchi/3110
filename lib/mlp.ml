module N = Owl_base_dense_ndarray.D

type arr = N.arr

let to_array a =
  let flat = Bigarray.reshape_1 a (N.numel a) in
  Array.init (Bigarray.Array1.dim flat) (Bigarray.Array1.get flat)

module AD = Owl_algodiff_generic.Make (Owl_base_algodiff_primal_ops.D)

type t = { sizes : int list; layers : (arr * arr) list }

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
      N.init [| n_in; n_out |] (fun _ ->
          Random.State.float rng (2. *. bound) -. bound)
    in
    (w, N.zeros [| 1; n_out |])
  in
  { sizes; layers = List.map layer (pairs sizes) }

let sizes n = n.sizes
let params n = List.concat_map (fun (w, b) -> [ w; b ]) n.layers

let with_params n ps =
  let rec go layers ps =
    match (layers, ps) with
    | [], [] -> []
    | (w, b) :: layers, w' :: b' :: ps ->
        if N.shape w <> N.shape w' || N.shape b <> N.shape b' then
          invalid_arg "Mlp.with_params: shape mismatch";
        (w', b') :: go layers ps
    | _ -> invalid_arg "Mlp.with_params: wrong number of arrays"
  in
  { n with layers = go n.layers ps }

let rec forward_ad ps x =
  match ps with
  | [ w; b ] -> AD.Maths.(dot x w + b)
  | w :: b :: ps -> forward_ad ps AD.Maths.(tanh (dot x w + b))
  | _ -> invalid_arg "Mlp.forward_ad: need a weight and bias per layer"

let forward n x =
  if Array.length x <> List.hd n.sizes then
    invalid_arg "Mlp.forward: input has the wrong length";
  let x = AD.pack_arr (N.of_array x [| 1; Array.length x |]) in
  let ps = List.map AD.pack_arr (params n) in
  to_array (AD.unpack_arr (forward_ad ps x))

let write oc n =
  Printf.fprintf oc "mlp %d" (List.length n.sizes);
  List.iter (Printf.fprintf oc " %d") n.sizes;
  output_char oc '\n';
  List.iter
    (fun a ->
      Array.iter (Printf.fprintf oc " %h") (to_array a);
      output_char oc '\n')
    (params n)

let read ic =
  let count = Scanf.bscanf ic " mlp %d" Fun.id in
  let sizes = List.init count (fun _ -> Scanf.bscanf ic " %d" Fun.id) in
  if count < 2 || List.exists (fun n -> n <= 0) sizes then
    failwith "Mlp.read: bad layer widths";
  let read_arr shape =
    let len = Array.fold_left ( * ) 1 shape in
    N.of_array (Array.init len (fun _ -> Scanf.bscanf ic " %h" Fun.id)) shape
  in
  let layers =
    List.map
      (fun (n_in, n_out) ->
        let w = read_arr [| n_in; n_out |] in
        (w, read_arr [| 1; n_out |]))
      (pairs sizes)
  in
  { sizes; layers }
