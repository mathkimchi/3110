(** Small fully connected neural networks: tanh on every hidden layer and a
    plain linear output layer. Only the forward pass lives here; training
    (backprop and Adam) runs in PyTorch, see [Torch_backend]. *)

type t
(** A network: one weight matrix and one bias row per layer.
    AF: layers [(w1, b1); ...; (wk, bk)] compute
    [x -> tanh (... tanh (x w1 + b1) ...) wk + bk], with tanh after every
    layer except the last.
    RI: there is at least one layer; layer [i]'s [w] is [n_i x n_(i+1)]
    (stored row-major) and its [b] has [n_(i+1)] entries, where [sizes] is
    [n_0; ...; n_k]. *)

val init : Random.State.t -> int list -> t
(** [init rng sizes] is a network whose layer widths are [sizes] (input
    first, output last), with weights drawn uniformly from
    [±1/sqrt fan_in] using [rng] and zero biases.
    Raises [Invalid_argument] if [sizes] has fewer than two entries or any
    entry is not positive. *)

val sizes : t -> int list
(** [sizes n] is the layer widths [n] was made with, input first. *)

val params : t -> float array list
(** [params n] is [w1; b1; w2; b2; ...], each weight matrix flattened
    row-major ([n_i x n_(i+1)]) and each bias as [n_(i+1)] entries. *)

val with_params : t -> float array list -> t
(** [with_params n ps] is [n] with its arrays replaced by [ps], in the order
    [params] gives. Raises [Invalid_argument] if any length differs. *)

val forward : t -> float array -> float array
(** [forward n x] runs [n] on the single input [x].
    Raises [Invalid_argument] if [x] is not as long as the input layer. *)

val write : out_channel -> t -> unit
(** [write oc n] writes [n] to [oc] as text: the layer widths, then every
    weight and bias, exactly (hex floats). *)

val read : Scanf.Scanning.in_channel -> t
(** [read ic] reads back one network written by [write].
    Raises [Failure] or [Scanf.Scan_failure] if [ic] does not hold one. *)
