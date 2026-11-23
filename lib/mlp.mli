(** Small fully connected neural networks: tanh on every hidden layer and a
    plain linear output layer. Built on owl-base's reverse-mode autodiff so
    [Ppo] can differentiate a loss through them. *)

module N = Owl_base_dense_ndarray.D
(** Plain double-precision arrays. *)

type arr = N.arr
(** A plain (non-differentiable) array of floats. *)

val to_array : arr -> float array
(** [to_array a] is every element of [a] in row-major order. *)

module AD :
  Owl_algodiff_generic_sig.Sig with type A.arr = arr and type A.elt = float
(** owl-base autodiff over [arr]s. *)

type t
(** A network: one weight matrix and one bias row per layer.
    AF: layers [(w1, b1); ...; (wk, bk)] compute
    [x -> tanh (... tanh (x w1 + b1) ...) wk + bk], with tanh after every
    layer except the last.
    RI: there is at least one layer; layer [i]'s [w] is [n_i x n_(i+1)] and its
    [b] is [1 x n_(i+1)] where [sizes] is [n_0; ...; n_k]. *)

val init : Random.State.t -> int list -> t
(** [init rng sizes] is a network whose layer widths are [sizes] (input
    first, output last), with weights drawn uniformly from
    [±1/sqrt fan_in] using [rng] and zero biases.
    Raises [Invalid_argument] if [sizes] has fewer than two entries or any
    entry is not positive. *)

val sizes : t -> int list
(** [sizes n] is the layer widths [n] was made with, input first. *)

val params : t -> arr list
(** [params n] is [w1; b1; w2; b2; ...], the arrays [forward_ad] takes. *)

val with_params : t -> arr list -> t
(** [with_params n ps] is [n] with its arrays replaced by [ps], in the order
    [params] gives. Raises [Invalid_argument] if any shape differs. *)

val forward_ad : AD.t list -> AD.t -> AD.t
(** [forward_ad ps x] runs the network whose [params] are [ps] on the batch
    [x] ([batch x input], one row per example), giving [batch x output].
    Differentiable in both [ps] and [x]. *)

val forward : t -> float array -> float array
(** [forward n x] runs [n] on the single input [x].
    Raises [Invalid_argument] if [x] is not as long as the input layer. *)

val write : out_channel -> t -> unit
(** [write oc n] writes [n] to [oc] as text: the layer widths, then every
    weight and bias, exactly (hex floats). *)

val read : Scanf.Scanning.in_channel -> t
(** [read ic] reads back one network written by [write].
    Raises [Failure] or [Scanf.Scan_failure] if [ic] does not hold one. *)
