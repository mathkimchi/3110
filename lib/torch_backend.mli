(** A running [python/ppo_torch.py]: the PyTorch process that does PPO's
    backprop and Adam steps (issue #41). It is started once and then answers
    requests over pipes; see the script for the protocol. [Ppo.update]
    builds the requests. *)

type t
(** A running backend process. *)

val default_python : unit -> string
(** [default_python ()] is ["uv run --quiet"] if [uv] is on the PATH (it
    installs torch on first use from the script's header), otherwise
    ["python3"]. *)

val start : ?python:string -> script:string -> unit -> t
(** [start ~python ~script ()] runs the shell command [python script]
    ([python] defaults to [default_python ()]) and waits until it says it
    is ready.
    Raises [Failure] with a message saying what to install if [script] does
    not exist or the process exits before it is ready (e.g. torch is
    missing). *)

val request : t -> string -> float array -> string list * float array
(** [request b line floats] sends the request [line] (without the newline
    or the [floats=] key, which it adds) followed by [floats], and is the
    reply's words after ["ok <count>"] together with its [count] floats.
    Raises [Failure] if the backend replies with an error or has died. *)

val stop : t -> unit
(** [stop b] closes the pipes, which makes the process exit, and waits for
    it. *)

val hex : float -> string
(** [hex x] is [x] as a hex float (["%h"]), the way requests send floats in
    their header line. *)
