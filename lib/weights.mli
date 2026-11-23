(** The file the training binary saves trained agents to ([--out]) and
    starts from ([--in]). *)

type t = {
  seed : int;  (** the seed of the run that saved the file *)
  hider : Ppo.agent option;  (** [None] if the hider was not PPO *)
  hunter : Ppo.agent option;  (** [None] if the hunter was not PPO *)
}

val save : string -> t -> unit
(** [save file w] writes [w] to [file] as text, replacing it. *)

val load : string -> t
(** [load file] reads back a file written by [save].
    Raises [Sys_error] if [file] cannot be opened, and [Failure] if it is not
    a weights file. *)
