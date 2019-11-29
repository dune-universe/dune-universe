(* uses the native affinity interface to
   declare that the current process should be
   attached to core number n *)

(** [numcores ()] get the number of processors online. *)
external numcores: unit -> int = "numcores"

(** [setcore i] pin current process to core number [i]. *)
external setcore: int -> unit = "setcore"
