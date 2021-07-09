(* Time-stamp: <modified the 16/03/2020 (at 11:33) by Erwan Jahier> *)

(** Some def that are specific to Lutin programs. *)

(* nb: This module should probably not be here in rdbg but in the Lutin plugin. *)
type lut_evt =
  | Ltop | Call | Exit 
  | Quit (* when a run is quited*)
  | Try  (* Try a constraint to compute the current step *)
  | Sat  (* the tryied constraint is Satisfiable *)
  | Nsat (* the tryied constraint is Not satisfiable *)
(*   | Deadlock *)

val to_lut_evt: RdbgEvent.kind -> lut_evt
val from_lut_evt: lut_evt  -> RdbgEvent.kind 

(** Try to hint why the current constraint failed (at fail events only) *)
val explain_failure : RdbgEvent.t -> unit


(** set on/off the profiler  *)
val profiler : bool -> unit

(** Reset profiling info  *)
val reset_profiler : unit -> unit


(** Dump profiling info *)
val dump_profile_info : unit -> string

(** control the behavior of print_event *)
(* val show_constraint : bool ref *)


