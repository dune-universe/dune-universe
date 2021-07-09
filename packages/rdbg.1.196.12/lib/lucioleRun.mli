(* Time-stamp: <modified the 16/07/2020 (at 09:46) by Erwan Jahier> *)

(**  This module has a twin: RifRun *)
type vars = (string * string) list
type sl = Data.subst list

(** Returns a kill and a step function. If the step returns None, its means a reset occurred *)
val make : string -> vars -> vars -> (string -> unit) * (sl -> sl option)
