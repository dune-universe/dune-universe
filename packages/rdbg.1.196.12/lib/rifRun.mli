(* Time-stamp: <modified the 20/07/2020 (at 10:33) by Erwan Jahier> *)

(** Use rif-based stdio instead of luciole when inputs are missing.

   This module has a twin: LucioleRun
*)

type vars = (string * string) list
    
type sl = Data.subst list

(** Returns a kill and a step  function. If the step returns None, its
   means a reset occurred. *)
val make : vars -> vars -> (string -> unit) * (sl -> sl option)
