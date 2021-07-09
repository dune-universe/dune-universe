(* Time-stamp: <modified the 16/03/2020 (at 11:34) by Erwan Jahier> *)

(* This module defines the data structure required by RDBG to run a Reactive Program. *)

type sl = ( string * Data.v) list (* substitutions *)
type e = RdbgEvent.t
type t = {
  id: string;
  inputs  : (Data.ident * Data.t) list; (* name and type *)
  outputs : (Data.ident * Data.t) list; (* ditto *)
  reset: unit -> unit;
  kill: string -> unit;
  save_state: int -> unit;
  restore_state: int -> unit;
  init_inputs  : sl;
  init_outputs : sl;
  step     : (sl -> sl); (* Lurette step *) 
  step_dbg : (sl -> e -> ( sl -> e -> e) -> e); (* RDBG step *)
}

val dummy:t 

