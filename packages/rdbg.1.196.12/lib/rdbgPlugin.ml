(* Time-stamp: <modified the 16/03/2020 (at 11:33) by Erwan Jahier> *)

type sl = ( string * Data.v) list (* substitutions *)
type e = RdbgEvent .t
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

let dummy = {
  id = "dummy";
  inputs = [];
  outputs= [];
  reset= (fun _ -> ());
  kill= (fun _ -> ());
  save_state= (fun _ -> ());
  restore_state= (fun _ -> ());
  init_inputs=[];
  init_outputs=[];
  step= (fun _sl -> assert false);     
  step_dbg=(fun _ _ -> assert false); 
}
