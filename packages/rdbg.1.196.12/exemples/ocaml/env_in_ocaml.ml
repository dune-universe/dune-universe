

(**************************************************************************)
(* Utilities *)

(* open Rdbg *)
open Data
let get_float var sol =  
  match (try List.assoc var sol with Not_found -> 
    Printf.printf "*** can not find the value of %s\n" var;
    flush stdout;
    assert false) 
  with (F f) -> f | _ -> 
    Printf.printf "*** error when parsing float\n";
    flush stdout;
    assert false
 
let get_bool var sol =  
  match (try List.assoc var sol with Not_found -> 
    Printf.printf "*** can not find the value of %s\n" var;
    flush stdout;
    assert false) 
  with (B b) -> b | _ -> 
    Printf.printf "*** error when parsing bool\n";
    flush stdout;
    assert false

(**************************************************************************)

let init  _ = ()
let kill  _ = ()
let reset () = ()
let ss _i = ()
let rs _i = ()
let mems_i = []
let mems_o = []

let (inputs  :(string * Data.t) list) = [ "a", Data.Real; "b", Data.Real]
let (outputs :(string * Data.t) list) = [ "x", Data.Real; "y", Data.Real]

let first_step = ref true
let (step :  Data.subst list -> Data.subst list)=
  fun env_outs -> 
    if !first_step  then  (
      (* ZZZ The env starts, and thus should be able to perform its
         first step with no input! *)
      first_step := false;
      [("x", F 10.0);("y", F 10.0)]
    )
    else 
      let a = get_float "a" env_outs
      and b = get_float "b" env_outs in
      let x = a +. 1.0 and y = b +. 1.0 in
(*       Printf.printf "Env step x=%f y=%f a=%f b=%f\n" x y a b; *)
(*       flush stdout; *)
      [("x",F x);("y",F y)]


type ctx = RdbgEvent.t
type e = RdbgEvent.t

let (step_dbg : Data.subst list -> ctx -> (Data.subst list -> ctx -> e) -> e) =
  fun sl ctx cont -> 
  let nctx = RdbgEvent.incr_event_nb ctx in
  let cont2 sl ctx =
    let nctx = RdbgEvent.incr_event_nb ctx in
    { ctx with
      RdbgEvent.step = ctx.RdbgEvent.step;
      RdbgEvent.data = sl;
      RdbgEvent.nb = ctx.RdbgEvent.nb;
      RdbgEvent.depth = ctx.RdbgEvent.depth;
      RdbgEvent.kind = RdbgEvent.Exit;
      RdbgEvent.lang = "ocaml";
      RdbgEvent.name = "ze env";
      RdbgEvent.inputs = inputs ;
      RdbgEvent.outputs = outputs;
      RdbgEvent.locals = [];
      RdbgEvent.sinfo = None; 
      RdbgEvent.next = (fun () -> cont sl nctx);
      RdbgEvent.terminate = ctx.RdbgEvent.terminate;
      RdbgEvent.reset = ctx.RdbgEvent.reset;
    }
  in
  { ctx with
    RdbgEvent.step = ctx.RdbgEvent.step;
    RdbgEvent.data = ctx.RdbgEvent.data;
    RdbgEvent.nb = ctx.RdbgEvent.nb;
    RdbgEvent.depth = ctx.RdbgEvent.depth;
    RdbgEvent.kind = RdbgEvent.Call;
    RdbgEvent.lang = "ocaml";
    RdbgEvent.name = "ze env";
    RdbgEvent.inputs = inputs ;
    RdbgEvent.outputs = outputs;
    RdbgEvent.locals = [];
    RdbgEvent.sinfo = None; (* cf sut.ml for a possible way to fill that in *)

    RdbgEvent.next = (fun () -> cont2 (step sl) nctx);
    RdbgEvent.terminate = ctx.RdbgEvent.terminate;
    RdbgEvent.reset = ctx.RdbgEvent.reset;
  }

      
open RdbgPlugin 
let plugin = {
  id = "ocaml env";
  inputs = inputs;
  outputs= outputs;
  reset = reset;
  kill= kill;
  save_state = ss;
  restore_state = rs;
  init_inputs= mems_i;
  init_outputs= mems_o;
  step = step;
  step_dbg = step_dbg
}

