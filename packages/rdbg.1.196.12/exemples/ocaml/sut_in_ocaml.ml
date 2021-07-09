(**************************************************************************)
(* Utilities *)

(* open Rdbg *)
open Data
let get_float var sol =  
  match (try List.assoc var sol with Not_found -> assert false) 
  with (F f) -> f | _ -> 
    Printf.printf "*** error when parsing float\n";
    flush stdout;
    assert false
 
let get_bool var sol =  
  match (try List.assoc var sol with Not_found -> assert false) 
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

let (inputs : (string * Data.t) list)  = [ "x", Data.Real; "y", Data.Real]
let (outputs :(string * Data.t) list) = [ "a", Data.Real; "b", Data.Real]

let (step :  Data.subst list -> Data.subst list) =
  fun sut_outs -> 
    let x = get_float "x" sut_outs
    and y = get_float "y" sut_outs in
    let a = x +. 1.0 and b = y +. 1.0 in
(*     Printf.printf "Sut step x=%f y=%f a=%f b=%f\n" x y a b; *)
(*     flush stdout; *)
      [("a",F a);("b",F b)]

type ctx = RdbgEvent.t
type e = RdbgEvent.t

let (step_dbg : Data.subst list -> ctx -> (Data.subst list -> ctx -> e)-> e) =
  fun sl ctx cont -> 
  let atom = {
    RdbgEvent.file = "sut.ml" ; 
    RdbgEvent.str = "
                 let x = get_float \"x\" sut_outs
                 and y = get_float \"y\" sut_outs in
                 let a = x +. 1.0 and b = y +. 1.0 in
                 [(\"a\",F a);(\"b\",F b)]
                 " ;
    RdbgEvent.line = 30,37; (* a bit touchy to be kept in sync manually... *)
    RdbgEvent.char = 871,1098 ; (* even worse! *)
    RdbgEvent.stack= None; 
  }
  in
  let sinfo = {
    (* it's not really mandatory to define it (i.e., to put a
         non-empty list in there), in particular if it is not compiler
         generated code... *)
    RdbgEvent.atoms = [atom]; 
    RdbgEvent.expr = ( (* not touchy this time, but tedious.. *)
      let xp1   = Expr.Op(Expr.Sum, [Expr.Var "x"; Expr.Fval 1.0]) in
      let yp1   = Expr.Op(Expr.Sum, [Expr.Var "y"; Expr.Fval 1.0]) in
      let def_a = Expr.Op(Expr.Eq, [Expr.Var "a"; xp1]) in
      let def_b = Expr.Op(Expr.Eq, [Expr.Var "b"; yp1]) in
      Expr.Op(Expr.And, [def_a; def_b]));  
    RdbgEvent.more = None;  (* stub *)
    RdbgEvent.in_subst = [];
    RdbgEvent.out_subst = [];                       
  }
  in
  let cont2 sl  ctx =
    let nctx = RdbgEvent.incr_event_nb ctx in
    { ctx with
      RdbgEvent.step = ctx.RdbgEvent.step;
      RdbgEvent.data = sl;
      RdbgEvent.nb = ctx.RdbgEvent.nb;
      RdbgEvent.depth = ctx.RdbgEvent.depth;
      RdbgEvent.kind = RdbgEvent.Exit;
      RdbgEvent.lang = "ocaml";
      RdbgEvent.name = "ze sut";
      RdbgEvent.inputs = inputs ;
      RdbgEvent.outputs = outputs;
      RdbgEvent.locals = [];
      RdbgEvent.sinfo = None; 
      RdbgEvent.next = (fun () -> cont sl nctx);
      RdbgEvent.terminate = ctx.RdbgEvent.terminate;
      RdbgEvent.reset = ctx.RdbgEvent.reset;
    }
  in
  let nctx = RdbgEvent.incr_event_nb ctx in
  { ctx with
    RdbgEvent.step = ctx.RdbgEvent.step;
    RdbgEvent.data = sl;
    RdbgEvent.nb = ctx.RdbgEvent.nb;
    RdbgEvent.depth = ctx.RdbgEvent.depth;
    RdbgEvent.kind = RdbgEvent.Call;
    RdbgEvent.lang = "ocaml";
    RdbgEvent.name = "ze sut";
    RdbgEvent.inputs = inputs ;
    RdbgEvent.outputs = outputs;
    RdbgEvent.locals = [];
    RdbgEvent.sinfo = Some (fun() -> sinfo);
    RdbgEvent.next = (fun () -> cont2 (step sl) nctx);
    RdbgEvent.terminate = ctx.RdbgEvent.terminate;
    RdbgEvent.reset = ctx.RdbgEvent.reset;
  } 

open RdbgPlugin 
let plugin = {
  id = "ocaml sut";
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


