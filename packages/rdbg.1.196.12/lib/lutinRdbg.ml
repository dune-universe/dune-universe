(* Time-stamp: <modified the 16/03/2020 (at 11:33) by Erwan Jahier> *)


type lut_evt =
  | Ltop | Call | Exit 
  | Quit (* when a run is quited*)
  | Try  (* Try a constraint to compute the current step *)
  | Sat  (* the tryied constraint is Satisfiable *)
  | Nsat (* the tryied constraint is Not satisfiable *)
(*   | Deadlock *)
(* 
   (Ltop (Call (Try.Nsat*.Sat))* Exit)* )*
 *)


let (to_lut_evt: RdbgEvent.kind -> lut_evt) = function 
  | RdbgEvent.Ltop -> Ltop
  | RdbgEvent.Call -> Call
  | RdbgEvent.Exit -> Exit
(*   | RdbgEvent.Abort -> Deadlock *)
  | RdbgEvent.MicroStep "try " -> Try
  | RdbgEvent.MicroStep "sat " -> Sat
  | RdbgEvent.MicroStep "usat" -> Nsat
  | RdbgEvent.MicroStep "quit" -> Quit
  | RdbgEvent.MicroStep e -> failwith (e^": Unknown event")

let (from_lut_evt: lut_evt  -> RdbgEvent.kind) = function
  | Ltop -> RdbgEvent.Ltop
  | Call -> RdbgEvent.Call
  | Exit -> RdbgEvent.Exit
  | Quit -> RdbgEvent.MicroStep "quit"
  | Try  -> RdbgEvent.MicroStep "try "
  | Sat  -> RdbgEvent.MicroStep "sat "
  | Nsat -> RdbgEvent.MicroStep "usat"
(*   | Deadlock -> RdbgEvent.Abort *)


(***************************************************************************)
(* a very simple Lutin Profiler *)
open RdbgEvent

let prof_tbl = Hashtbl.create 50 

let incr_prof (si:RdbgEvent.src_info_atom) = 
  try 
    let cpt = Hashtbl.find prof_tbl si in
      Hashtbl.replace prof_tbl si (cpt+1)
  with 
      Not_found -> Hashtbl.add prof_tbl si 1
 
let (prof_add: RdbgEvent.t -> unit) =
  fun e ->     
    match to_lut_evt e.kind, e.sinfo with
      | (Sat | Nsat), Some src -> List.iter incr_prof (src()).atoms
      | _ -> ()

let (profiler : bool -> unit) = 
  fun on -> 
    if on then RdbgStdLib.add_hook "profile" prof_add 
          else RdbgStdLib.del_hook "profile"

let (reset_profiler : unit -> unit) = 
  fun () -> 
    Hashtbl.clear prof_tbl

let (dump_profile_info : unit -> string) =
  fun () -> 
    let str_l = 
      Hashtbl.fold
        (fun si cpt acc-> 
           (Printf.sprintf "| %3i-%-3i | %5i-%-5i | %4i | %40s | %10s | \n" 
              (fst si.line) (snd si.line) (fst si.char) (snd si.char) 
              cpt si.str (Filename.basename si.file))::acc
        )
        prof_tbl
        []
    in
    let str_l = List.sort compare str_l in
      ("
|---------+-------------+------+------------------------------------------+------------+ 
|  lines  |    chars    | hits |             source code                  | file name  |
|---------+-------------+------+------------------------------------------+------------+
") ^
        (String.concat "" str_l) ^
        ("|---------+-------------+------+------------------------------------------+------------+ \n")


(***************************************************************************)
let (explain_failure : RdbgEvent.t -> unit) = 
  fun e ->
    (match to_lut_evt e.kind, e.sinfo with
      | Nsat, Some src_info -> 
        let si = src_info () in 
        print_string "\n\nThe current constraint is unsatisfiable:\n";
        Expr.dump si.expr;
        (match si.more with
          | None -> () 
          | Some more -> 
            print_string "\nBecause this one is unsatisfiable:\n";
            Expr.dump (more ()))
      | Nsat, None -> print_string "No source info is avaible.\n"
      | _,_ -> print_string "Current RdbgEvent is not a usat event.\n"
    );
    flush stdout
