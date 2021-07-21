(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: prog.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)


(****************************************************************************)
(* exported *)
type atomic_ctrl_state = string
type ctrl_state = atomic_ctrl_state list



(****************************************************************************)
type dyn_weight = V of int | Infin

(** A wt (weigthed tree) is a n-ary tree *)
type wt = children Util.StringMap.t * string  (* the top-level node of the wt *) 
    
and children =
  | Children of (dyn_weight * string) list
  | Leave of Exp.formula * atomic_ctrl_state
  | Stop of string


(****************************************************************************)
(* exported *)
type  dynamic_state_fields = {
  (* memory  : Var.subst list; *)
  memory : Var.env;
  ctrl_state : ctrl_state list;
  (* input : Var.subst list; *)
  last_input : Var.env;
  last_output : Var.env;
  last_local : Var.env;
  snt: Solver.t;
  verbose : int
}

(* exported *)
type t = {
  initial_ctrl_state : ctrl_state list;
  in_vars  : Exp.var list;
  out_vars : Exp.var list;
  loc_vars : Exp.var list;
  ext_func_tbl : Exp.ext_func_tbl;
  memories_names   : (string * Exp.var) list ;
  bool_vars_to_gen : Exp.var list list ;
  num_vars_to_gen  : Exp.var list list ;
  output_var_names : Var.name list list ; (* redundant, but useful *)
  reactive : bool ; (* A scorie to remove. At least, it should be a list of bool *)

  get_wtl :  Var.env_in -> state -> ctrl_state -> wt list;
  is_final : ctrl_state list -> bool;
  gen_dot : ctrl_state list -> ctrl_state list -> string -> int
}

(* exported *)
and state = {
  d : dynamic_state_fields ;
  s : t
}

let (memory_of_state : state -> Var.env) = fun s -> s.d.memory 
let last_values_of_state st = (st.d.last_input, st.d.last_output, st.d.last_local)

(****************************************************************************)
(* exported *)
let (ctrl_state_to_string : ctrl_state -> string) =
  fun nl -> 
    String.concat "," nl

(* exported *)
let (string_to_atomic_ctrl_state : string -> atomic_ctrl_state) =
  fun str -> str

(* exported *)
let (ctrl_state_list_to_string_list : ctrl_state -> string list) =
  fun str -> str


(****************************************************************************)
(* exported *)
let (ctrl_state_to_string_long : ctrl_state list -> string) =
  fun ctrl_states -> 
    let cnl = List.map ctrl_state_to_string ctrl_states in
    let nodes_str = String.concat " " cnl in
    let node_msg =
      if
	     (List.length cnl) = 1
      then
	     ("current node: " ^ nodes_str)
      else
	     ("current nodes: " ^ nodes_str)
    in
      (node_msg ^ "\n")

(****************************************************************************)
let (compute_weight : Exp.weight -> Var.env_in -> state -> dyn_weight) =
  fun w input state ->
    match w with
	   Exp.Wint(i) -> V i
    | Exp.Infinity -> Infin
    | Exp.Wexpr(e) ->
      let ctx_msg = ctrl_state_to_string_long state.d.ctrl_state in 
      match Solver.eval_int_expr state.d.snt e ctx_msg input
              state.d.memory state.d.verbose
      with
      | Some v -> V v
      | None -> 
        print_string (
          "*** The weight expression " ^ (Exp.weight_to_string w) ^
          " ougth to depend only on pre and input vars\n");
        exit 2



(****************************************************************************)
(* exported *)
let dyn_weight_to_string =
  function
      V i -> string_of_int i
    | Infin -> "infinity"

(****************************************************************************)
(* exported *)
let rec (print_wt : wt -> unit) =
  fun (tbl, n) -> 
    print_string ("toplevel node:" ^ n ^ "\n");
    Util.StringMap.iter 
      (fun n children -> 
	      print_string ("\n\t" ^ n ^ " -> ");
	      print_children children
      )
      tbl;
    print_string "\n---------------------------------------------\n";
    flush stdout
and 
    print_children = function
	     Children l -> 
	       List.iter 
	         (fun (dw,n) -> 
	            print_string (dyn_weight_to_string dw);
	            print_string (":" ^ n ^ ", ");
	         )
	         l
      | Leave (f,next_state) -> 
	       print_string ((Exp.formula_to_string f) ^ "->"^next_state)
      | Stop str ->  print_string ("Stop " ^ str)
   



(****************************************************************************)
(* exported *)
let (print_mem : state -> unit) = 
  fun s -> 
    print_string "*** memories:\n";
    (* Var.print_subst_list s.d.memory stdout; *)
    output_string stdout (Value.OfIdent.to_string "" s.d.memory);
    flush stdout
  
(****************************************************************************)
