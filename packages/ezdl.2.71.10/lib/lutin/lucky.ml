(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: lucky.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)

(****************************************************************************)

open List
open Exp
open Var
open Prog

(****************************************************************************)
(****************************************************************************)

type cs  = Prog.ctrl_state
type csl = cs list
(* type sl  = subst list *)
type sl  = Value.OfIdent.t


(****************************************************************************)
let (draw_values : Var.env_in -> Prog.state -> int -> Thickness.numeric -> 
     FGen.t list -> FGen.t list * csl * (sl * sl) list) =
  fun input state p num_thickness ral ->
    let bool_to_gen_l = state.s.bool_vars_to_gen 
    and num_to_gen_l = state.s.num_vars_to_gen in
    let list_to_formula l =
      if l = [] then True else
	     List.fold_left (fun acc vn -> (And(Bvar(vn), acc)))
	       (Bvar(hd l)) (tl l)
    in
    let bool_to_gen_fl = List.map list_to_formula bool_to_gen_l in
    let rec loop_choose_formula bool_to_gen_f num_to_gen ra =
      let ctx_msg = Prog.ctrl_state_to_string_long state.d.ctrl_state in 
      let vl = state.d.verbose in
      let ra, f, cs = ra.FGen.choose_one_formula () in
      match
	     Solver.solve_formula state.d.snt input state.d.memory vl ctx_msg
        state.s.output_var_names p num_thickness bool_to_gen_f num_to_gen f 
		with
        | _snt, [] ->  (* The constraint is unsatisfiable because of the numerics *)
          loop_choose_formula bool_to_gen_f num_to_gen ra
        | _,sol_l -> ra, f, cs, sol_l
    in
    let (ral, _fl, csl, sol_ll) = 
      Util.list_split4(
        Util.list_map3 loop_choose_formula bool_to_gen_fl num_to_gen_l ral) 
    in
    (* fl is a list of formula that should actually be considered
       as a conjunction.  But because of the thickness, when we
       draw values in each formula in turn, we obtain a list of
       list that ougth to be transposed.  *)
    let sol_ll : (sl * sl) list list =  Util.transpose sol_ll in
    let (merge_sol : (sl * sl) list -> sl * sl) =
      fun sol_l -> (* Merge solutions coming from the fl: in other terms, we
                      interpret the formula list as a conjunction. *)
        List.fold_left
          (fun  (acc1, acc2) (sl1,sl2) -> (Value.OfIdent.union sl1 acc1, Value.OfIdent.union sl2 acc2))
          (Value.OfIdent.empty,Value.OfIdent.empty) 
          sol_l
    in
    let sol_l = List.map merge_sol sol_ll in
    assert (List.length ral = List.length csl);
		(* ZARBI 2010/12/10 *)
      (* assert (List.length ral = List.length sol_l); *)
    ral, csl,  sol_l


(****************************************************************************)
(*****************************************************************************)

(* Returns the subst for a memory box, if it can be computed *)
let (maybe_update_one_pre_var : env_in -> env_out -> env_loc -> sl ->
       Var.name * Exp.var -> subst option) =
  fun input (output:sl) (local:sl) (memory:sl) (pre_vn, vn) ->
    try (
      let v =
	match Var.mode vn with
	  | Input  -> Value.OfIdent.get input (Var.name vn)
	  | Output -> Value.OfIdent.get output (Var.name vn)
	  | Local  -> Value.OfIdent.get local (Var.name vn) 
	  | Pre -> Value.OfIdent.get memory (Var.name vn) 
      in
	Some (pre_vn,v)
    )   
    with Not_found -> None

      (* E.g., at the second step, input vars migth still not
	 be available if the env starts to generate outputs. *)
   
(** [update_pre input output local] Updates the values of pre
    variables (updating [env_state.pre]) with [input], [output], and
    [local].  *)
let (update_pre: env_in -> env_out -> env_loc -> Prog.state -> sl) =
  fun input output local state ->
    let maybe_pre =
      List.map (maybe_update_one_pre_var input output local state.d.memory)
	state.s.memories_names
    in
    let sl =
      (* OBSOLETE
      ( List.fold_left
	  (fun acc ms ->
	     match ms with
	     | Some(s) -> (s::acc)
	     | None -> acc
	  )
	  []
	  maybe_pre
      )
      *)
      ( List.fold_left
	  (fun acc ms ->
	     match ms with
	     | Some(s) -> (Value.OfIdent.add acc s)
	     | None -> acc
	  )
	  Value.OfIdent.empty
	  maybe_pre
      )
    in
      if state.d.verbose >= 3 then (
	print_string "*** Update the memory: \n";
	(* print_subst_list sl stdout; *)
	Value.OfIdent.print sl stdout;
	print_string "\n";
	List.iter (fun (x,_) -> print_string (x^" ")) state.s.memories_names;
	print_string "\n";
	flush stdout
      );
      sl

(*****************************************************************************)
(*****************************************************************************)


type step_mode =  StepInside | StepEdges | StepVertices
type solution =  Var.env_out * Var.env_loc


(* We save the inputs and the outputs in order to be able to restore them easily
   when the automata is blocked (when no transition can be taken in the 
   reactive mode).
*)
let previous_output = ref None
let previous_local = ref None

let (env_try_do : Thickness.formula_draw_nb * Thickness.numeric -> env_in -> 
      Prog.state -> FGen.t list -> (env_out * env_loc) list -> 
       FGen.t list * csl * (env_out * env_loc) list) =
  fun (p, num_thickness) input state ral acc ->
    let  ral', csl', sol = (draw_values input state p num_thickness ral) in
      ral', csl', (rev_append acc sol)
    
(* Exported *)
let (env_try : Thickness.t -> env_in -> Prog.state -> FGen.t list -> 
      FGen.t list * (env_out * env_loc) list) =
  fun (bt,nt) input state ral ->
(*     let ral = LucFGen.get input state in *)
    if fst bt then
      (* try all formula *)
      let rec f ral acc =
	try
	  let ral', _csl, acc' = env_try_do (snd bt,nt) input state ral acc in
	    f ral' acc'
	with 
	    FGen.NoMoreFormula -> ral,acc
      in
	f ral []
    else
      let (ral', _csl, sol) = env_try_do (snd bt,nt) input state ral [] in
	ral', sol
	
(*****************************************************************************)
(* Exported *)
let (env_step : step_mode -> env_in -> Prog.state ref -> FGen.t list -> 
     solution) =
  fun step_mode input state_ref ral ->
    let state = !state_ref in
    let thick =
      match step_mode with
	   | StepInside -> (1, 0, Thickness.AtMost 0)
	   | StepEdges -> (0, 1, Thickness.AtMost 0)
	   | StepVertices -> (0, 0, Thickness.AtMost 1)
    in
    try
      Utils.time_C "env_try_do";
	   let (_ral', csl, soll) = env_try_do (1, thick) input state ral [] in
      Utils.time_R "env_try_do";
	   let (output, local) = assert( soll <> []); List.hd soll in
	   let new_state_dyn = {
	     memory = update_pre input output local state;
	     ctrl_state = csl;
	     last_input = input;
	     last_output = output;
	     last_local  = local;
        snt = Bddd.clear state.d.snt;
	     verbose = state.d.verbose
	   }
	   in	
	   let new_state = { d = new_state_dyn ; s = state.s } in

	   previous_output := Some output;
	   previous_local := Some local;
      state_ref:= new_state;
	   (output, local)
    with
	   FGen.NoMoreFormula  -> 
	   match (!previous_output, !previous_local) with
	   | Some(out), Some(loc) ->
		  if state.s.reactive then 
		    (
		      if state.d.verbose >= 1 then 
			     print_string (
			       "### No transition is available; " ^ 
			       "values of the previous cycle are used.\n");
		      flush stdout;
		      (out, loc)
		    ) 
		  else
		    (
		      if state.d.verbose >= 1 then 
			     print_string (
			       "# No transition is labelled by a satisfiable formula.\n" ^
			       "# The program is blocked.\n");
		      flush stdout;
		      raise FGen.NoMoreFormula
		    )
	   | _, _ -> raise FGen.NoMoreFormula

