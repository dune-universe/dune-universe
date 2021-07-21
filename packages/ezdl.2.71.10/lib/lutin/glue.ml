
(*

Glue between lutin (CoAlgExp, CoIdent etc) and lucky ( Type Var Exp)
Necessary to reuse the lucky solver

*)

let _dbg = Verbose.get_flag "Glue"

(* CONVERSION TYPE LUTIN -> TYPE LUCKY (CkTypeEff.t  -> Type.t) *)
let lucky_type_of teff = (
	if (teff = CkTypeEff.boolean) then Type.BoolT
	else if (teff = CkTypeEff.integer) then Type.IntT
	else if (teff = CkTypeEff.real) then  Type.FloatT
	else (raise (LutErrors.Internal_error (
		"Glue.lucky_type_of",
		"Unexpected type"^(CkTypeEff.to_string teff)
	)))
) 

(* Utiles *)
let lucky_exp_zero = Exp.Numer (Exp.Ival (Num.num_of_int 0))

let lucky_exp_of_value = function
| Value.B true -> Exp.Formu Exp.True
| Value.B false -> Exp.Formu Exp.False
| Value.N (Value.I i) -> Exp.Numer (Exp.Ival i)
| Value.N (Value.F f) -> Exp.Numer (Exp.Fval f)


let lucky_exp_var_ref (x: Exp.var) = (
	match Var.typ x with
	|	Type.BoolT   -> Exp.Formu (Exp.Bvar x)
	|	Type.IntT    -> Exp.Numer (Exp.Ivar x)
	|	Type.FloatT  -> Exp.Numer (Exp.Fvar x)
	|  _ -> assert false
)


(* CONVERSION EXP LUTIN -> EXP LUCKY (CkTypeEff.t  -> Type.t) *)

(* Translators CoAlgExp to Exp take as argument
   the function in charge of translating ident refs:
	the argument MUST be a id ref (AE_pre, AE_support, AE_alias)

	If "eval", constant are propagated, in particular, an exp
	that contains NO vars if fully evaluated

	N.B. partial eval is defined in another module
*)
type id2exp = bool -> CoAlgExp.node -> Exp.t

let lucky_exp_of (eval:bool) (id2exp:id2exp) (e: CoAlgExp.t) = (

  (* recursive any type translator *) 
  let rec _lucky_exp_of e = (
	 let t = CoAlgExp.get_type e in
	 if t = CkTypeEff.boolean then
		Exp.Formu (_lucky_formula_of e)
	 else if t = CkTypeEff.integer then 
		Exp.Numer (_lucky_numexp_of e)
	 else if t = CkTypeEff.real then
		Exp.Numer (_lucky_numexp_of e)
	 else  failwith ("XXX :"^(CkTypeEff.to_string t)^":"^(CoAlgExp.lus_dumps e)^"\n")

  )
  (* recursive bool translator *) 
  and _lucky_formula_of e = (
	 let raw_res = (
		let nat = e.CoAlgExp.ae_val in
		match nat with
		|	CoAlgExp.AE_true -> Exp.True
		|	CoAlgExp.AE_false -> Exp.False
		|	CoAlgExp.AE_pre id 
		|	CoAlgExp.AE_support id
		|	CoAlgExp.AE_alias id -> (
			 match id2exp eval nat with
			 |	Exp.Formu f -> f
			 |	e -> (
				  let msg = "unexpected exp type for var \""^(CoIdent.to_string id)^"\""^
					         "     bool exp is expected but get \""^(Exp.to_string e)^"\""
				  in
				  raise (LutErrors.Internal_error ("Glue.lucky_formula_of", msg))
				)
		  )
		|	CoAlgExp.AE_call (id, ops) -> (
			 match (id, ops) with
			 |	("not", [o])     -> Exp.Not (_lucky_formula_of o)
			 |	("and", [o1;o2]) -> Exp.And (_lucky_formula_of o1, _lucky_formula_of o2)
			 |	("or", [o1;o2])  -> Exp.Or (_lucky_formula_of o1, _lucky_formula_of o2)
			 |	("xor", [o1;o2]) -> Exp.Xor (_lucky_formula_of o1, _lucky_formula_of o2)
			 |	("nxor", l) -> Exp.NXor (List.map _lucky_formula_of l)
			 |	("nor", l) -> Exp.Nor (List.map _lucky_formula_of l)
			 |	("#", l) -> Exp.Diese (List.map _lucky_formula_of l)

	       |	("impl", [o1;o2])   -> Exp.Impl (_lucky_formula_of o1, _lucky_formula_of o2)
	       |	("ite", [o1;o2;o3]) -> Exp.IteB (_lucky_formula_of o1,
                                             _lucky_formula_of o2, _lucky_formula_of o3)
			 |	("lt", [o1;o2])   -> Exp.Inf (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("lte", [o1;o2])  -> Exp.InfEq (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("gt", [o1;o2])   -> Exp.Sup (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("gte", [o1;o2])  -> Exp.SupEq (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("neq", [o1;o2])
			 |	("eq", [o1;o2])   -> (
				  let ty = CoAlgExp.get_type o1 in
				  let eqexp = if (ty = CkTypeEff.boolean ) then
						Exp.EqB (_lucky_formula_of o1, _lucky_formula_of o2)
					 else if ((ty = CkTypeEff.integer) || (ty = CkTypeEff.real)) then (
						Exp.Eq (_lucky_numexp_of o1, _lucky_numexp_of o2)
					 ) else (
						assert false
					 ) in
				  if (id = "neq") then Exp.Not eqexp else eqexp
				)
			 | _ -> assert false	
		  )
		|	CoAlgExp.AE_rconst x ->
	     raise (LutErrors.Internal_error (
            "lucky_formula_of", "unexpected AE_rconst "^x^" CoAlgExp.t" ))
		|	CoAlgExp.AE_iconst x ->
	     raise (LutErrors.Internal_error (
            "lucky_formula_of", "unexpected AE_iconst "^x^" CoAlgExp.t" ))
		|	CoAlgExp.AE_rval x ->
	     raise (LutErrors.Internal_error (
            "lucky_formula_of", "unexpected AE_rval "^(string_of_float x)^" CoAlgExp.t" ))
		|	CoAlgExp.AE_ival x ->
	     raise (LutErrors.Internal_error (
            "lucky_formula_of", "unexpected AE_ival "^
                                (string_of_int x)^" CoAlgExp.t" ))
		|	CoAlgExp.AE_const x ->
		  raise (LutErrors.Internal_error ( "lucky_formula_of", "unexpected AE_const "^x^" CoAlgExp.t" ))
		|  CoAlgExp.AE_external_call (_id, _ei, _prof, _ops) -> (
			 raise ( LutErrors.Internal_error (
				  "Glue.lucky_formula_of",
				  "sorry, bool type in external function not yet implemented"))
		  )
	 ) in if eval then ExpEval.simp_formula raw_res else raw_res
  )
  (* recursive num translator *) 
  and _lucky_numexp_of e = (
	 let raw_res = (
		let nat = e.CoAlgExp.ae_val in
		match nat with
		|	CoAlgExp.AE_iconst s -> Exp.Ival (Num.num_of_string s)
		|	CoAlgExp.AE_rconst s -> Exp.Fval (float_of_string s)
		|	CoAlgExp.AE_ival i -> Exp.Ival (Num.num_of_int i)
		|	CoAlgExp.AE_rval r -> Exp.Fval r
		|	CoAlgExp.AE_pre id 
		|	CoAlgExp.AE_support id
		|	CoAlgExp.AE_alias id -> (
			 match id2exp eval nat with
			 |	Exp.Numer n -> n
			 |	e -> (
				  let msg = "unexpected exp type for var \""^(CoIdent.to_string id)^"\""^
					         "     numerical exp is expected but get \""^(Exp.to_string e)^"\""
				  in
				  raise (LutErrors.Internal_error ("Glue._lucky_formula_of", msg))
				)
		  )
		|  CoAlgExp.AE_call (id, ops) -> (
			 match (id, ops) with
			 |	("uminus", [o])       -> Exp.Uminus (_lucky_numexp_of o)
			 |	("plus", [o1;o2]) -> Exp.Sum (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("minus", [o1;o2]) -> Exp.Diff (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("times", [o1;o2]) -> Exp.Prod (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("slash", [o1;o2]) -> Exp.Quot (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("mod", [o1;o2]) -> Exp.Mod (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("div", [o1;o2]) -> Exp.Div (_lucky_numexp_of o1, _lucky_numexp_of o2)
			 |	("ite", [o1;o2;o3]) -> Exp.Ite (_lucky_formula_of o1, _lucky_numexp_of o2, _lucky_numexp_of o3)
			 |	("interval_continue", [o1;o2;o3]) ->
				Exp.Icont (_lucky_numexp_of o1, _lucky_numexp_of o2, _lucky_numexp_of o3)
			 |	("interval_stop", [o1;o2;o3]) -> 
				Exp.Istop (_lucky_numexp_of o1, _lucky_numexp_of o2, _lucky_numexp_of o3)
			 |	("gauss_continue", [o1;o2;o3]) -> 
				Exp.Gcont (_lucky_numexp_of o1, _lucky_numexp_of o2, _lucky_numexp_of o3)
			 |	("gauss_stop", [o1;o2;o3]) -> 
				Exp.Gstop (_lucky_numexp_of o1, _lucky_numexp_of o2, _lucky_numexp_of o3)
			 |  _ -> raise ( LutErrors.Internal_error (
				  "Glue.lucky_numexp_of",
				  "Unexpected expression \""^(CoAlgExp.lus_dumps e)^"\""
				))
		  )
		|  CoAlgExp.AE_external_call (id, ei, prof, ops) -> (
			 (* HERE: common/Exp currently only implements 2 kinds of external calls :
				 FFC and IFC, both taking func_call_arg as arg :
				 func_call_arg = string * Ezdl.cfunc * ext_func_type * ext_lib_name * t list
				 ext_func_type = Type.t list
				 ext_lib_name = string
				 *)
			 let args = List.map _lucky_exp_of ops in
			 let atypes = List.map lucky_type_of (CkTypeEff.params_of_prof prof) in
			 let fcargs = (id, ei.CkIdentInfo.ed_sym, atypes, ei.CkIdentInfo.ed_lib_name, args) in
			 let rtype = match (CkTypeEff.res_of_prof prof) with
				| [t] -> lucky_type_of t
				| _ -> assert false
			 in
			 match rtype with
			 | Type.IntT -> Exp.IFC fcargs
			 | Type.FloatT -> Exp.FFC fcargs
			 | Type.BoolT -> raise ( LutErrors.Internal_error (
				  "lucky_num_exp_of",
				  "sorry, bool type in external function not yet implemented"))
			 | _ -> (
				  assert false		
				)
		  )
		| _ -> assert false
	 ) in if eval then ExpEval.simp_num raw_res else raw_res
  ) in
  (**** MAIN CALL of lucky_exp_of *)
  _lucky_exp_of e
)

(*
let lucky_formula_of_list (id2exp:id2exp) (el: CoAlgExp.t list) = (
	(* liste -> and  *)
	let make_and cin e = ( 
Utils.time_C "lucky_formula_of";
		let fe = lucky_formula_of id2exp e in
Utils.time_R "lucky_formula_of";
		match cin with
		|	Exp.True -> fe
		|	_ -> Exp.And (cin, fe)
	) in
	List.fold_left make_and Exp.True el  
)
*)

let lucky_var_of (id2exp: id2exp)(si : Expand.support_info) = (
	let nme = CoIdent.to_string si.Expand.si_ident in
	let ty  = lucky_type_of si.Expand.si_type in
	let mode = match si.Expand.si_nature with
		| Expand.Input  -> Var.Input 
		| Expand.Output -> Var.Output
		| Expand.LocalOut  -> Var.Local
		(* local in (i.e. run results) are treated as input *)
		| Expand.LocalIn  -> Var.Input 
	in
	let res = Var.make "" nme ty mode in
	(* HERE: store it ??? *)
	(* init ? *)
	let res = match si.Expand.si_init with
	|  None -> res
	|  Some e -> Var.set_init res (lucky_exp_of true id2exp e)
	in
	(* range ? *)
	let res = match si.Expand.si_range with
	| None -> res
	| Some (low, high) ->
		let res = Var.set_min res (lucky_exp_of true id2exp low) in
		let res = Var.set_max res (lucky_exp_of true id2exp high) in
		res
	in
	res
)

(* check FULL satisfiability 
	of a formula CONTAINING ONLY CONTROLABLE VARS
	for the time being, just a simplified version of Solver.solve_formula

does not work ...

let check_satisfiablity (f:Exp.formula) = (
	(* builds the bdd ... *)
   let bdd = Formula_to_bdd.f
      Value.OfIdent.empty   (* no input needed *)
      Value.OfIdent.empty   (* no pres needed *) 
      "check_satisfiablity" (* message *)
      0                     (* no verbose *)
      f                     (* THE FORMULA *)
   in
	(*  *)
	if (Bdd.is_false bdd) then false
	else (
		(* search at least one num sol *)
		(* Solver.draw requires the "and"
		   of all bool vars to gen *)
		let sols = Solver.draw
			Value.OfIdent.empty   (* no input needed *)
			Value.OfIdent.empty   (* no pres needed *)
			0                     (* no verbose *)
			"check_satisfiablity" (* message *)
			[]             (* don't care of which vars are outs or locs *)
			(1,0,Thickness.AtMost 0)  (* Thickness.numeric: (inside, edges, vertices) *)
			[]             (* bool_vars_to_gen: Var.name list *)
			[]             (* num_vnt_to_gen: var list *)
			(Bdd.support bdd)  (* comb: Bdd.t *)
			bdd            (* ze bdd to draw in *)
		in
		(sols <> [])
	)
)
*)
