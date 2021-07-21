
(*

Interface avec les modules lucky :

  Type
  Var
  Exp

*)

open LutErrors
open Exp
open Value
open Util

type lucky_var = Exp.var

let dbg = Verbose.get_flag "LutProg"

type t = {
	auto : AutoGen.t;
	(* table : string -> lucky_var for compiling *)
	lucky_var_tab : lucky_var StringMap.t;
	lucky_prevar_tab :  lucky_var StringMap.t;
} 

(* set the pre_tab of a LutProg.t associated to a ... *)

let create a = {
	auto = a;
	lucky_var_tab = StringMap.empty;
	lucky_prevar_tab = StringMap.empty;
}

(* CONVERSION TYPE LUTIN -> TYPE LUCKY (CkTypeEff.t  -> Type.t) *)

let lucky_type_of teff = (
	if (teff = CkTypeEff.boolean) then Type.BoolT
	else if (teff = CkTypeEff.integer) then Type.IntT
	else if (teff = CkTypeEff.real) then  Type.FloatT
	else (raise (LutErrors.Internal_error (
		"LutProg.lucky_type_of",
		"Unexpected type"^(CkTypeEff.to_string teff)
	)))
) 

(* Utiles *)
let lucky_exp_zero = Exp.Numer (Exp.Ival (Num.num_of_int 0))

let lucky_exp_var_ref (x:lucky_var) = (
	match Var.typ x with
	|	Type.BoolT   -> Exp.Formu (Exp.Bvar x)
	|	Type.IntT    -> Exp.Numer (Exp.Ivar x)
	|	Type.FloatT  -> Exp.Numer (Exp.Fvar x)
	|  _ -> assert false
)


(* CONVERSION EXP LUTIN -> EXP LUCKY (CkTypeEff.t  -> Type.t) *)

let lucky_ref_of ?(where="") tab nme = (
	try (
		lucky_exp_var_ref (StringMap.find nme tab)
	) with Not_found -> (
		raise (
			LutErrors.Internal_error (
				"LutProg.lucky_ref_of",
				where^"unexpected var \""^nme^"\""
			)
		)
	)
)

(* translators CoAlgExp to Exp take as argument
   the function in charge of translating ident refs:
	the argument MUST be a id ref (AE_pre, AE_support, AE_alias)
*)
type id2exp = CoAlgExp.node -> Exp.t

(* standard id solver for compilation
*)
let compil_id2exp (this:t) idref = (
	match idref with
	|	CoAlgExp.AE_pre id 
	|	CoAlgExp.AE_support id
	|	CoAlgExp.AE_alias id -> (
		let nme = CoIdent.to_string id in
		let (tab,wh) = match idref with
		| CoAlgExp.AE_pre _ ->
			(this.lucky_prevar_tab, "in lucky_formula_of, searching in prevar_tab, ")
		| _ ->
			(this.lucky_var_tab, "in lucky_formula_of, searching in var_tab, ")
		in
		lucky_ref_of ~where:wh tab nme
	)
	| _ -> assert false
)

(* la table est remplie + bas ... *)

let rec lucky_exp_of (id2exp:id2exp) (e: CoAlgExp.t) = (
	let t = CoAlgExp.get_type e in
	if t = CkTypeEff.boolean then
		Exp.Formu (lucky_formula_of id2exp e)
	else if t = CkTypeEff.integer then 
		Exp.Numer (lucky_numexp_of id2exp e)
	else if t = CkTypeEff.real then
		Exp.Numer (lucky_numexp_of id2exp e)
	else  failwith ("XXX :"^(CkTypeEff.to_string t)^":"^(CoAlgExp.lus_dumps e)^"\n")

) and lucky_formula_of (id2exp:id2exp) (e: CoAlgExp.t) = (
	let nat = e.CoAlgExp.ae_val in
	match nat with
	|	CoAlgExp.AE_true -> Exp.True
	|	CoAlgExp.AE_false -> Exp.False
	|	CoAlgExp.AE_pre id 
	|	CoAlgExp.AE_support id
	|	CoAlgExp.AE_alias id -> (
		match id2exp nat with
		|	Exp.Formu f -> f
		|	e -> (
			let msg = "unexpected exp type for var \""^(CoIdent.to_string id)^"\""^
			          "     bool exp is expected but get \""^(Exp.to_string e)^"\""
			in 
			raise (LutErrors.Internal_error ("LutProg.lucky_formula_of", msg))
		)
	)
	|	CoAlgExp.AE_call (id, ops) -> (
		match (id, ops) with
		|	("not", [o])       -> Exp.Not (lucky_formula_of id2exp o)
		|	("and", [o1;o2]) -> Exp.And (lucky_formula_of id2exp o1, lucky_formula_of id2exp o2)
		|	("or", [o1;o2])  -> Exp.Or (lucky_formula_of id2exp o1, lucky_formula_of id2exp o2)
		|	("xor", [o1;o2]) -> Exp.Xor (lucky_formula_of id2exp o1, lucky_formula_of id2exp o2)
		|	("impl", [o1;o2])   -> Exp.Impl (lucky_formula_of id2exp o1, lucky_formula_of id2exp o2)
		|	("ite", [o1;o2;o3]) -> Exp.IteB (lucky_formula_of id2exp o1, lucky_formula_of id2exp o2, lucky_formula_of id2exp o3)
		|	("lt", [o1;o2])     -> Exp.Inf (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("lte", [o1;o2])  -> Exp.InfEq (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("gt", [o1;o2])   -> Exp.Sup (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("gte", [o1;o2])  -> Exp.SupEq (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("neq", [o1;o2])
		|	("eq", [o1;o2])   -> (
			let ty = CoAlgExp.get_type o1 in
			let eqexp = if (ty = CkTypeEff.boolean ) then
				Exp.EqB (lucky_formula_of id2exp o1, lucky_formula_of id2exp o2)
			else if ((ty = CkTypeEff.integer) || (ty = CkTypeEff.real)) then (
				Exp.Eq (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
			) else (
				assert false
			) in
			if (id = "neq") then Exp.Not eqexp else eqexp
		)
		| _ -> assert false	
	)
	|	CoAlgExp.AE_rconst x ->
				raise (LutErrors.Internal_error ( "lucky_formula_of", "unexpected AE_rconst "^x^" CoAlgExp.t" ))
	|	CoAlgExp.AE_iconst x ->
				raise (LutErrors.Internal_error ( "lucky_formula_of", "unexpected AE_iconst "^x^" CoAlgExp.t" ))
	|	CoAlgExp.AE_rval x ->
				raise (LutErrors.Internal_error ( "lucky_formula_of", "unexpected AE_rval "^(string_of_float x)^" CoAlgExp.t" ))
	|	CoAlgExp.AE_ival x ->
				raise (LutErrors.Internal_error ( "lucky_formula_of", "unexpected AE_ival "^(string_of_int x)^" CoAlgExp.t" ))
	|	CoAlgExp.AE_const x ->
				raise (LutErrors.Internal_error ( "lucky_formula_of", "unexpected AE_const "^x^" CoAlgExp.t" ))
	|  CoAlgExp.AE_external_call (_id, _ei, _prof, _ops) -> (
		 raise ( LutErrors.Internal_error (
			"LutProg.lucky_formula_of",
			"sorry, bool type in external function not yet implemented"))
	)
) and lucky_numexp_of (id2exp:id2exp) (e: CoAlgExp.t) = (
	let nat = e.CoAlgExp.ae_val in
	match nat with
	|	CoAlgExp.AE_iconst s -> Exp.Ival (Num.num_of_string s)
	|	CoAlgExp.AE_rconst s -> Exp.Fval (float_of_string s)
	|	CoAlgExp.AE_ival i -> Exp.Ival (Num.num_of_int i)
	|	CoAlgExp.AE_rval r -> Exp.Fval r
	|	CoAlgExp.AE_pre id 
	|	CoAlgExp.AE_support id
	|	CoAlgExp.AE_alias id -> (
		match id2exp nat with
		|	Exp.Numer n -> n
		|	e -> (
				let msg = "unexpected exp type for var \""^(CoIdent.to_string id)^"\""^
				          "     numerical exp is expected but get \""^(Exp.to_string e)^"\""
				in 
				raise (LutErrors.Internal_error ("LutProg.lucky_formula_of", msg))
			)
		)
	|  CoAlgExp.AE_call (id, ops) -> (
		match (id, ops) with
		|	("uminus", [o])       -> Exp.Uminus (lucky_numexp_of id2exp o)
		|	("plus", [o1;o2]) -> Exp.Sum (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("minus", [o1;o2]) -> Exp.Diff (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("times", [o1;o2]) -> Exp.Prod (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("slash", [o1;o2]) -> Exp.Quot (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("mod", [o1;o2]) -> Exp.Mod (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("div", [o1;o2]) -> Exp.Div (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2)
		|	("ite", [o1;o2;o3]) -> Exp.Ite (lucky_formula_of id2exp o1, lucky_numexp_of id2exp o2, lucky_numexp_of id2exp o3)
		|	("interval_continue", [o1;o2;o3]) ->
				Exp.Icont (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2, lucky_numexp_of id2exp o3)
		|	("interval_stop", [o1;o2;o3]) -> 
				Exp.Istop (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2, lucky_numexp_of id2exp o3)
		|	("gauss_continue", [o1;o2;o3]) -> 
				Exp.Gcont (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2, lucky_numexp_of id2exp o3)
		|	("gauss_stop", [o1;o2;o3]) -> 
				Exp.Gstop (lucky_numexp_of id2exp o1, lucky_numexp_of id2exp o2, lucky_numexp_of id2exp o3)
		|  _ -> raise ( LutErrors.Internal_error (
				"LutProg.lucky_numexp_of",
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
		let args = List.map (lucky_exp_of id2exp) ops in
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
(*
*)
	| _ -> assert false
)

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

let add_pre (it:t) (x:lucky_var) = (
	let nme = Var.name x in
	(* Verbose.put ~flag:dbg "  LutProg.add_pre \"%s\"\n" nme; *)
	try (
		StringMap.find nme it.lucky_prevar_tab, it
	) with Not_found -> (
		let prex = Var.make_pre x in
		let it = { it with lucky_prevar_tab = StringMap.add nme prex it.lucky_prevar_tab } in
		prex, it
	)
)

let lucky_make_var it _mnode nme ty mode range_opt = (
	(* let res = Var.make mnode nme ty mode in *)
	(* pour pas avoir le nom du neaoud en prefixe *)
	let id2exp = compil_id2exp it in
	let res = Var.make "" nme ty mode in
   match ty with
   |  Type.BoolT   -> res 
   |  Type.IntT    -> 
        (match range_opt with
           | None -> res
           | Some (low, high) -> 
               let res = Var.set_min res (lucky_exp_of id2exp low) in
               let res = Var.set_max res (lucky_exp_of id2exp high) in
                 res
	)
   |  Type.FloatT  ->
        (match range_opt with
           | None -> res
           | Some (low, high) -> 
               let res = Var.set_min res (lucky_exp_of id2exp low) in
               let res = Var.set_max res (lucky_exp_of id2exp high) in
                 res
	)
   |  Type.UT (Type.ArrayT _) -> assert false (* XXX finish me! *)
   |  Type.UT (Type.StructT _) -> assert false (* XXX finish me! *)
   |  Type.UT (Type.EnumT _) -> assert false (* XXX finish me! *)
)

let init_vars (it: t) = (
  let auto = it.auto in
  let source_code = AutoGen.source auto in
  let mnode = Expand.node_name source_code in
  (***********************************************************)
  (*************** FONCTIONS EXTERNES ************************)
  (***********************************************************)
  (* ICI -> A FAIRE ??
	  let etab2prof s xi acc = (
	  (s, xi.xi_prof)::acc
	  ) in
	  let xlist = Hashtbl.fold etab2prof 
	  (Expand.extern_tab source_code) [] in
	  if (xlist = []) then ()
	  else (
	  fprintf os "\nfunctions {\n";
	  let pxd (s, p)	= (
	  fprintf os "   %s : %s;\n" s (CkTypeEff.prof_to_string p)
	  ) in
	  List.iter pxd xlist;
	  fprintf os "}\n";
	  );
  *)

  (***********************************************************)
  (*************** VARIABLES *********************************)
  (***********************************************************)
  let add_support mode it id = (
	 let nme = CoIdent.to_string id in
	 let info = Util.StringMap.find id (Expand.support_tab source_code) in
	 (* Verbose.put ~flag:dbg "  LutProg.add_support \"%s\"\n" nme; *)
	 let res = lucky_make_var it mnode nme (lucky_type_of info.Expand.si_type) mode info.Expand.si_range in
	 (* init ? *)
	 let res = match info.Expand.si_init with
		|	None -> res
		|	Some e -> Var.set_init res (lucky_exp_of (compil_id2exp it) e)
	 in	
	 (* default ? *)
	 let res = match info.Expand.si_default with
		|	None -> res
		|	Some e -> Var.set_default res (lucky_exp_of (compil_id2exp it) e)
	 in	
	 (* le pre est utilisé qq part <=> le champ si_pre_ref_exp est renseigné *)
	 let _ = match info.Expand.si_pre_ref_exp with
		|	Some _e -> let _, it = add_pre it res in it
		|	None -> it
	 in	
	 { it with lucky_var_tab = StringMap.add nme res it.lucky_var_tab }
  ) in

  let add_alias it id = (
	 let nme = CoIdent.to_string id in
	 Verbose.put ~flag:dbg "  LutProg.add_alias \"%s\"\n" nme;
	 let info = Util.StringMap.find id (Expand.alias_tab source_code) in
	 (* les alias sont des Local spéciaux en lucky *)
	 let res = Var.set_alias
		  (lucky_make_var it mnode nme (lucky_type_of info.Expand.ai_type) Var.Local None)
		  (lucky_exp_of (compil_id2exp it) info.Expand.ai_def_exp)
	 in
	 { it with lucky_var_tab = StringMap.add nme res it.lucky_var_tab }
  ) in

  (* Loop counters are special local vars.
	  "i" is the (unique) index of an expression loopi/loopa
	  N.B. i in [0 , CoTraceExp.nb_loops ()[
	  *)
  let add_counter it (i : int) = (
	 let id = CoIdent.of_cpt i in
	 let nme = CoIdent.to_string id in
	 Verbose.put ~flag:dbg "  LutProg.add_counter \"%s\"\n" nme;
    let x = lucky_make_var it mnode nme (lucky_type_of CkTypeEff.integer)
        Var.Local None in
	 let x = Var.set_init x lucky_exp_zero in
	 (* par defaut : pre cpt *)
	 let px, it = add_pre it x in
	 let x = Var.set_default x (lucky_exp_var_ref px) in
	 { it with lucky_var_tab = StringMap.add nme x it.lucky_var_tab }
  ) in

  (* ENTRÉES *)
  let it =
    List.fold_left (add_support Var.Input) it (Expand.input_list source_code)
  in

  (* SORTIES *)
  let it =
    List.fold_left (add_support Var.Output) it (Expand.output_list source_code);
  in
  (* LOCALES support *)
  let it =
    List.fold_left (add_support Var.Local) it (Expand.local_out_list source_code);
  in
  (* ALIAS (de source_code) *)
  let it =
    List.fold_left add_alias it (Expand.alias_list source_code);
  in
  (* Loop counters :
	  2010/11 -> the creation of loop counters is no longer
	  make in AutoExlore, since it is NOT compatible with the
	  lazy exploration.
	  Now -> counters are created A PRIORI, one for each loopa/loopi
	  created during Expand. 
	  *)
  (* List.iter add_counter (AutoGen.counters auto); *)
  let it_ref = ref it in
  for i = 0 to (CoTraceExp.nb_loops () - 1) do
	 it_ref := add_counter !it_ref i
  done;
  !it_ref
)

(*

EXPLORATION D'UNE COUCHE :
- nouvelle structure AutoGen.gtree, plus pratique à traduire 
  N.B. le nommage des noeuds est garanti unique

type gtree = string * gtree_node
and gtree_node =
|  GT_leaf of (CoAlgExp.t list * string)
|  GT_choice of (weightexp option * gtree) list

-----------------------
type wt = children Util.StringMap.t * string  (* the top-level node of the wt *)

and children =
  | Children of (dyn_weight * string) list
  | Leave of Exp.formula * atomic_ctrl_state
  | Stop of string

*)

(*
WARNING :
	- "ctrlst" is the same as with "st.Prog.d.Prog.ctrl_state"
	- input = current input map
   - st.Prog.d.Prog.input  = previous values of input (indexed by "ident") 
     (useless, see below)
   - st.Prog.d.Prog.memory = previous values of ALL vars (indexed by "pre ident")
	  including pre's of inputs
	==> see Prog.memory_of_state	

N.B. in simu mode, pre's are not handled by lucky but zelut !
 
*)

(*
   extract the necessary pre's 
   from a triple of current input, out and loc, 
*)
let make_pre_env (zelut:t) ins outs locs =
   (* DON'T FAIL here ! will fail in eval if pre's are actually used
   let error what who = raise (
      Internal_error ("AutoGen.make_pre_env",
         Printf.sprintf "can't find previous value of %s \"%s\"" what who
      )
   ) in
   *)
  let lvt = zelut.lucky_var_tab in
  let dopre nme _lucvar acc =
		Verbose.put ~flag:dbg "%% make_pre_ena/dopre \"%s\"" nme;
      try (
			let zevar = StringMap.find nme lvt in
         let tab = match (Var.mode zevar)  with
			| Var.Input -> ins
			| Var.Output -> outs
			| Var.Local -> locs
			| Var.Pre -> assert false
			in
         let value = Value.OfIdent.get tab nme in
         Value.OfIdent.add acc (nme,value)
      ) with Not_found -> acc
  in
  let pre_values = StringMap.fold dopre zelut.lucky_prevar_tab Value.OfIdent.empty in
  pre_values

let lut_get_wtl (zelut:t) (input:Var.env_in) (st:Prog.state) (ctrlst:Prog.ctrl_state) = (

  let zesrc = match ctrlst with
	 |	[cs] -> cs
	 |	_ -> assert false
  in

  let (li, lo, ll) = Prog.last_values_of_state st in
  let zepres = make_pre_env zelut li lo ll in
	 (* let zecfg = AutoGen.make_config (Some (input, zepres)) zesrc in *)
  let zecfg = AutoGen.make_config zesrc in

    Verbose.exe ~flag:dbg 
      (fun _ ->
	     let memory = Prog.memory_of_state st in
	     Verbose.put ~flag:dbg "++lut_get_wtl input = %s"  (Value.OfIdent.to_string "" input);
	     Verbose.put ~flag:dbg "++lut_get_wtl last_input = %s"  (Value.OfIdent.to_string "" li);
	     Verbose.put ~flag:dbg "++lut_get_wtl last_output = %s"  (Value.OfIdent.to_string "" lo);
	     Verbose.put ~flag:dbg "++lut_get_wtl last_local = %s"  (Value.OfIdent.to_string "" ll);
	     Verbose.put ~flag:dbg "++lut_get_wtl memory = %s" (Value.OfIdent.to_string "" memory);
	     Verbose.put ~flag:dbg "++lut_get_wtl pre's = %s"  (Value.OfIdent.to_string "" zepres);
      );

	 (* Appele AutoGen.state2gtree -> AutoGen.gtree *)
    Verbose.exe ~level:2 (fun () -> Verbose.put "# -> state2gtree\n");
    Utils.time_C "state2gtree";

    let gt, auto = AutoGen.config2gtree zelut.auto zecfg in
	 let zelut = { zelut with auto = auto } in
      Utils.time_R "state2gtree";
      Verbose.exe ~level:2 (
	     fun () -> Verbose.put "# <- state2gtree, done: %d nodes\n"
          (AutoGen.gtree_size gt)
      );
	   (* traduction gtree -> Prog.wt *)

	   let ze_wt = ref Util.StringMap.empty in 

	   let rec treat_gtree (s,gt) = (
		  match gt with
		    |	AutoGen.GT_stop msg -> (
			     ze_wt := Util.StringMap.add s (Prog.Stop msg) !ze_wt
		      ) 
		    |	AutoGen.GT_leaf (grd, dest_nme) -> (
			     (* bizarement, la formule est attachée a la dest ? *)
			     (* let src2dest = Prog.Children [ (Prog.Infin, dest_nme) ] in *)
			     (* ze_wt := Util.StringMap.add s src2dest !ze_wt ; *)
			     let fl = Guard.to_exp_list grd in
                Utils.time_C "treat_gtree (formula)";
                Verbose.exe ~flag:dbg (
	               fun () -> (
		              let sz = List.fold_left (fun acc e -> acc + (CoAlgExp.sizeof e)) 0 fl in
		                Printf.fprintf stderr "lucky_formula_of_list: size =%d\n" sz
	               )
                );
			       let lform = Prog.Leave (lucky_formula_of_list (compil_id2exp zelut) fl, dest_nme) in
                  Utils.time_R "treat_gtree (formula)";
			         ze_wt := Util.StringMap.add s lform !ze_wt
		      )
		    |	AutoGen.GT_choice chl -> (
			     let treat_ch (wo, (dest_nme, dest_def)) = (
				    (* (compute_weight : Exp.weight -> Var.env_in -> state -> dyn_weight) *)
				    let dw = match wo with
				      |	None -> Prog.V 1 (* 1 par défaut *)
				      |	Some AutoGen.W_huge ->  Prog.Infin
				      |	Some AutoGen.W_exp e -> (
                       (* Verbose.put ~flag:dbg "--TREAT WEIGHT EXP: %s\n" (CoAlgExp.lus_dumps e); *)
					        let we = lucky_numexp_of (compil_id2exp zelut)  e in
                         Utils.time_C "treat_gtree (weight)";
                         let res = match we with
                           | Exp.Ival i -> Prog.V (Util.int_of_num i)
                           | _ -> Prog.compute_weight (Exp.Wexpr we) input st
                         in
					            (* let res = Prog.compute_weight (Exp.Wexpr we) input st in *)
                           Utils.time_R "treat_gtree (weight)";
					            res
				         ) in
				      (* traitement récursif *)
				      treat_gtree (dest_nme, dest_def);
				      (dw, dest_nme)
			     ) in
			     let v = Prog.Children (List.map treat_ch chl) in
			       ze_wt := Util.StringMap.add s v !ze_wt
		      )
	   ) in
      
      Verbose.exe ~level:2 (fun () -> Verbose.put "# -> treat_gtree\n");
      Utils.time_C "treat_gtree";
	   let _ = treat_gtree gt in
      Utils.time_R "treat_gtree";
      Verbose.exe ~level:2 (fun () -> Verbose.put "# <- treat_gtree, done\n");
      
	   Verbose.exe ~level:3 ( fun () ->
		  Printf.printf "lut_get_wtl ->\n";
		  Prog.print_wt (!ze_wt, zesrc);
	   );     
	   [ (!ze_wt, zesrc) ]
)

let make ?(libs: string list option = None) infile mnode = (
  try (
    let _ = 
      print_string "*** WARNING! obselete algo! cf LutExe.\n";
      flush stdout
    in
    let mainprg = assert (infile <> []); Parsers.read_lut infile in
    let tlenv = CheckType.check_pack libs mainprg in
    let mnode = if mnode <> "" then mnode else
      let all_nodes = 
        Hashtbl.fold
          (fun n _ acc -> n::acc)
          mainprg.Syntaxe.pck_nodetab
          []
      in
        (* It is not necessary to build to complete list to take the first
           one, but I'm sure that list will be useful in the future... R1.*)
      let mnode = 
        if all_nodes = [] then
          (* shouldn't that be checked before? *)
          raise (LutErrors.Global_error ((String.concat "," infile)^ 
                                        " contains no node"))
        else
          List.hd all_nodes 
      in
        Verbose.put ~level:1 "# No node is specified: will use %s \n" mnode;
        mnode
    in
    let exped = Expand.make tlenv mainprg mnode in
      (* Les tables de variables decoulent du exped  *)
      Verbose.put ~level:3 "#---begin AutoGen.make\n";

      (* STUPID ! let auto = AutoGen.make exped in *)
      (* HERE : PROBLEM, COUNTER ARE NO LONGER DISCOVERED ??? *)
      let auto = AutoGen.init exped in

	     Verbose.put ~level:3 "#---end AutoGen.make\n";
	     let zelut = create auto in
	     let zelut = init_vars zelut in
	     let id2var (id: CoIdent.t) =
	       let nme = CoIdent.to_string id in
	         StringMap.find nme zelut.lucky_var_tab
	     in
	     let sort_bool_num _k v (blin, nlin) =
          (* Verbose.exe ~level:3 (fun () -> Printf.fprintf stderr "sort_bool_num %s=%s\n" k (Var.to_string v)); *)
	       match Var.mode v with
	         |	Var.Output
	         |	Var.Local -> (
		           if (Var.alias v = None) then (
		             if (Var.typ v = Type.BoolT)
		             then (v::blin, nlin)
		             else (blin, v::nlin)
		           ) else (blin, nlin)
		         )
	         |	_ -> (blin, nlin)		
	     in
	     let (bl,nl) = StringMap.fold sort_bool_num zelut.lucky_var_tab ([],[]) in
	       (* let get_all_mems n ve a = (n,ve)::a in *)
	       (* let get_all_mems n ve a = (Prevar.get_pre_var_name n, ve)::a in *)
	     let get_all_mems n ve a = (Var.name ve, StringMap.find n zelut.lucky_var_tab)::a in
	       (* la fonction qui dit si c'est final *)
	     let is_final s =
	       match AutoGen.get_state_info zelut.auto s with
	         |	AutoGen.SS_final _ -> true
	         |	_ -> false
	     in
	       (* returns a t AND a Prog.t *)
	       (zelut,
	        {
	          Prog.initial_ctrl_state = [[AutoGen.init_control auto]];
	          Prog.in_vars = List.map id2var (Expand.input_list exped);
	          Prog.out_vars = List.map id2var (Expand.output_list exped);
	          Prog.loc_vars = List.map id2var (Expand.local_out_list exped);
	          Prog.ext_func_tbl = Util.StringMap.empty;
	          Prog.memories_names = StringMap.fold get_all_mems zelut.lucky_prevar_tab [];
	          Prog.bool_vars_to_gen = [bl];
	          Prog.num_vars_to_gen = [nl];
	          Prog.output_var_names = [Expand.output_list exped];
	          Prog.reactive = false;
	          Prog.get_wtl = lut_get_wtl zelut;
	          Prog.is_final = List.for_all (List.for_all is_final) ;
	          Prog.gen_dot = (fun _ _ _ -> assert false);
	        }
	       )
  ) with 
      Sys_error(s) -> 
	     prerr_string (s^"\n") ; exit 1
    | LutErrors.Global_error s ->  
        LutErrors.print_global_error s ; exit 1
	 | Parsing.Parse_error ->
        LutErrors.print_compile_error (Lexeme.last_made ()) "syntax error"; exit 1
    | LutErrors.Compile_error(lxm,msg) ->
        LutErrors.print_compile_error lxm msg ; exit 1
    | LutErrors.Internal_error (fname,msg) ->
		  LutErrors.print_internal_error fname msg ; exit 1
)

let get_init_state ?(verb_level=0) _zelutprog zeprog = (
  let get_init_vals accin lucvar = (
	 let nme = Var.name lucvar in
	 match Var.init lucvar with
	 |	None -> accin
	 |	Some e -> (
		  let v = match e with
			 | Formu True -> B true
			 | Formu False -> B false
			 | Numer (Ival i) -> N (I i)
			 | Numer (Fval r) -> N (F r)
			 | Numer (Uminus (Ival i)) -> N (I (Num.minus_num i))
	       | Numer (Uminus (Fval r)) -> N (F (-.r))
	       | _ -> raise (Internal_error
                          ("LutProg.get_init_state",
				               ("initial value of \""^nme^"\" must be a constant expression"
                            ^" (but get "^(Exp.to_string e)^")"
			                  )
			                 ))
        in
		  Value.OfIdent.add accin (nme, v)	
		)
  ) in
  {
	 Prog.s = zeprog;
	 Prog.d = {
		Prog.memory = Value.OfIdent.empty;
		Prog.ctrl_state = zeprog.Prog.initial_ctrl_state;
		(* Prog.last_input = Value.OfIdent.empty; *)
		(* Prog.last_output = Value.OfIdent.empty; *)
		(* Prog.last_local = Value.OfIdent.empty; *)
	   Prog.last_input =
        List.fold_left get_init_vals Value.OfIdent.empty (zeprog.Prog.in_vars);
      Prog.last_output =
        List.fold_left get_init_vals Value.OfIdent.empty (zeprog.Prog.out_vars);
      Prog.last_local =
        List.fold_left get_init_vals Value.OfIdent.empty (zeprog.Prog.loc_vars);
      Prog.snt = Solver.init();
		Prog.verbose = verb_level
	 }
  }
)
(* renvoie le state initial facon Prog *)
let make_state  ?(libs: string list option = None) ?(verb_level=0) infile mnode = (
	let (zelutprog, zeprog) = make ~libs:libs infile mnode in
	get_init_state ~verb_level:verb_level zelutprog zeprog
)
