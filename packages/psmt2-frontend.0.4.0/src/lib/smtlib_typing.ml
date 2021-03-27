open Options
open Smtlib_error
open Smtlib_syntax
open Smtlib_typed_env

(******************************************************************************)
let inst_and_unify (_env,locals) m a b pos =
  let _m, a = Smtlib_ty.inst locals m a in
  Smtlib_ty.unify a b pos

let find_par_ty (env,locals) symb pars args =
  try
    let res = SMap.find symb.c locals in
    symb.is_quantif <- true;
    res
  with Not_found -> try
      find_fun (env,locals) symb pars args false
    with Not_found ->
      let s_symb =
        (List.fold_left (fun acc arg ->
             Printf.sprintf "%s %s" acc arg
           ) symb.c args) in
      try
        find_fun (env,locals) {symb with c = s_symb} pars [] false
      with Not_found ->
        error (Typing_error ("Undefined fun : " ^ symb.c)) symb.p

let find_pattern (env,locals) symb pars args all_type =
  try SMap.find symb.c locals, locals
  with Not_found -> try
      find_fun (env,locals) symb pars args all_type, locals
    with Not_found ->
      let dum = Smtlib_ty.new_type (Smtlib_ty.TDummy) in
      dum, SMap.add symb.c dum locals

let check_if_dummy t l =
  if Smtlib_ty.is_dummy t.ty then
    t :: l
  else
    l

let check_if_escaped l =
  List.iter (fun d ->
      if Smtlib_ty.is_dummy d.ty then begin
        error (Typing_error ("Escaped type variables")) d.p;
      end;
    ) l

let type_cst c _pos=
  match c with
  | Const_Dec (_s) -> Smtlib_ty.new_type Smtlib_ty.TReal
  | Const_Num (_s) ->
    Smtlib_ty.new_type
      (if get_is_real () then Smtlib_ty.TReal else Smtlib_ty.TInt)
  | Const_Str (_s) -> Smtlib_ty.new_type Smtlib_ty.TString
  | Const_Hex (_s) ->
    Smtlib_ty.new_type
      (if get_is_fp () then Smtlib_ty.TBitVec(0)
       else if get_is_real () then Smtlib_ty.TReal
       else Smtlib_ty.TInt)
  | Const_Bin (_s) ->
    Smtlib_ty.new_type
      (if get_is_fp () then Smtlib_ty.TBitVec(0)
       else if get_is_real () then Smtlib_ty.TReal
       else Smtlib_ty.TInt)

let type_qualidentifier (env,locals) q pars =
  match q.c with
  | QualIdentifierId (id) ->
    let symb,idl = get_identifier id in
    let ty = find_par_ty (env,locals) symb pars idl in
    inst_and_unify (env,locals) Smtlib_ty.IMap.empty ty q.ty q.p;
    ty
  | QualIdentifierAs (id, sort) ->
    let symb,idl = get_identifier id in
    let ty = find_par_ty (env,locals) symb pars idl in
    let ty_sort = find_sort (env,locals) sort in
    inst_and_unify (env,locals) Smtlib_ty.IMap.empty ty ty_sort symb.p;
    Smtlib_ty.unify sort.ty ty_sort sort.p;
    Smtlib_ty.unify q.ty ty q.p;
    ty

let rec type_match_case (env,locals,dums,constrs) ty_match (pattern,term) cstrs=
  match pattern.c with
  | MatchUnderscore ->
    let ty,dums = type_term (env,locals,dums) term in
    ty, dums, SMap.empty
  | MatchPattern (constr,args) ->
    match args with
    | [] ->
      if SMap.mem constr.c cstrs then
        let ty,dums = type_term (env,locals,dums) term in
        ty, dums, SMap.remove constr.c constrs
      else
        let ty, locals = find_pattern (env,locals) constr [] [] false in
        if Smtlib_ty.is_dummy ty then
          let ty,dums = type_term (env,locals,dums) term in
          ty, dums, SMap.empty
        else begin
          inst_and_unify (env,locals) Smtlib_ty.IMap.empty ty_match ty constr.p;
          assert false;
          (* ty, dums, SMap.empty *)
        end
    | _ ->
      let locals,args = List.fold_left (fun (locals,pars) par ->
          let ty = (Smtlib_ty.new_type (Smtlib_ty.TDummy)) in
          SMap.add par.c ty locals, ty :: pars
        ) (locals,[]) (List.rev args) in
      let ty_constr,locals = find_pattern (env,locals) constr args [] true in
      if Smtlib_ty.is_dummy ty_constr then
        error (Typing_error
                 (Printf.sprintf "Undefined Constructor %s" constr.c)) term.p;
      let ty = Smtlib_ty.new_type (Smtlib_ty.TFun (args,ty_match)) in
      inst_and_unify (env,locals) Smtlib_ty.IMap.empty ty_constr ty constr.p;
      let ty,dums = type_term (env,locals,dums) term in
      ty, dums, SMap.remove constr.c constrs

and type_key_term (env,locals,dums) key_term =
  match key_term.c with
  | Pattern(term_list) ->
    List.fold_left (fun dums t ->
        let _,dums = type_term (env,locals,dums) t in
        dums
      ) [] term_list
  | Named(_symb) ->
    if Options.verbose () > 0 then
      Printf.eprintf ";[Warning] (! :named not yet supported)\n%!";
    dums

and type_term (env,locals,dums) t =
  match t.c with
  | TermSpecConst (cst) ->
    Smtlib_ty.unify t.ty (type_cst cst t.p) t.p;
    t.ty, dums

  | TermQualIdentifier (qualid) ->
    let ty_q = type_qualidentifier (env,locals) qualid [] in
    Smtlib_ty.unify t.ty ty_q t.p;
    t.ty, check_if_dummy t dums

  | TermQualIdTerm (qualid,term_list) ->
    let pars,dums =
      List.fold_left (fun (pars,dums) t ->
          let ty, dums = type_term (env,locals,dums) t in
          ty :: pars, dums
        ) ([],dums) term_list in
    let pars = List.rev pars in
    let q = (type_qualidentifier (env,locals) qualid pars) in
    Smtlib_ty.unify t.ty q t.p;
    t.ty, check_if_dummy t dums

  | TermLetTerm (varbinding_list,term) ->
    let locals,dums = List.fold_left (fun (locals,dums) (symb,term) ->
        let ty, dums = type_term (env,locals,dums) term in
        SMap.add symb.c ty locals, dums
      ) (locals,dums) varbinding_list in
    let ty,dums = type_term (env,locals,dums) term in
    Smtlib_ty.unify t.ty ty t.p;
    t.ty, dums

  | TermForAllTerm (sorted_var_list, term) ->
    let locals = List.fold_left (fun locals (symb,sort) ->
        SMap.add symb.c (find_sort (env,locals) sort) locals
      ) locals sorted_var_list in
    let ty,dums = type_term (env,locals,dums) term in
    Smtlib_ty.unify t.ty ty t.p;
    t.ty, dums

  | TermExistsTerm (sorted_var_list, term) ->
    let locals = List.fold_left (fun locals (symb,sort) ->
        SMap.add symb.c (find_sort (env,locals) sort) locals
      ) locals sorted_var_list in
    let ty,dums = type_term (env,locals,dums) term in
    Smtlib_ty.unify t.ty ty t.p;
    t.ty, dums

  | TermExclimationPt (term, key_term_list) ->
    let dums = List.fold_left (fun dums kt ->
        type_key_term (env,locals,dums) kt
      ) dums key_term_list in
    let ty,dums = type_term (env,locals,dums) term in
    ty, dums

  | TermMatch (term, match_case_list) ->
    let ty,dums = type_term (env,locals,dums) term in
    (* check if term is datatype *)
    Smtlib_ty.unify (Smtlib_ty.new_type (Smtlib_ty.TDatatype("",[]))) ty term.p;
    let dt_name = Smtlib_ty.get_dt_name ty in
    let constrs = try SMap.find dt_name env.constructors
      with _ ->
        error
          (Typing_error
             (Printf.sprintf "No constructors found for datatype %s\n%!"
                dt_name)) term.p in
    let cstrs = constrs in
    let res,dums,constrs = List.fold_left (fun (res,dums,constrs) mc ->
        let ty_mc, dums, constrs =
          type_match_case (env,locals,dums,constrs) ty mc cstrs in
        Smtlib_ty.unify res ty_mc term.p;
        res,dums,constrs
      ) (Smtlib_ty.new_type (Smtlib_ty.TDummy),dums,constrs) match_case_list in
    if not (SMap.is_empty constrs) then
      error (Typing_error "non-exhaustive pattern matching") term.p;
    Smtlib_ty.unify res t.ty term.p;
    res,dums

let get_term (env,locals) pars term =
  let locals = Smtlib_typed_env.extract_pars locals pars in
  let ty,dums = type_term (env,locals,[]) term in
  check_if_escaped dums;
  ty

let get_sorted_locals (env,locals) params =
  List.fold_left (fun locals (symb,sort) ->
      SMap.add symb.c (Smtlib_typed_env.find_sort (env,locals) sort) locals
    ) locals (List.rev params)

let get_fun_def_locals (env,locals) (name,pars,params,return) =
  let locals = Smtlib_typed_env.extract_pars locals pars in
  let locals = get_sorted_locals (env,locals) params in
  let ret = (Smtlib_typed_env.find_sort (env,locals) return) in
  let params = List.map (fun (_,sort) -> sort) params in
  locals, ret, (name,params,return)

let assertion_stack = Stack.create ()

(******************************************************************************)
(************************************ Commands ********************************)
let type_command (env,locals) c =
  match c.c with
  | Cmd_Assert(dec) | Cmd_CheckEntailment(dec) ->
    let pars,t = dec in
    Smtlib_ty.unify
      (Smtlib_ty.new_type Smtlib_ty.TBool) (get_term (env,locals) pars t) t.p;
    env

  | Cmd_CheckAllSat tl ->
      let pars = [] in
      let idl = [] in
      List.iter (fun symb ->
          let ty = find_par_ty (env,locals) symb pars idl in
          Smtlib_ty.unify (Smtlib_ty.new_type Smtlib_ty.TBool) ty symb.p
        ) tl;
      env

  | Cmd_Minimize t | Cmd_Maximize t ->
      let t' = get_term (env,locals) [] t in
      begin
        (* try to typecheck it as an Int, then as a real if it fails *)
        try Smtlib_ty.unify (Smtlib_ty.new_type Smtlib_ty.TInt) t' t.p
        with _ -> Smtlib_ty.unify (Smtlib_ty.new_type Smtlib_ty.TReal) t' t.p
      end;
      env

  | Cmd_CheckSat -> env
  | Cmd_CheckSatAssum _prop_lit ->
    Options.check_command "check-sat-assuming";
    env
  | Cmd_DeclareConst (symbol,(pars,sort)) ->
    Smtlib_typed_env.mk_const (env,locals) (symbol,pars,sort)
  | Cmd_DeclareDataType (symbol,(pars,datatype_dec)) ->
    Smtlib_typed_env.mk_datatype (env,locals) symbol pars datatype_dec
  | Cmd_DeclareDataTypes (sort_dec_list, datatype_dec_list) ->
    Smtlib_typed_env.mk_datatypes (env,locals) sort_dec_list datatype_dec_list
  | Cmd_DeclareFun (name,fun_dec) ->
    Smtlib_typed_env.mk_fun_dec (env,locals) (name,fun_dec)
  | Cmd_DeclareSort (symbol,arit) ->
    Smtlib_typed_env.mk_sort_decl (env,locals) symbol arit false
  | Cmd_DefineFun (fun_def,term) ->
    let locals,ret,fun_dec = get_fun_def_locals (env,locals) fun_def in
    let ty,dums = type_term (env,locals,[]) term in
    check_if_escaped dums;
    let env = Smtlib_typed_env.mk_fun_def (env,locals) fun_dec in
    inst_and_unify (env,locals) Smtlib_ty.IMap.empty ret ty term.p;
    env
  | Cmd_DefineFunRec (fun_def,term) ->
    let locals,ret,fun_dec = get_fun_def_locals (env,locals) fun_def in
    let env = Smtlib_typed_env.mk_fun_def (env,locals) fun_dec in
    let ty,dums = type_term (env,locals,[]) term in
    check_if_escaped dums;
    inst_and_unify (env,locals) Smtlib_ty.IMap.empty ret ty term.p;
    env
  | Cmd_DefineFunsRec (fun_def_list, term_list) ->
    let env,locals_term_list =
      List.fold_left (fun (env,locals_term_list) fun_def ->
          let locals,ret,fun_dec = get_fun_def_locals (env,locals) fun_def in
          let env = Smtlib_typed_env.mk_fun_def (env,locals) fun_dec in
          env, (locals,ret) :: locals_term_list
        ) (env,[]) (List.rev fun_def_list)
    in
    List.iter2 (fun (locals,ret) term ->
        let ty,dums = type_term (env,locals,[]) term in
        check_if_escaped dums;
        inst_and_unify (env,locals) Smtlib_ty.IMap.empty ret ty term.p;
      ) locals_term_list term_list;
    env
  | Cmd_DefineSort (symbol, symbol_list, sort) ->
    Smtlib_typed_env.mk_sort_def (env,locals) symbol symbol_list sort
  | Cmd_Echo (_attribute_value) -> Options.check_command "echo"; env
  | Cmd_GetAssert -> Options.check_command "get-assertions"; env
  | Cmd_GetProof -> Options.check_command "get-proof"; env
  | Cmd_GetUnsatCore -> Options.check_command "get-unsat-core"; env
  | Cmd_GetValue (_term_list) -> Options.check_command "get-value"; env
  | Cmd_GetAssign -> Options.check_command "get-assignement"; env
  | Cmd_GetOption (_keyword) -> Options.check_command "get-option"; env
  | Cmd_GetInfo (_key_info) -> Options.check_command "get-info"; env
  | Cmd_GetModel -> Options.check_command "get-model"; env
  | Cmd_GetUnsatAssumptions -> Options.check_command "get-unsat-core"; env
  | Cmd_Reset -> Options.check_command "reset"; env
  | Cmd_ResetAssert -> Options.check_command "reset-assertions"; env
  | Cmd_SetLogic(symb) -> Smtlib_typed_logic.set_logic env symb
  | Cmd_SetOption (_option) -> Options.check_command "set-option"; env
  | Cmd_SetInfo (_attribute) -> Options.check_command "set-info"; env
  | Cmd_Push n -> begin
      try
        let n = int_of_string n in
        for _i = 0 to (n - 1 ) do
          Stack.push env assertion_stack
        done;
        env
      with _ ->
        error (Incremental_error ("Push argument must be an integer")) c.p
    end
  | Cmd_Pop n -> begin
      let env = ref env in
      try
        let n = int_of_string n in
        for _i = 0 to (n -1) do
          env := Stack.pop assertion_stack
        done;
        !env
      with
      | Stack.Empty ->
        error (Incremental_error ("Too many pop command")) c.p
      | _ ->
        error (Incremental_error ("Pop argument must be an integer")) c.p
    end
  | Cmd_Exit -> env


let typing parsed_ast =
  let env =
    if not (get_logic ()) then
      try
        let c = List.hd parsed_ast in
        Smtlib_typed_logic.set_logic
          (Smtlib_typed_env.empty ()) {c with c="ALL"}
      with _ -> assert false
    else Smtlib_typed_env.empty ()
  in
  let env =
    List.fold_left (fun env c ->
        let env = type_command (env,SMap.empty)  c in
        if Options.verbose () > 0 then Smtlib_printer.print_command c;
        env
      ) env parsed_ast
  in if Options.verbose () > 1 then begin
    Smtlib_printer.print_env env;
  end
