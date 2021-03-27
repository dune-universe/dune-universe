open Smtlib_syntax
open Smtlib_typed_env
open Smtlib_ty
open Printf

let print_ty = false
let fmt = stderr

let print_list f l =
  (List.fold_left (fun acc a ->
       sprintf "%s %s" acc (f a)
     ) "" l)

let print_constant cst =
  match cst with
  | Const_Dec s | Const_Num s | Const_Str s | Const_Hex s | Const_Bin s -> s

let print_identifier id =
  match id.c with
  | IdSymbol s -> s.c
  | IdUnderscoreSymNum _ ->
    Options.check_command "printer for (_ .."; ""

let rec print_sort s =
  match s.c with
  | SortIdentifier id ->
    if true || print_ty then
      sprintf "%s:%s" (print_identifier id) (to_string s.ty)
    else
      (print_identifier id)
  | SortIdMulti (id,sl) ->
    let sl = List.map print_sort sl in
    if print_ty then
      sprintf "(%s:%s %s)"
        (print_identifier id) (to_string s.ty) (String.concat " " sl)
    else
      sprintf "(%s %s)" (print_identifier id) (String.concat " " sl)

let print_sorted_var (symb,sort)  =
  sprintf "(%s %s)" symb.c (print_sort sort)

let print_qualid qid =
  match qid.c with
  | QualIdentifierId(id) ->
    if print_ty then sprintf "%s:%s" (print_identifier id) (to_string qid.ty)
    else print_identifier id
  | QualIdentifierAs(id,sort) ->
    sprintf "(as %s %s)" (print_identifier id) (print_sort sort)

let rec print_var_binding (var,bind) =
  sprintf "(%s %s)" var.c (print_term bind)

and print_var_bindings varbindings =
  List.fold_left (fun acc varbinding ->
      sprintf "%s %s" acc (print_var_binding varbinding)) "" varbindings

and print_term t =
  let s =
    match t.c with
    | TermSpecConst cst -> print_constant cst
    | TermQualIdentifier qid -> print_qualid qid
    | TermQualIdTerm (qid,tl) ->
      let tl = List.map print_term tl in
      sprintf "(%s %s)" (print_qualid qid) (String.concat " " tl)
    | TermLetTerm (varbinding_list,term) ->
      sprintf "(let (%s) %s)"
        (print_var_bindings varbinding_list) (print_term term)
    | TermForAllTerm (sorted_vars,term) ->
      sprintf "(forall (%s) %s)"
        (print_sorted_vars sorted_vars) (print_term term)
    | TermExistsTerm (sorted_vars,term) ->
      sprintf "(exists (%s) %s)"
        (print_sorted_vars sorted_vars) (print_term term)
    | TermExclimationPt (term,_key_term_list) -> (print_term term)
    | TermMatch (_term,_pattern_term_list) ->
      Options.check_command "printer for match terms"; ""
  in
  sprintf "%s:%s " s ((to_string t.ty))

and print_pars pars =
  List.fold_left (fun acc par ->
      sprintf "%s %s:%s" acc par.c (to_string par.ty)) "" pars

and print_sorts sorts =
  List.fold_left (fun acc sort ->
      sprintf "%s %s" acc (print_sort sort)) "" sorts

and print_sorted_vars sorted_vars =
  List.fold_left (fun acc sort ->
      sprintf "%s %s" acc (print_sorted_var sort)) "" sorted_vars

let print_assert pars t =
  if pars = [] then
    sprintf "%s" (print_term t)
  else
    sprintf "(par (%s) %s)" (print_pars pars) (print_term t)

let print_const_dec pars sort =
  match pars with
  | [] -> print_sort sort
  | _ -> sprintf "(par (%s) %s)" (print_pars pars) (print_sort sort)

let print_fun_dec (pars,sl,s) =
  match pars with
  | [] -> sprintf "(%s) %s" (print_sorts sl) (print_sort s)
  | _ -> sprintf "(par (%s) (%s) %s)"
           (print_pars pars) (print_sorts sl) (print_sort s)

let print_fun_def (symb,pars,svl,s) =
  match pars with
  | [] -> sprintf "%s (%s) %s" symb.c (print_sorted_vars svl) (print_sort s)
  | _ -> sprintf "%s (par (%s) (%s) %s)"
           symb.c (print_pars pars) (print_sorted_vars svl) (print_sort s)

let print_sort_dec (s,n) =
  sprintf "(%s %s)" s.c n

let print_selector (s,sort) =
  sprintf "(%s %s)" s.c (print_sort sort)

let print_cst_dec (s,selector_list) =
  sprintf "(%s %s)"
    s.c
    (print_list print_selector selector_list)

let print_dt_dec (pars,cst_dec_list) =
  match pars with
  | [] -> sprintf "(%s)" (print_list print_cst_dec cst_dec_list)
  | _ -> sprintf "(par (%s) (%s))"
           (print_pars pars)
           (print_list print_cst_dec cst_dec_list)

let print_pro_lit p =
  match p.c with
  | PropLit(s) -> sprintf "%s" s.c
  | PropLitNot(s) -> sprintf "(not %s)" s.c

let print_option _o =
  Options.check_command "printer for get/set-option"; ""
let print_info _key_info =
  Options.check_command "printer for get/set-option"; ""
let print_attribute _a =
  Options.check_command "printer for get/set-option"; ""

let print_command c =
  match c.c with
  | Cmd_Assert(pars,t) -> printf "(assert %s)\n%!" (print_assert pars t)
  | Cmd_CheckEntailment(dec) ->
    let pars,t = dec in printf "(check-entailment %s)\n%!" (print_assert pars t)

  | Cmd_CheckSat ->
    printf "(checksat)\n%!"
  | Cmd_CheckSatAssum prop_lit_list ->
    printf "(check-sat-assuming %s)\n%!"
      (print_list print_pro_lit prop_lit_list)

  | Cmd_CheckAllSat tl ->
      let tl = List.map (fun symb -> symb.c) tl in
      let s = String.concat " " tl in
      printf "(check-all-sat %s)\n%!" s

  | Cmd_DeclareConst(symbol,(pars,sort)) ->
    printf "(declare-const %s %s)\n%!" symbol.c (print_const_dec pars sort)

  | Cmd_DeclareDataType(symbol,(pars,dt_dec)) ->
    printf "(declare-datatype %s %s)\n%!" symbol.c (print_dt_dec (pars,dt_dec))
  | Cmd_DeclareDataTypes(sort_dec_list,dt_dec_list) ->
    printf "(declare-datatypes %s %s)\n%!"
      (print_list print_sort_dec sort_dec_list)
      (print_list print_dt_dec dt_dec_list)

  | Cmd_DeclareFun(symbol,fun_dec) ->
    printf "(declare-fun %s %s)\n%!" symbol.c (print_fun_dec fun_dec)
  | Cmd_DeclareSort(symbol,s) ->
    printf "(declare-sort %s %s)\n%!" symbol.c s

  | Cmd_DefineFun(fun_def,term) ->
    printf "(define-fun %s %s)\n%!" (print_fun_def fun_def) (print_term term)
  | Cmd_DefineFunRec(fun_def,term) ->
    printf "(define-fun-rec %s %s)\n%!"
      (print_fun_def fun_def) (print_term term)
  | Cmd_DefineFunsRec(fun_def_list,term_list) ->
    printf "(define-fun-rec %s %s)\n%!"
      (print_list print_fun_def fun_def_list)
      (print_list print_term term_list)

  | Cmd_DefineSort(symbol,symbol_list,sort) ->
    printf "(define-sort %s (%s) %s)\n"
      symbol.c (print_pars symbol_list) (print_sort sort)

  | Cmd_Echo(s) -> printf "(echo %s)\n" s.c
  | Cmd_GetAssert -> printf "(get-assertions)\n"
  | Cmd_GetProof -> printf "(get-proof)\n"
  | Cmd_GetUnsatCore -> printf "(get-unsat-core)\n"
  | Cmd_GetValue(term_list) ->
    printf "(get-value %s)\n" (print_list print_term term_list)
  | Cmd_GetAssign -> printf "(get-assignement)\n"
  | Cmd_GetOption(o) -> printf "(get-option %s)\n" (print_option o)
  | Cmd_GetInfo(key_info) -> printf "(get-info %s)\n" (print_info key_info)
  | Cmd_GetModel -> printf "(get-model)\n"
  | Cmd_GetUnsatAssumptions -> printf "(get-unsat-assumptions)\n"
  | Cmd_Reset -> printf "(reset)\n"
  | Cmd_ResetAssert -> printf "(reset-assertions)\n"
  | Cmd_SetLogic(s) -> printf "(set-logic %s)\n%!" s.c
  | Cmd_SetOption(o) -> printf "(set-option %s)\n%!" (print_option o)
  | Cmd_SetInfo(a) -> printf "(set-info %s)\n%!" (print_attribute a)
  | Cmd_Push(n) -> printf "(push %s)\n%!" n
  | Cmd_Pop(n) -> printf "(pop %s)\n%!" n
  | Cmd_Exit -> printf "(exit)\n"
  | Cmd_Minimize t -> printf "(minimize %s)\n%!" (print_term t)
  | Cmd_Maximize t -> printf "(minimize %s)\n%!" (print_term t)

let print commands =
  List.iter print_command commands




(****************** Env printer **********************)
(******************************************************************************)
(*********************************** Printer **********************************)
let print_sort s (arit_s, arit_t) =
  Printf.printf "%s : %d / %d \n%!" s arit_s arit_t

let print_fun s fun_def =
  Printf.printf "%s : %s \n%!" s (Smtlib_ty.to_string fun_def.params)

let print_par_fun s _fun_def =
  Printf.printf "%s : par fun  \n%!" s

let print_env env =
  Printf.printf ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n%!";
  Printf.printf ";;;;;;;;;;;;;;;;;;;;;;; Sorts;;; ;;;;;;;;;;;;;;;;;\n%!";
  SMap.iter (fun s (arit, _) ->
      print_sort s arit
    ) env.sorts;
  Printf.printf ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n%!";
  Printf.printf ";;;;;;;;;;;;;;;;;;;;;;; Funs ;;;;;;;;;;;;;;;;;;;;;\n%!";

  SMap.iter (fun s fun_defs ->
      List.iter (fun fun_def ->
          print_fun s fun_def
        ) fun_defs
    ) env.funs;
  Printf.printf ";;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;\n%!";
  Printf.printf ";;;;;;;;;;;;;;;;;;;;;;; Par funs ;;;;;;;;;;;;;;;;;\n%!";
  SMap.iter (fun s fun_defs ->
      List.iter (fun fun_def ->
          print_par_fun s fun_def
        ) fun_defs
    ) env.par_funs
