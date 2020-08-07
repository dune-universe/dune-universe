(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Term
open Proof
open Signature
open Support
open Container
open Printf

module Option = Fmlib.Option
module PC = Proof_context

type kind =
    PAxiom
  | PDeferred
  | PNormal


let is_deferred (k:kind): bool =
  match k with
    PDeferred -> true
  | _         -> false



let analyze_imp_opt
    (i:int)
    (info:    info)
    (imp_opt: implementation option)
    (c:Context.t)
    : kind * compound =
  let iface = Context.is_interface_use c || Context.is_interface_check c in
  let kind,is_do,clst =
    match imp_opt with
      None ->
        if Context.is_interface_use c then
          PAxiom,  false, []
        else
          PNormal, false, []
    | Some Impdeferred ->
        if 0 < i then
          error_info info "Deferred not allowed here";
        PDeferred,false, []
    | Some Impbuiltin ->
        if 0 < i then
          error_info info "Axiom not allowed here";
        if iface then
          error_info info "Axiom not allowed in interface file";
        PAxiom,   false, []
    | Some Impevent ->
        error_info info "Assertion cannot be an event"
    | Some (Impdefined (Some locs,is_do,cmp)) ->
        not_yet_implemented info "Local variables in assertions"
    | Some (Impdefined (None,is_do,cmp)) ->
        if Context.is_interface_use c then begin
          if is_do || cmp <> [] then
            error_info info "proof not allowed in interface file";
          PAxiom,  false, []
        end else
          PNormal, false, cmp
  in
  if is_do then
    not_yet_implemented info "Assertions with do block"
  else
    kind, clst


let analyze_body (i:int) (info:info) (bdy: feature_body) (c:Context.t)
    : kind * compound * compound * compound =
  match bdy with
    _, _, [] ->
      error_info info "Assertion must have an ensure clause"
  | rlst, imp_opt, elst ->
      let kind,clst =
        analyze_imp_opt i info imp_opt c
      in
      kind, rlst, clst, elst



let get_boolean_term (e: expression) (pc:Proof_context.t): term =
  let c = Proof_context.context pc in
  (Typer.boolean_term e c).v

let term_preconditions (info:info) (t:term) (pc:PC.t): term list =
  let c = PC.context pc in
  try
    Context.term_preconditions t c
  with NYI ->
    not_yet_implemented info ("Calculation of the preconditions of " ^
                              (PC.string_of_term t pc))



let prove_insert_close (t:term) (pc:PC.t): unit =
  ignore(Prover.prove_and_insert t pc);
  PC.close pc



let verify_preconditions (t:term) (info:info) (pc:Proof_context.t): unit =
  if PC.is_private pc then begin
    let pres = term_preconditions info t pc in
    List.iter
      (fun p ->
        try
          ignore (Prover.proof_term p pc)
        with Proof.Proof_failed msg ->
          error_info info ("Cannot prove precondition \"" ^
                           (PC.string_of_term p pc) ^
                           "\"\n  of term \"" ^
                           (PC.string_of_term t pc) ^ "\"" ^
                           msg)
      )
      pres
  end


let get_boolean_term_verified (e: expression) (pc:Proof_context.t): term =
  let t = get_boolean_term e pc in
  verify_preconditions t e.i pc;
  t


let terms_of_compound (lst:compound) (pc:PC.t): term withinfo list =
  List.map
    (fun e ->
      let t = get_boolean_term e pc in
      withinfo e.i t
    )
    lst


let add_assumption_or_axiom_terms
    (lst: term withinfo list)
    (is_axiom: bool)
    (pc:PC.t)
    : (int * info) list =
  List.map
    (fun it ->
      let t = it.v in
      if is_axiom && Term.is_all_quantified t then begin
        let str =
          "Universal quantification not allowed in ensure clause"
          ^ "\n  normalized term: "
          ^ (PC.string_long_of_term t pc) in
        error_info it.i str
      end;
      verify_preconditions t it.i pc;
      let idx =
        if is_axiom then
          Proof_context.add_axiom t pc
        else begin
          Proof_context.add_assumption t true pc
        end in
      idx,it.i)
    lst



let add_assumptions_or_axioms
    (lst:compound) (is_axiom:bool) (pc:Proof_context.t)
    : (int*info) list =
  let lst = terms_of_compound lst pc in
  add_assumption_or_axiom_terms lst is_axiom pc


let add_assumptions (lst:compound) (pc:Proof_context.t): unit =
  let _ = add_assumptions_or_axioms lst false pc in ();
  PC.close pc


let add_axioms (lst:term withinfo list) (pc:Proof_context.t): (int*info) list =
  add_assumption_or_axiom_terms lst true pc



let function_property_list (lst:compound) (pc:PC.t): term list =
  let pc1 = Proof_context.push_empty pc in
  List.map
    (fun e ->
      let t = get_boolean_term e pc in
      verify_preconditions t e.i pc1;
      let _ = PC.add_assumption t true pc1 in
      t)
    lst





let add_property_assertion
    (idx:int)
    (pc: PC.t)
    : unit =
  assert (PC.is_global pc);
  let ft = PC.feature_table pc in
  let lst = Feature_table.function_property_assertions idx ft in
  List.iter
    (fun t ->
      ignore(PC.add_proved t (Axiom t) pc)
    )
    lst



let update_feature
    (info:      info)
    (idx:       int)
    (is_new:    bool)
    (is_export: bool)
    (spec:      Feature.Spec.t)
    (impl:      Feature.implementation)
    (pc:        PC.t): unit =
  assert (not (is_new && is_export));
  let match_impl priv pub =
    match priv,pub with
      Feature.Deferred, Feature.Deferred |
      Feature.Builtin,  Feature.Empty |
      Feature.Empty,    Feature.Empty -> true
    | _ -> false
  in
  let ft          = PC.feature_table pc in
  let update (): unit =
    let is_ghost = Feature_table.is_ghost_specification spec ft in
    if is_ghost && not (Feature_table.is_ghost_function idx ft) then
      error_info info "Must be a ghost function";
    Feature_table.update_specification idx spec ft
  in
  if PC.is_private pc || not (PC.is_interface_check pc) then begin
    if not is_new then begin
      let spec0,impl0 = Feature_table.body idx ft in
      if not (Feature.Spec.is_consistent spec0 spec) then
        error_info info "Specification does not match the previous declaration";
      if not ((PC.is_private pc && impl0=impl) || match_impl impl0 impl) then
        error_info info
          "Implementation status does not match the previous declaration";
      if Feature.Spec.has_no_definition spec then
        Feature_table.hide_definition idx ft
    end else
      update ()
  end else if is_export then begin
    assert (PC.is_interface_check pc);
    let spec0,impl0 = Feature_table.body idx ft in
    if not (match_impl impl0 impl) then
      error_info info "Implementation status is not consistent with private status";
    if not (Feature.Spec.is_consistent spec0 spec) then
      error_info info "Specification is not consistent with private specification"
  end else begin
    assert (PC.is_interface_check pc);
    let spec0,impl0 = Feature_table.body idx ft in
    if not (Feature.Spec.equivalent spec spec0) then
      error_info info "Specification does not match the previous declaration";
    if not (match_impl impl0 impl) then
      error_info info "Implementation status is not consistent with private status"
  end


(* Functions defined by properties

      f(a:A,b:B,...):RT
          require
              r1; r2; ...
          ensure
              e1; e2; ...   -- 'ei' contains 'Result'
          end

   Proof obligations:

   a) Existence:

         some(x) e1[Result:=x] and e2[Result:=x] and ...

   b) Uniqueness:  (requires that RT derives from ANY)

         all(x,y) e1[Result:=x] ==> e2[Result:=x] ==> ...
                  e2[Result:=y] ==> e2[Result:=y] ==> ...
                  x = y

   Assertions:

        all(a,b,...) r1 ==> r2 ==> ... ==> ei[Result:=f(a,b,...)]
 *)

let adapt_inner_function_term
    (info:info)
    (t:term)
    (nargs:int)
    (pc: PC.t): term =
  (* Functions have a result variable with number [nargs]. However all preconditions,
     definition terms and postconditions finally don't contain the result variable.
     If a function is defined by properties then the variable 'Result' is replaced
     by the corresponding call. I.e. all variables starting from [nargs] are shifted
     down by one. *)
  if PC.has_result_variable pc then
    try
      Term.down_from 1 nargs t
    with Term_capture ->
      error_info info "illegal use of \"Result\""
  else
    t


let is_feature_term_recursive (t:term) (idx:int) (pc:PC.t): bool =
  let c = PC.context pc in
  let nvars = Context.count_variables c in
  let free  = Term.free_variables t nvars in
  IntSet.mem (idx+nvars) free




(* Recursion Checker
   =================

   Valid recursive call: At least one argument of the recursive call is
                         structurally smaller than the original argument.

   Algorithm: We maintain a list of quadruples

       (n,term,level,iarg)

   where (n,term) is a subterm of [iarg] where level indicates which level
   below. [level = 0] indicates that the term is at the same level as the
   argument.
 *)



let check_recursion0 (info:info) (idx:int) (t:term) (pc:PC.t): unit =
  (* Check that the term [t] is a valid recursive definition term for the
     feature [idx], i.e. all recursive calls are valid.

     [idx] is absolute
     [pc] is a valid environment for the term [t]
   *)
  assert (PC.is_toplevel pc);
  let c = PC.context pc
  and ft = PC.feature_table pc in
  let nargs   = Context.count_last_arguments c
  in
  let find (n:int) (t:term) (lst:(int*term*int*int) list): int * int =
    let _,_,level,iarg =
      List.find
        (fun (n0,t0,level,iarg) ->
          assert (n0 <= n);
          Term.up (n-n0) t0 = t)
        lst in
    level,iarg
  in
  let find_opt (n:int) (t:term) (lst:(int*term*int*int) list): (int * int) option =
    try
      let level,iarg = find n t lst in Some(level,iarg)
    with Not_found -> None
  in
  let add_pattern (insp_arr: (int*int) option array) (n:int) (parr:term array)
      (nb:int) (lst:(int*term*int*int) list): (int*term*int*int) list =
    let len_insp = Array.length insp_arr
    and len_pat  = Array.length parr in
    assert (len_pat = len_insp);
    interval_fold
      (fun lst i ->
        match insp_arr.(i) with
          Some (level,iarg) ->
            let plst = Feature_table.pattern_subterms n parr.(i) nb ft in
            List.fold_left
              (fun lst (nall,p,plevel) -> (nall,p,level+plevel,iarg)::lst)
              lst
              plst
        | None ->
            lst)
      lst 0 len_insp
  in
  let rec check
            (t:term) (tlst:(int*term*int*int) list) (c:Context.t)
          : unit =
    let nb = Context.count_variables c in
    let check_args args =
      Array.iter (fun arg -> check arg  tlst c) args
    and check0 t = check t tlst c
    in
    match t with
      Variable i when i = idx + nb ->
        assert (nargs = 0);
        assert (Feature_table.arity idx ft = 0);
        error_info info ("Illegal recursive call of the constant " ^
                         Feature_table.feature_name idx ft)
    | Variable i ->
        ()
    | VAppl (i,args,_,_) when i = idx + nb ->
        let len = Array.length args in
        if len = 0 then
          error_info info ("Illegal recursive call of the constant " ^
                           Feature_table.feature_name idx ft);
        let is_lower_arg i =
          try
            let level,iarg = find nb args.(i) tlst in
            iarg = i && level > 0
          with Not_found ->
            false
        in
        if not (interval_exist is_lower_arg 0 len) then
          error_info info ("Illegal recursive call \"" ^
                           (Context.string_of_term t c) ^ "\"")
    | VAppl (i,args,_,_) ->
        check_args args
    | Application (f,args,_) ->
        check f tlst c;
        check_args args
    | Lam (tps,fgs,pres,t0,rt) ->
        not_yet_implemented
          info
          "Lambda expressions in recursive definitions"
    | QExp (fargs,fgs,t0,_) ->
        error_info
          info
          "Quantified expressions not allowed in recursive definitions"
    | Ifexp (c, a, b) ->
       check0 c;
       check0 a;
       check0 b
    | Asexp (t, tps, pat) ->
       check0 t (* the pattern has only constructors,
                   no recursive calls possible *)
    | Inspect (insp, cases) ->
       check0 insp;
       let insp_arr = Feature_table.args_of_tuple insp nb ft
       in
       let insp_arr2 = Array.map (fun t -> find_opt nb t tlst) insp_arr in
       let ninsp    = Array.length insp_arr in
       Array.iter
         (fun (fs,pat,res) ->
           let n = Array2.count fs in
           let c1 = Context.push_typed0 fs Formals.empty c in
           let pat_tp = Context.type_of_term pat c1 in
           let parr =
             let arr = Feature_table.args_of_tuple pat (n+nb) ft in
             if Array.length arr > ninsp then
               Feature_table.args_of_tuple_ext pat pat_tp (n+nb) ninsp ft
             else
               arr
           in
           assert (Array.length parr = ninsp); (* because there have to be enough
                                                  pattern for the inspected
                                                  expressions *)
           let tlst2 = add_pattern insp_arr2 n parr nb tlst in
           check res tlst2 c1
         )
         cases
    | Indset (n,nms,rs) ->
        error_info
          info
          "Inductively defined sets not allowed in recursive definitions"
  in
  let nvars = Context.count_variables c in
  let tlst0 =
    interval_fold (fun lst i -> (nvars,Variable i,0,i)::lst) [] 0 nargs in
  check t tlst0 c




let check_recursion (info:info) (idx:int) (t:term) (pc:PC.t): unit =
  if not (PC.is_interface_use pc) && is_feature_term_recursive t idx pc then
    check_recursion0 info idx t pc


let feature_specification
    (info:info)
    (idx: int)
    (nms: int array)
    (reqlst: compound)
    (enslst: compound)
    (pc:PC.t)
    : Feature.Spec.t =
  let nargs = Array.length nms
  and context = PC.context pc in
  let adapt_term t = adapt_inner_function_term info t nargs pc in
  let adapt_list lst = List.map adapt_term lst in
  add_assumptions reqlst pc;
  let pres = PC.assumptions pc in
  if List.exists (fun t -> is_feature_term_recursive t idx pc) pres then
    error_info info "Recursive calls not allowed in preconditions";
  let pres = adapt_list pres in
  match enslst with
    [] ->
      Feature.Spec.make_func_spec nms pres []
  | _ ->
      let prove cond errstring =
        try Prover.prove cond pc
        with Proof.Proof_failed msg ->
          error_info info ("Cannot prove " ^ errstring ^ " of \"Result\"" ^ msg)
      in
      let posts = function_property_list enslst pc in
      if List.exists (fun t -> is_feature_term_recursive t idx pc) pres then
        error_info info "Recursive calls not allowed in preconditions";
      if PC.is_private pc then begin
        let exist = Context.existence_condition posts context in
        let unique =
          try Context.uniqueness_condition posts context
          with Not_found ->
            error_info
              info
              ("Result type does not inherit "
               ^ (Class_table.class_name Constants.any_class (PC.class_table pc)))
        in
        prove exist  "existence";
        prove unique "uniqueness"
      end;
      let posts = Context.function_postconditions idx posts context in
      assert (List.for_all (fun t -> is_feature_term_recursive t idx pc) posts);
      let posts = adapt_list posts
      in
      Feature.Spec.make_func_spec nms pres posts


let feature_specification_ast
    (info:info)
    (nms: int array)
    (idx: int)
    (bdy: feature_body option)
    (exp: expression option)
    (pc: Proof_context.t): Feature.Spec.t * (info*term) option =
  let nargs = Array.length nms in
  let adapt_term t =
    adapt_inner_function_term info t nargs pc in
  let adapt_list lst = List.map adapt_term lst in
  let feature_spec reqlst enslst =
    feature_specification info idx nms reqlst enslst pc, None in
  let context = PC.context pc in
  match bdy, exp with
    None, None ->
      Feature.Spec.make_empty nms, None
  | None, Some ie ->
      let term = Typer.result_term ie context in
      let term1 = adapt_term term in
      (Feature.Spec.make_func_def nms (Some term1) []), Some(ie.i,term)
  | Some (reqlst,_,enslst), None ->
      feature_spec reqlst enslst
  | Some (reqlst,None,[]), Some ie ->
      let term = Typer.result_term ie context in
      let term1 = adapt_term term in
      add_assumptions reqlst pc;
      let pres = PC.assumptions pc in
      if List.exists (fun t -> is_feature_term_recursive t idx pc) pres then
        error_info info "Recursive calls not allowed in preconditions";
      let pres = adapt_list pres in
      (Feature.Spec.make_func_def nms (Some term1) pres), Some(ie.i,term)
  | Some bdy, Some exp ->
      assert false (* cannot happen *)



let implementation_status
    (info:info)
    (bdy: feature_body option)
    (pc: Proof_context.t): Feature.implementation =
  match bdy with
    None
  | Some (_,None,_) -> Feature.Empty
  | Some (_,Some Impbuiltin,_) -> Feature.Builtin
  | Some (_,Some Impdeferred,_) -> Feature.Deferred
  | Some (_,Some Impevent,_) ->
      not_yet_implemented info "events"
  | Some (_,Some Impdefined(_,_,_),_) ->
      not_yet_implemented info "features with locals"


let check_function_term (idx:int) (opt:(info*term)option) (pc:PC.t): unit =
  match opt with
    None -> ()
  | Some (info,term) ->
      check_recursion info idx term pc;
      verify_preconditions term info pc


let analyze_feature
    (fn: feature_name withinfo)
    (entlst: entities list withinfo)
    (rt: return_type)
    (is_func: bool)
    (bdy: feature_body option)
    (exp: expression option)
    (pc: Proof_context.t): unit =
  (*  - Analyze the signature and push into the context
      - Find the index, check if it is a new feature or it is the exportation of an
        already available feature.
      - Add a new feature or export an already available feature.
      - Get the specification of the feature and update the feature.
      - Check the validity of a potential recursion
      - Verify the preconditions of the definition term. (What about precondition
        terms and postconditions).
          *)
  if rt = None then
    not_yet_implemented fn.i "Features without result type";
  let pc1 =
    let rvar = is_func || Option.has rt in
    PC.push entlst rt false is_func rvar pc in
  let nms, sign, tvs =
    let c = Proof_context.context pc1 in
    Context.local_argnames c,
    Context.signature c,
    Context.tvars c
  in
  if Tvars.count tvs > 0 then
    error_info entlst.i "Untyped variables not allowed in global functions";
  let ft = Proof_context.feature_table pc in
  let imp  = implementation_status fn.i bdy pc in
  let idx, is_new, is_export =
    try
      let idx = Feature_table.find_with_signature fn tvs sign ft in
      let is_export =
        PC.is_interface_check pc
        && not (Feature_table.is_feature_visible idx ft) in
      if is_export && not (Sign.is_ghost sign) &&
        Feature_table.is_ghost_function idx ft
      then
        error_info fn.i "Must be a ghost function";
      if is_export then
        Feature_table.export_feature idx ft;
      idx, false, is_export
    with Not_found ->
      let cnt = Feature_table.count ft in
      Feature_table.add_feature fn tvs nms sign imp ft;
      cnt, true, false
  in
  if PC.is_interface_check pc && is_new then
    error_info fn.i "Feature not declared in implementation file";
  let spec,opt = feature_specification_ast fn.i nms idx bdy exp pc1 in
  update_feature fn.i idx is_new is_export spec imp pc;
  check_function_term idx opt pc1;
  if is_new then begin
    add_property_assertion idx pc;
    Inherit.add_new_feature fn.i idx pc
  end




let add_case_axiom (t:term) (pc:Proof_context.t): int =
  PC.add_proved t (Proof.Axiom t) pc



let add_case_inversion_equal (idx1:int) (idx2:int) (cls:int) (pc:PC.t): unit =
  (* Add case inversions

     all(a11:A11,a12:A12,...,a21:A21,a22:A22,...)
         c1(a11,a12,...) = c2(a21,a22,...)  ==>  false
   *)
  assert (idx1 <> idx2);
  let ft = PC.feature_table pc in
  let tvs1,s1 = Feature_table.signature0 idx1 ft
  and tvs2,s2 = Feature_table.signature0 idx2 ft in
  assert (tvs1 = tvs2);
  let n1 = Sign.arity s1
  and n2 = Sign.arity s2 in
  let args1 = Array.init n1 (fun i -> Variable i)
  and args2 = Array.init n2 (fun i -> Variable (n1+i))
  and fgnms,fgcon = Tvars.fgnames tvs1, Tvars.fgconcepts tvs1
  and tps = Array.append (Sign.arguments s1) (Sign.arguments s2) in
  let ags = standard_substitution (Array.length fgcon) in
  let appl idx args =
    VAppl(n1+n2+idx,args,ags,false)
  in
  let t1 = appl idx1 args1
  and t2 = appl idx2 args2
  and eq_id    = n1 + n2 + Feature_table.equality_index cls ft
  and imp_id   = n1 + n2 + Constants.implication_index
  in
  let t = Term.binary imp_id
      (VAppl(eq_id, [|t1;t2|], ags,false))
      (Feature_table.false_constant (n1+n2)) in
  let t = Term.all_quantified
      (Formals.make (standard_argnames (n1+n2)) tps)
      (Formals.make fgnms fgcon)
      t in
  (*printf "inversion %s\n" (Proof_context.string_of_term t pc);*)
  ignore(add_case_axiom t pc)




let add_case_inversion_as (idx1:int) (idx2:int) (cls:int) (pc:PC.t): unit =
  (* Add case inversions

     all(a:T) a as pat1  ==>  a as pat2  ==>  false
   *)
  assert (idx1 <> idx2);
  let ft = PC.feature_table pc in
  let tvs1,s1 = Feature_table.signature0 idx1 ft
  and tvs2,s2 = Feature_table.signature0 idx2 ft in
  assert (tvs1 = tvs2);
  let ags = standard_substitution (Tvars.count_fgs tvs1) in
  let make_asexp idx s =
    let n = Sign.arity s in
    let args = standard_substitution n in
    Asexp (Variable 0,
           Sign.arguments s,
           VAppl (1+n+idx, args, ags, false))
  in
  let pat1 = make_asexp idx1 s1
  and pat2 = make_asexp idx2 s2
  and imp_id   = 1 + Constants.implication_index
  and false_const = Feature_table.false_constant 1
  in
  let t = Term.binary imp_id pat1 (Term.binary imp_id pat2 false_const) in
  let nms = standard_argnames 1
  and tps = [|Sign.result s1|]
  and fgnms = Tvars.fgnames tvs1
  and fgcon = Tvars.fgconcepts tvs1
  in
  let q = Term.all_quantified (Formals.make nms tps) (Formals.make fgnms fgcon) t
  in
  (*printf "inversion %s\n" (PC.string_of_term q pc);*)
  ignore(add_case_axiom q pc)




let add_case_inversions
    (cls:  int)
    (clst: int list)
    (pc:   Proof_context.t): unit =
  List.iter
    (fun idx1 ->
      List.iter
        (fun idx2 ->
          if idx1 = idx2 then
            ()
          else begin
            add_case_inversion_equal idx1 idx2 cls pc;
            if idx1 < idx2 then
              add_case_inversion_as idx1 idx2 cls pc
          end)
        clst)
    clst



let add_case_injections
    (clst: int list)
    (pc:Proof_context.t): unit =
  (* Add the injection laws for the constructors [clst]. For each constructor and
     each argument of the constructor there is an injection law of the form:

     all(a1,..,b1,..) c(a1,..) = c(b1,..) ==> ai = bi
   *)
  let ft   = Proof_context.feature_table pc in
  List.iter
    (fun idx ->
      let tvs,s = Feature_table.signature0 idx ft in
      let n = Sign.arity s in
      if n = 0 then
        ()
      else
        let fgnms,fgcon = Tvars.fgnames tvs, Tvars.fgconcepts tvs
        and tps  = Sign.arguments s
        and rtp  = Sign.result s in
        let tps  = Array.append tps tps
        and nfgs = Array.length fgnms in

        (* We need [2*n] variables: a1,..,b1,.. *)
        let args1 = Array.init n (fun i -> Variable i)
        and args2 = Array.init n (fun i -> Variable (n+i))
        and nms   = standard_argnames (2*n)
        and ags   = standard_substitution nfgs
        in

        (* The term c(a1,..) = c(b1,...) *)
        let eq_ca_cb =
          let ca = VAppl(2*n+idx, args1, ags, false)
          and cb = VAppl(2*n+idx, args2, ags, false) in
          Feature_table.equality_term ca cb (2*n) rtp tvs ft
        in
        for i = 0 to n - 1 do
          let itp = tps.(i) in
          let eq_ai_bi =
            let ai,bi = Variable i, Variable (n+i) in
            Feature_table.equality_term ai bi (2*n) itp tvs ft
          in
          let imp = Feature_table.implication eq_ca_cb eq_ai_bi (2*n) in
          let t =
            Term.all_quantified
              (Formals.make nms tps)
              (Formals.make fgnms fgcon)
              imp in
          (*printf "injection %s\n" (Proof_context.string_of_term t pc);*)
          ignore(add_case_axiom t pc)
        done)
    clst


let can_be_constructed_without (cls:int) (posset:IntSet.t) (pc:PC.t): bool =
  (* Does the class [cls] have a base constructor which does not use any
     object of type of a formal generic which is in any position of [posset]
     in the class type?
   *)
  let ct = PC.class_table pc
  and ft = PC.feature_table pc in
  assert (Class_table.is_inductive cls ct);
  IntSet.exists
    (fun c ->
      let tvs,sign = Feature_table.signature0 c ft in
      assert (Tvars.count tvs = 0);
      let nfgs = Tvars.count_fgs tvs in
      let cls2,fgs = split_type (Sign.result sign) in
      assert (cls2 = cls + nfgs);
      let fgset:IntSet.t =
        IntSet.fold
          (fun pos set ->
            assert (pos < Array.length fgs);
            assert (Term.is_variable fgs.(pos));
            IntSet.add (Term.variable fgs.(pos)) set)
          posset
          IntSet.empty in
      List.for_all
        (fun argtp ->
          let cls,_ = split_type argtp in
          not (IntSet.mem cls fgset)
        )
        (Array.to_list (Sign.arguments sign))
    )
    (Class_table.base_constructors cls ct)



let is_base_constructor (idx:int) (cls:int) (pc:PC.t): bool =
  let ct = PC.class_table pc
  and ft = PC.feature_table pc in
  let tvs,sign = Feature_table.signature0 idx ft in
  let ntvs     = Tvars.count_all tvs in
  let is_class_involved tp = Tvars.is_class_involved cls tp tvs
  in
  List.for_all
    (fun tp ->
      match tp with
        Variable i when i = cls + ntvs ->
          false
      | VAppl(i,ags,_,_) when i = cls + ntvs ->
          false
      | VAppl(i,ags,_,_) ->
          assert (ntvs <= i);
          Class_table.is_inductive (i-ntvs) ct &&
          begin
            let nags = Array.length ags in
            let rec get_posset_from k posset =
              if k = nags then
                posset
              else
                let posset =
                  if is_class_involved ags.(k) then
                    IntSet.add k posset
                  else
                    posset in
                get_posset_from (k+1) posset
            in
            let posset = get_posset_from 0 IntSet.empty in
            can_be_constructed_without (i-ntvs) posset pc
          end
      | _ ->
          true
    )
    (Array.to_list (Sign.arguments sign))


let creators_check_formal_generics
      (info:info) (clst:int list) (tvs:Tvars.t) (cls:int) (ft:Feature_table.t)
    : unit =
  assert (Tvars.count tvs = 0);
  let nfgs = Tvars.count_fgs tvs in
  for i = 0 to nfgs - 1 do
    match Tvars.concept i tvs with
    | Variable i  when i = cls + nfgs ->
       ()
    | _ ->
       if List.for_all
            (fun cidx ->
              let _,sign = Feature_table.signature0 cidx ft in
              let argtps = Sign.arguments sign in
              interval_for_all
                (fun j ->
                  argtps.(j) <> Variable i)
                0 (Array.length argtps))
            clst
       then
         let nme = (Tvars.fgnames tvs).(i) in
         error_info info ("Formal generic " ^ (ST.string nme)
                          ^ " does not occur in any constructor")
  done



let put_creators
    (cls: int)
    (cls_is_new:bool)
    (tvs: Tvars.t)
    (cls_tp: type_term)
    (creators: (feature_name withinfo * entities list) list withinfo)
    (pc: Proof_context.t)
    : unit =
  let c    = Proof_context.context pc
  and info = creators.i in
  let ft   = Context.feature_table c in
  let ct   = Feature_table.class_table ft in
  let c0lst, c1lst =
    List.fold_left
      (fun (c0lst,c1lst) (fn,ents) ->
        let formals,n_untyped =
          Class_table.formal_arguments (withinfo fn.i ents) tvs ct in
        assert (n_untyped = 0);
        let formals = Array.of_list formals in
        let nms, argtps = Myarray.split formals in
        let sign = Sign.make_func argtps cls_tp in
        let cnt = Feature_table.count ft in
        let spec = Feature.Spec.make_func_def nms None []
        and imp  =
          if Class_table.is_deferred cls ct then
            Feature.Deferred
          else
            Feature.Empty
        in
        let idx, is_new, is_export =
          try
            let idx = Feature_table.find_with_signature fn tvs sign ft in
            let is_export =
              PC.is_interface_check pc &&
              not (Feature_table.is_feature_visible idx ft) in
            idx, false, is_export
          with Not_found ->
            cnt, true, false
        in
        assert (not cls_is_new || is_new);
        for i = 0 to Sign.arity sign - 1 do
          let arg = Sign.arg_type i sign in
          if not (Class_table.type_descends_from_any arg tvs ct)
          then
            error_info fn.i
              ("Type " ^
                 (Class_table.string_of_type arg tvs ct)
                 ^ " does not inherit "
                 ^ (Class_table.class_name Constants.any_class ct))
        done;
        if is_new then
          Feature_table.add_feature fn tvs nms sign imp ft
        else if is_export then
          Feature_table.export_feature idx ft;
        update_feature fn.i idx is_new is_export spec imp pc;
        let is_base = is_base_constructor idx cls pc in
        if is_base && c1lst <> [] then
          error_info fn.i
            "Base constructors must be defined before other constructors"
        else if not is_base && c0lst = [] then
          error_info fn.i "No base constructors available";
        if is_base then idx::c0lst, c1lst else c0lst, idx::c1lst)
      ([],[])
      creators.v in
  let clst_rev = c1lst @ c0lst in
  let clst = List.rev clst_rev in
  let cset = IntSet.of_list clst
  and cset_base = IntSet.of_list c0lst in
  if not (Class_table.is_inductive cls ct) then
    begin
      creators_check_formal_generics creators.i clst tvs cls ft;
      Class_table.set_constructors cset_base cset cls ct;
      add_case_inversions cls clst pc;
      add_case_injections clst pc;
      PC.add_induction_law0 cls pc
  end else if
    not (IntSet.equal (Class_table.constructors cls ct) cset)
  then
    error_info info "Different constructors than previously declared"
  else
    ()



let inherit_any (cls:int) (pc:Proof_context.t): unit =
  let simple_type (str:string): type_t =
    Normal_type ([], ST.symbol str,[])
  in
  (* add or export equality *)
  if PC.is_interface_check pc then
    Feature_table.export_equality cls (PC.feature_table pc)
  else
    Feature_table.add_equality cls (PC.feature_table pc);
  (* inherit ANY *)
  let parent = false, withinfo UNKNOWN (simple_type "ANY"), []
  and tvs =
    Class_table.tvs_of_class_for_parent cls (Proof_context.class_table pc)
  in
  Inherit.inherit_parents cls tvs [parent] pc






let put_class
    (hm:       header_mark withinfo)
    (cv:       int withinfo option) (* optional class variable *)
    (cn:       classname)
    (fgs:      formal_generics)
    (creators: (feature_name withinfo * entities list) list withinfo)
    (pc: Proof_context.t)
    : unit =
  (*  Analyze the class declaration [hm,cn,fgs,creators] and add or update the
      corresponding class.  *)
  assert (Proof_context.is_global pc);
  if fst cn.v <> [] then
    error_info cn.i "Illegal class name qualification";
  let ft = Proof_context.feature_table pc in
  let ct = Feature_table.class_table ft in
  let tvs = Class_table.class_tvs fgs ct in
  begin
    match hm.v, cv with
    | _ , None | Deferred_hmark, Some _ ->
       ()
    | _,  Some cv  ->
       Format.printf
         "%s %s@."
         (info_string cv.i)
         "Only deferred (i.e. abstract classes) can have a class variable.";
       exit 1
  end;
  let idx,is_new =
    try
      let idx = Class_table.find_for_declaration (snd cn.v) ct in
      Class_table.update idx cn.i hm cv tvs ct;
      idx, false
    with Not_found ->
      let path, cn0 = cn.v in
      if path <> [] then
        error_info
          cn.i
          ("Class \"" ^ (string_of_classname path cn0) ^ "\" cannot be found");
      let idx = Class_table.count ct in
      Class_table.add hm cv cn0 tvs ct;
      idx, true
  in
  if is_new && PC.is_interface_check pc then
    error_info
      cn.i
      ("A new class "  ^ (ST.string (snd cn.v))
       ^ " cannot be declared in an interface file");
  if 2 <= PC.verbosity pc then begin
    let str = if is_new then "new" else "update" in
    printf "\n  %s class %s\n" str (ST.string (snd cn.v));
    end;
  let cls_tp,cls_tvs = Class_table.class_type idx ct in
  if idx <> Constants.any_class && (hm.v = Deferred_hmark || creators.v <> [])
  then
    begin
      inherit_any idx pc;
      if creators.v <> [] then
        put_creators idx is_new cls_tvs cls_tp creators pc
    end


let put_inheritance
    (hm:       header_mark withinfo)
    (cn:       classname)
    (fgs:      formal_generics)
    (inherits: inherit_clause)
    (pc: Proof_context.t)
    : unit =
  (*  Analyze the inheritance declaration [hm,cn,fgs,inherits] and update the
      corresponding class.  *)
  assert (Proof_context.is_global pc);
  let ft = Proof_context.feature_table pc in
  let ct = Feature_table.class_table ft in
  let tvs = Class_table.class_tvs fgs ct in
  let path,cn0 = cn.v in
  let cls = Class_table.class_index  path cn0 Tvars.empty cn.i ct in
  Class_table.check_class cls hm None tvs ct;
  Inherit.inherit_parents cls tvs inherits pc


let put_formal_generic (fgnme:int withinfo) (cn:classname) (pc:PC.t): unit =
  let ct = PC.class_table pc in
  let path, cn0 = cn.v
  and info = cn.i in
  let cidx = Class_table.class_index path cn0 Tvars.empty info ct in
  Class_table.put_formal fgnme cidx ct


let analyze (ast: declaration list) (pc:Proof_context.t): unit =
  let rec analyz (ast: declaration list): unit =
    let one_decl (d:declaration) =
      match d with
      | Class_declaration (hm, cvar, cname, fgens, creators) ->
         put_class hm cvar cname fgens creators pc
      | Inheritance_declaration (hm,cname,fgens,inherits) ->
         put_inheritance hm cname fgens inherits pc
      | Named_feature (fn, entlst, rt, is_func, body, expr) ->
          analyze_feature fn entlst rt is_func body expr pc
      | Theorem (entlst, req, ens, prf) ->
         if PC.verbosity pc > 1 then
           Prover.push_statistics ();
         Source_prover.prove_and_store entlst req ens prf pc;
         if PC.is_private pc && PC.verbosity pc > 1 then
           Prover.print_statistics_and_pop (PC.trace_prefix pc)
      | Formal_generic (fgname, cn) ->
         put_formal_generic fgname cn pc
      | Class_list lst ->
          not_yet_implemented lst.i "Mutually recursive types"
      | Feature_list lst ->
          not_yet_implemented lst.i "Mutually recursive features"
    in
    match ast with
      [] -> ()
      | f::t -> one_decl f; analyz t
  in
  analyz ast;
  if Proof_context.is_interface_check pc then
    Proof_context.check_interface pc
