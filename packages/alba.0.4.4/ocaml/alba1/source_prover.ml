(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support
open Term
open Signature
open Proof
open Container
open Printf

module PC = Proof_context

type info_term  = term withinfo
type info_terms = info_term list


let term_preconditions (it:info_term) (pc:PC.t): term list =
  let c = PC.context pc in
  try
    Context.term_preconditions it.v c
  with NYI ->
    not_yet_implemented it.i ("Calculation of the preconditions of " ^
                              (PC.string_of_term it.v pc))


let verify_preconditions (it:info_term) (pc:PC.t): unit =
  if PC.is_private pc then begin
    let pres = term_preconditions it pc in
    List.iter
      (fun p ->
        try
          Prover.prove p pc
        with Proof.Proof_failed msg ->
          error_info it.i ("Cannot prove precondition \"" ^
                           (PC.string_of_term p pc) ^
                           "\"\n  of term \"" ^
                           (PC.string_of_term it.v pc) ^ "\"" ^
                           msg))
      pres
  end


let get_boolean_term (e: expression) (pc:Proof_context.t): info_term =
  let c = PC.context pc in
  Typer.boolean_term e c


let get_boolean_term_verified
    (e: expression) (pc:Proof_context.t): info_term =
  let it = get_boolean_term e pc in
  verify_preconditions it pc;
  it


let get_term (e:expression) (pc:PC.t): info_term =
  let c = PC.context pc in
  Typer.untyped_term e c


let get_term_verified
    (e: expression) (pc:Proof_context.t): info_term =
  let it = get_term e pc in
  verify_preconditions it pc;
  it





let push
    (entlst: entities list withinfo)
    (rlst: compound)
    (elst: compound)
    (pc:PC.t)
    :  info_terms * info_terms * PC.t =
  assert (PC.count_type_variables pc = 0);
  let tps,fgs,rlst,elst =
    Typer.structured_assertion entlst rlst elst (PC.context pc)
  in
  rlst, elst, PC.push_typed0 (Formals.from_pair tps) (Formals.from_pair fgs) pc




let add_assumptions (rlst:info_terms) (pc:PC.t): unit =
  List.iter
    (fun it ->
      verify_preconditions it pc;
      ignore (PC.add_assumption it.v true pc)
    )
    rlst


let add_axiom (it:info_term) (pc:PC.t): int =
  verify_preconditions it pc;
  PC.add_axiom it.v pc



let prove_insert_report_base0
    (info:info) (goal:term) (print_goal:term) (search:bool) (pc:PC.t): int =
  try
    let t,pt = Prover.proof_term goal pc in
    PC.add_proved_term t pt search pc
  with Proof.Proof_failed msg ->
    error_info info ("Cannot prove \"" ^ (PC.string_of_term print_goal pc)
                     ^ "\"" ^ msg)


let prove_insert_report_base
      (info:info) (goal:term) (search:bool) (pc:PC.t): int =
  prove_insert_report_base0 info goal goal search pc


let prove_insert_report (goal:info_term) (search:bool) (pc:PC.t): int =
  prove_insert_report_base goal.i goal.v search pc


let prove_insert_close (goal:info_term) (pc:PC.t): int =
  let idx = prove_insert_report goal true pc in
  PC.close pc;
  idx


let store_unproved
    (is_defer:bool)
    (elst: info_terms)
    (pc:PC.t)
    : unit =
  assert (PC.is_toplevel pc);
  let idx_lst = List.map (fun it -> add_axiom it pc) elst in
  let pair_lst = List.map (fun idx -> PC.discharged_bubbled idx pc) idx_lst in
  let anchor =
    if is_defer then
      PC.owner pc
    else
      -1
  in
  assert (not is_defer || anchor <> -1);
  let pc0 = PC.pop pc in
  PC.add_proved_list is_defer anchor pair_lst pc0



let one_goal (elst: info_terms): info_term =
  match elst with
    [] ->
      assert false (* cannot happen *)
  | [goal] ->
      goal
  | _ :: tgt2 :: _ ->
      error_info tgt2.i "Only one goal allowed here"




let prove_goal (goal: info_term) (pc:PC.t): unit =
  verify_preconditions goal pc;
  let idx = prove_insert_report goal false pc in
  let t,pt = PC.discharged_bubbled idx pc in
  let pc0 = PC.pop pc in
  ignore (PC.add_proved t pt pc0)



let find_goals (elst: info_terms) (pc:PC.t): unit =
  assert (PC.is_toplevel pc);
  List.iter
    (fun e ->
      let chn = PC.assumptions_chain e.v pc
      and tps = PC.local_formals pc
      and fgs = PC.local_fgs pc
      in
      let t1 =
        Term.all_quantified (Formals.from_pair tps) (Formals.from_pair fgs) chn in
      let pc0 = PC.pop pc in
      let t2 = PC.prenex_term t1 pc0 in
      try
        ignore(PC.find t2 pc0)
      with Not_found ->
        error_info e.i "Not proved in the implementation file"
    )
    elst


let beta_reduced (t:term) (pc:PC.t): term =
  match t with
  | Application(Lam(tps,fgs,_,t0,rt), args, _) ->
     assert (Array.length args = 1);
     let n = Formals.count tps
     and tup_tp = Context.tuple_type_of_types (Formals.types tps) (PC.context pc) in
     PC.beta_reduce n t0 tup_tp args 0 pc
  | _ ->
      t


(* Support functions for induction proofs *)


type inductive_set_data =
    {pc:      PC.t;
     goal:    term;
     goal_predicate: term; (* [element in goal_predicate] reduces to [goal] *)
     other_vars: int array;
     ass_lst: int list;
     element: term;
     set:     term;        (* as written in the inpect expression *)
     set_expanded: term;   (* the inductive set '{(p): r0, r1, ... }' *)
     rules:  term array;   (* the rules *)
     induction_rule: int;  (* index of the assertion of the induction rule *)
     element_in_set: int   (* assertion which proves [element in set] *)
   }




let inner_case_context
    (ass_lst_rev: int list) (* List of assumptions already in the middle context *)
    (hypo_lst_rev: int list)(* List of induction hypotheses *)
    (case_goal_pred: term)  (* case goal predicate *)
    (nass:int)              (* additional assumptions in the goal predicate *)
    (ass_no_ivar: bool)     (* Do the assumptions not contain the induction
                               variables? *)
    (pc:PC.t)
    : int list * term * PC.t (* assumptions, goal, inner context *)
    =
  let stren_goal = PC.beta_reduce_term case_goal_pred pc in
  let tps1,fgs1,chn =
    Term.all_quantifier_split_1 stren_goal in
  let n1 = Formals.count tps1 in
  assert (fgs1 = Formals.empty);
  let pc1 = PC.push_typed0 tps1 fgs1 pc in
  let complete_ass_lst_rev, goal =
    interval_fold
      (fun (alst,chn) _ ->
        let a,chn =
          try
            PC.split_implication chn pc1
          with Not_found ->
            assert false (* cannot happen *)
        in
        PC.add_assumption a true pc1 :: alst,
        chn
      )
      (ass_lst_rev, chn)
      0
      nass
  in
  if n1 <> 0 && (nass = 0 || ass_no_ivar) then
    (* Add specialized induction hypotheses *)
    List.iter
      (fun hypo_idx ->
        (* Induction hypothesis has the form 'all(y,...) r1 ==> r2 ==> ... ==>
           goal' and the induction variable does not occur in the
           assumptions. *)
        let hypo = PC.term hypo_idx pc1 in
        if not (Term.is_all_quantified hypo) then
          () (* Must be a precondition of a constructor rule of a pseudo
                inductive type. A precondition does not contain the goal
                predicate and therefore is not universally quantified.
              *)
        else
          begin
            assert (Term.is_all_quantified hypo);
            let idx =
              let reason = if nass = 0 then 2 else 0 in
              PC.specialized hypo_idx (standard_substitution n1) [||] reason pc1
            in
            let rec use_local_ass_from (i:int) (idx:int): int =
              if i = nass then
                idx
              else
                begin
                  let search = if i + 1 = nass then true else false in
                  let a_idx = i + PC.count_previous pc1 in
                  let idx = PC.add_mp a_idx idx search pc1 in
                  use_local_ass_from (i+1) idx
                end
            in
            let idx = use_local_ass_from 0 idx in
            PC.add_to_work idx pc1
          end
      )
      (List.rev hypo_lst_rev);
  (* Now we have context [all(other_vars) require a1; a2; ...] *)
  PC.close pc1;
  complete_ass_lst_rev, goal, pc1




let analyze_type_inspect
    (info:info)
    (ivar:int) (* induction variable *)
    (goal_pred:term)
    (pc:PC.t)
    : IntSet.t * int * type_term =
  (* constructor set, specialized induction law, inductive type *)
  let c     = PC.context pc in
  let nvars = Context.count_variables c
  and ct    = Context.class_table c
  in
  assert (ivar < nvars);
  let tp = Context.variable_type ivar c in
  let cls = Context.variable_class ivar c in
  let gen_ind_idx, cs, _ =
    try
      Class_table.primary_induction_law cls ct
    with Not_found ->
      let str = ST.string (Context.variable_name ivar c) in
      error_info info ("The type of \"" ^ str ^ "\" has no induction law")
  in
  let ind_idx = PC.specialize_induction_law gen_ind_idx goal_pred ivar pc
  and cons_set =
    Array.fold_left
      (fun set (_,cons,_) ->
        IntSet.add cons set
      )
      IntSet.empty
      cs
  in
  cons_set, ind_idx, tp


let analyze_type_case_pattern
    (e:expression)
    (cons_set:IntSet.t)
    (tp:type_term)
    (pc:PC.t)
    : int * names =
  (* cons_idx, names *)
  let nms,pat =  Typer.case_pattern e tp (PC.context pc) in
  let nvars = PC.count_variables pc
  and n = Array.length nms
  in
  let invalid_pat () =
    error_info e.i
      ("Invalid pattern \"" ^ (string_of_expression e) ^ "\"") in
  let cons_idx =
    match pat with
      VAppl(i,args,_,_) ->
        let argslen = Array.length args in
        if argslen <> n then invalid_pat ();
        for k = 0 to n-1 do
          if args.(k) <> Variable k then invalid_pat ()
        done;
        let cons_idx = i - nvars - n in
        if not (IntSet.mem cons_idx cons_set) then invalid_pat ();
        cons_idx
    | _ ->
        invalid_pat ()
  in cons_idx, nms



let find_constructor_rule
      (cons_idx:int)  (* constructor *)
      (src_nms: names)
      (law_idx:int)   (* specialized induction law *)
      (pc:PC.t)
    : formals0 * term list * term * term =
  (* arguments, ind hypos reversed, pattern, target *)
  let tps,_,ps_rev0,tgt0 =
    PC.split_general_implication_chain (PC.term law_idx pc) pc in
  let n0 = Formals.count tps in
  assert (n0 = 0);
  let rec find_rule ps_rev0 =
    match ps_rev0 with
    | [] ->
       assert false (* cannot happen, there must be one rule for [cons_idx] *)
    | rule :: ps_rev0 ->
       let tps,_,ps_rev,tgt = PC.split_general_implication_chain rule pc in
       let n = Formals.count tps
       and tps = Formals.types tps in
       match tgt with
       | Application (gp, [|VAppl(i,args,ags,oo)|], _)
            when cons_idx + n + (PC.count_variables pc) = i ->
          assert (n = Array.length src_nms);
          let nms = Array.make n (-1) in
          Array.iteri
            (fun i arg ->
               match arg with
               | Variable j ->
                  assert (j < n);
                  nms.(j) <- src_nms.(i)
               | _ ->
                  assert false (* cannot happen *)
            )
            args;
          (nms,tps), ps_rev, (VAppl(i,args,ags,oo)), tgt
       | _ ->
          find_rule ps_rev0
  in
  find_rule ps_rev0



let inductive_type_case_context
    (cons_idx: int)     (* constructor *)
    (ind_idx: int)      (* index of the specialized induction law *)
    (nms: names)        (* argument names of the constructor *)
    (tp:  type_term)    (* type of the induction variable *)
    (goal_pred:term)
    (nass: int)         (* number of assumptions in the goal predicate *)
    (ass_no_ivar: bool) (* Do the assumptions not contain the induction variable?*)
    (pc:PC.t)
    : int list * term * term * term * PC.t
      (* assumptions, goal, (inner context),
         pattern, case_goal_pred (middle context),
         inner context *)
    =
  (* Prepare the inner context and return the reversed list of assumptions,
     the goal and the inner context (2 levels deeper than the inspect context).

     goal_pred: p = {x: all(y,...) r1 ==> r2 ==> ... ==> goal}

         case cons_i(...)
            all(cvars)
                require
                    cond     -- optional precondition
                    p(ra1)   -- induction hypothesis ra1: recursive argument 1
                    p(ra2)
                    ...
                ensure
                    p(cons_i(...))
                assert
                    'p(ra1) beta reduced'
                    'p(ra2) beta reduced'
                    ...
                    all(y,...)
                        require
                            r1
                            r2
                            ...
                        ensure
                            goal[x:=cons_i(...)]
                        assert
                          'p(ra1) beta reduced and specialized'
                          'p(ra2) beta reduced and specialized'
                          ...
                           ...  -- <- user proof
                        end
                end
   *)
  let tps,ps_rev,pat,case_goal_pred =
    find_constructor_rule cons_idx nms ind_idx pc
  in
  let pc1 = PC.push_typed0 (Formals.from_pair tps) Formals.empty pc
  in
  let ind_hyp_idx_lst =
    List.fold_left
      (fun lst hypo ->
        let search =
          match hypo with
          | Application (Lam _, [|Variable _|],_) ->
             false
          | _ ->
             true
        in
        let idx = PC.add_assumption hypo search pc1 in
        idx :: lst
      )
      []
      (List.rev ps_rev)
  in
  let hyp_lst_rev =
    List.fold_left
      (fun lst idx ->
        try
          (PC.try_add_beta_reduced idx true pc1) :: lst
        with Not_found ->
          idx :: lst
      )
      []
      (List.rev ind_hyp_idx_lst)
  in
  PC.close pc1;
  let ass_lst_rev, goal, pc2 =
    inner_case_context hyp_lst_rev hyp_lst_rev case_goal_pred nass ass_no_ivar pc1
  in
  ass_lst_rev, goal, pat, case_goal_pred, pc2





let induction_goal_predicate
    (vars:int array)              (* induction variables *)
    (others:int list)             (* other variables *)
    (ass_lst:int list)            (* list of assumptions *)
    (nvars_total: int)            (* number of variables in the used contexts *)
    (goal: term)
    (pc:PC.t)
    : term * int array =
  (* Generate the goal predicate:

        {vars: all(others) a1 ==> a2 ==> ... ==> goal}

   *)
  (*assert (ass_lst <> [] || others = []);*)
  let c = PC.context pc in
  let chn =
    PC.implication_chain
      (List.rev_map (fun idx -> PC.term idx pc) ass_lst)
      goal
      pc
  in
  let others =
    Term.used_variables_filtered
      chn
      (fun i -> IntSet.mem i (IntSet.of_list others))
      false
    |>  List.rev |> Array.of_list
  in
  let nothers = Array.length others
  and nvars   = Array.length vars in
  let chn =
    let map =
      interval_fold
        (fun map i ->
          let ivar =
            if i < nothers then
              others.(i)
            else
              vars.(i-nothers) in
          IntMap.add ivar i map
        )
        IntMap.empty
        0 (nothers + nvars)
    in
    Term.lambda_inner_map chn map
  in
  let varnme arr i = Context.variable_name arr.(i) c
  and vartp  arr i = Context.variable_type arr.(i) c in
  let nms_inner = Array.init nothers (varnme others)
  and tps_inner = Array.init nothers (vartp  others)
  and nms_outer = Array.init nvars   (varnme vars)
  and tps_outer = Array.init nvars   (vartp  vars)
  in
  let t =
    Term.all_quantified (Formals.make nms_inner tps_inner) Formals.empty chn
  in
  let t =
    Context.make_lambda
      (Formals.make nms_outer tps_outer) Formals.empty [] t None c
  in
  t, others





let inductive_set
    (info:info) (set:term) (c:Context.t)
    : term * type_term * term array =
  (* Check if the term [set] represents an inductively defined set. If yes,
     return the set term expanded, the type of the set and the rules
     transformed to the context. *)
  try
    let set_exp = Context.inductive_set set c in
    begin
      match set_exp with
        Indset (nme,tp,rs) ->
          let rs = Array.map (fun r -> Term.apply r [|set|]) rs in
          set_exp, tp, rs
      | _ ->
          assert false (* cannot happen *)
    end
  with Not_found ->
    error_info info ("\"" ^ (Context.string_of_term set c) ^
                     "\" does not evaluate to an inductive set")


let inductive_set_elements
      (info:info)
      (set:term)
      (elem:term)
      (c:Context.t)
    : term array * int list =
  (* Check the inductive set elements [elem] and return an element array with
     a list of positions (in reversed order) which need to be substituted by
     new variables.  *)
  let open Format in
  begin
    (* Check that [set] and [elem] do not have common variables. *)
    let nvars = Context.count_variables c in
    let common_vars =
      IntSet.inter
        (IntSet.of_list (Term.used_variables elem nvars))
        (IntSet.of_list (Term.used_variables set nvars))
    in
    if not (IntSet.is_empty common_vars) then
      begin
        eprintf "@[<v>%s: %s@,%s@,@,   %s@,@,%s@,@,   %s@,@,%s@,@,   %s@]@."
                (info_string info)
                "Induction error"
                "The inductive set"
                (Context.string_of_term set c)
                "must not have common variables with the element(s)"
                (Context.string_of_term elem c)
                "but there the common variable(s)"
                (String.concat
                   ", "
                   (List.map
                      (fun i -> ST.string (Context.variable_name i c))
                      (IntSet.elements common_vars)));
        exit 0
      end
  end;
  let elems = Context.args_of_tuple elem c in
  let vlst,elst =
    interval_fold
      (fun (vlst,elst) i ->
        match elems.(i) with
        | Variable j ->
           if List.mem j vlst then
             vlst,
             i :: elst
           else
             j :: vlst,
             elst
        | _ ->
           vlst,
           i :: elst
      )
      ([],[]) 0 (Array.length elems)
  in
  elems, elst




let inductive_set_context
    (info: info)
    (elem: term)
    (set:  term)
    (insp: term)
    (user_goal: term)
    (pc:PC.t)
    : inductive_set_data =
  (* Analyzes the outer part of an inductive set proof.

        inspect
            elem in set  -- elem must be either a variable or a tuple of variables
                         -- The variables in 'elem' must not appear in 'set'.
        ...
   *)
  assert (not (PC.is_global pc));
  let c    = PC.context pc in
  let nvars = Context.count_variables c in
  let set_expanded, set_tp, rules = inductive_set info set c in
  let goal_pred, other_vars, ass_lst =
    let vars =
      Array.map
        (fun v -> match v with
                  | Variable i -> i
                  | _ -> assert false)
        (Context.args_of_tuple elem c)
    in
    let ass_lst, other_vars, nvars_total =
      let insp_vars = Term.used_variables elem nvars in
      let insp_vars = Term.used_variables_0 set nvars insp_vars in
      PC.assumptions_for_variables vars insp_vars user_goal pc in
    let goal_pred, other_vars =
      induction_goal_predicate
        vars
        other_vars
        ass_lst
        nvars_total
        user_goal
        pc
    in
    goal_pred, other_vars, ass_lst
  in
  let pa = Application(set,[|elem|],false) in
  let pa_idx = prove_insert_report_base0 info pa insp false pc in
  let ind_idx = PC.add_set_induction_law set goal_pred elem pc in
  {pc;
   goal           = user_goal;
   goal_predicate = goal_pred;
   other_vars;
   ass_lst;
   set;
   set_expanded;
   element        = elem;
   rules;
   induction_rule = ind_idx;
   element_in_set = pa_idx;
 }




let inductive_set_case
    (case_exp: expression)
    (data: inductive_set_data)
    : int * term =
  let c = PC.context data.pc in
  let rule = (Typer.boolean_term case_exp c).v in
  let irule =
    try
      interval_find
        (fun i -> Term.equivalent data.rules.(i) rule)
        0
        (Array.length data.rules)
    with Not_found ->
      error_info case_exp.i "Invalid case"
  in
  irule, rule



let type_cases
      (ivar: int)
      (cases:one_case list)
      (c:Context.t)
    : (int*names*info*source_proof) list * int list =
  (* Return the reversed list of cases and a corresponding list of
     constructors. *)
  let tp = Context.variable_type ivar c
  and nvars = Context.count_variables c
  in
  List.fold_left
    (fun (lst,cs) (e,prf) ->
      let nms,pat = Typer.case_pattern e tp c in
      let n = Array.length nms in
      let invalid_pat str =
        error_info
          e.i
          (str ^ " pattern \"" ^ (string_of_expression e) ^ "\"")
      in
      let cons_idx =
        match pat with
        | VAppl(i,args,_,_) ->
           let argslen = Array.length args in
           if argslen <> n then
             invalid_pat "Invalid";
           for k = 0 to n-1 do
             if args.(k) <> Variable k then
               invalid_pat "Invalid"
           done;
           i - nvars - n
        | _ ->
           invalid_pat "Invalid"
      in
      try
        ignore (List.find (fun (c,_,_,_) -> cons_idx = c) lst);
        invalid_pat "Duplicate"
      with Not_found ->
        (cons_idx,nms,e.i,prf) :: lst, cons_idx::cs
    )
    ([],[])
    cases

(*
module TID =
  struct
    module RD = Rule_data
    type t = {
        ivar:int; (* induction variable *)
        goal:term;
        goal_pred: term;
        law_idx: int;  (* specialized law *)
        cs: int list;  (* constructors *)
        ps: term list; (* premises of the specialized induction law *)
        tgt: term;     (* target of the specialized induction law *)
        case_lst: (int * names * info * source_proof) list;
        pc: PC.t   (* outer context *)
      }

    let induction_type (data:t): type_term =
      Context.variable_type data.ivar (PC.context data.pc)

    let constructor_rule (c:int) (data:t): names * types * term list * term =
      let _,pp =
        Mylist.find2
          (fun c0 _ -> c = c0)
          data.cs
          data.ps
      in
      let n,(nms,tps),fgs,ps_rev,tgt =
        PC.split_general_implication_chain pp data.pc
      in
      assert (fgs = empty_formals);
      nms,tps,ps_rev,tgt

    let make
          (info:info) (ivar:int) (goal:term) (cases:one_case list) (pc:PC.t)
        : t =
      let c = PC.context pc in
      let ct = Context.class_table c in
      let ass_idx_lst, other_vars =
        PC.assumptions_for_variables [|ivar|] [ivar] goal pc in
      let goal_pred =
        induction_goal_predicate [|ivar|] other_vars ass_idx_lst goal pc
      and tp = Context.variable_type ivar c
      and tvs = Context.tvars c
      in
      let cls = Tvars.principal_class tp tvs
      in
      let case_lst_rev, cs_rev = type_cases ivar cases c
      in
      let law_idx0,cs =
        try
          Class_table.find_induction_law cs_rev cls ct
        with Not_found ->
          error_info
            info
            ("The class " ^ (Class_table.class_name cls ct)
             ^ " does not have an induction law for the chosen constructors")
      in
      let law_idx =
        let sub = [|goal_pred; Variable ivar|] in
        let ags =
          try
            RD.verify_specialization sub c (PC.rule_data law_idx0 pc)
          with Not_found ->
            assert false
        in
        PC.specialized law_idx0 sub ags 0 pc
      in
      let ps_rev,tgt =
        Term.split_implication_chain
          (PC.term law_idx pc)
          (PC.count_variables pc + Constants.implication_index)
      in
      assert (List.length cs = List.length ps_rev);
      {ivar; goal; goal_pred; law_idx; tgt;
       ps = List.rev ps_rev;
       cs = List.map (fun (_,c) -> c) cs;
       case_lst = List.rev case_lst_rev;
       pc}
  end (* TID *)
 *)

let error_string_case (ps_rev:term list) (goal:term) (pc:PC.t): string =
  let psstr = String.concat "\n"
      (List.rev_map
         (fun ass -> (PC.string_of_term (beta_reduced ass pc) pc))
         ps_rev)
  and tgtstr = PC.string_of_term (beta_reduced goal pc) pc in
  "\n" ^ psstr ^ "\n--------------------------\n" ^ tgtstr




let add_set_induction_hypothesis
    (hypo_idx:int)
    (pc:PC.t)
    : int =
  (* The induction hypothesis has the form

         all(hypo_vars) d1 ==> ... ==>
                {ind_vars: all(other_vars) a1 ==> ... ==> user_goal}(ind_vars)

     and has been added to the context of the case at [hypo_idx].

     We have to add the following assertion to the context and return its index:

         all(hypo_vars, other_vars) a1 ==> a2 ==> ... ==> d1 ==> ... ==> goal
    *)
  let hypo = PC.term hypo_idx pc
  in
  let fargs1,fgs1,ps_rev1,goal_redex1 =
    PC.split_general_implication_chain hypo pc
  in
  let n1 = Formals.count fargs1 in
  assert (fgs1 = Formals.empty);
  let pc1 = PC.push_typed0 fargs1 fgs1 pc
  in
  match goal_redex1 with
    Application(Lam(_),_,_) ->
      let outer_goal = PC.beta_reduce_term goal_redex1 pc1
      in
      let fargs2,fgs2,ps_rev2,user_goal =
        PC.split_general_implication_chain outer_goal pc1
      in
      let n2 = Formals.count fargs2 in
      assert (fgs2 = Formals.empty);
      let pc2 = PC.push_typed0 fargs2 fgs2 pc1
      in
      (* Now we have two contexts: all(hypo_vars)  all(other_vars *)
      let alst_rev =
        List.rev_map
          (fun a -> PC.add_assumption a false pc2)
          (List.rev ps_rev2)
      in
      let dlst_rev =
        List.rev_map
          (fun d -> PC.add_assumption (Term.up n2 d) false pc2)
          (List.rev ps_rev1)
      in
      (* Now we have a1; a2; ... ; d1; d2, ... as assumptions in the context and
         can specialize the induction hypothesis hypo_idx.*)
      let gen_goalpred_idx =
        let spec_hypo_idx =
          let args = Array.init n1 (fun i -> Variable (i + n2)) in
          PC.specialized hypo_idx args [||] 0 pc2
        in
        List.fold_left
          (fun idx d_idx ->
            PC.add_mp d_idx idx false pc2)
          spec_hypo_idx
          (List.rev dlst_rev)
      in
      let gen_goal_idx = PC.add_beta_reduced gen_goalpred_idx false pc2 in
      let chn_goal_idx =
        let args = standard_substitution n2 in
        PC.specialized gen_goal_idx args [||] 0 pc2
      in
      let goal_idx =
        List.fold_left
          (fun idx a_idx -> PC.add_mp a_idx idx false pc2)
          chn_goal_idx
          (List.rev alst_rev)
      in
      let t,pt = PC.discharged_bubbled goal_idx pc2 in
      let idx = PC.add_proved_term t pt false pc1 in
      let t,pt = PC.discharged_bubbled idx pc1 in
      PC.add_proved_term t pt true pc
  | _ ->
      hypo_idx



let string_of_case_context
    (prefix: string)
    (ass_lst_rev: int list)
    (goal: term)
    (pc:PC.t)
    : string =
  let ass_str_lst =
    List.fold_left
      (fun lst a_idx ->
        let str = PC.string_long_of_term_i a_idx pc in
        let str =
          if PC.is_trace_extended pc then
            str ^ "\n" ^ prefix ^ "  " ^ Term.to_string (PC.term a_idx pc)
          else
            str
        in
        if List.mem str lst then
          lst
        else
          str :: lst
      )
      []
      ass_lst_rev
  in
  let ass_str = String.concat ("\n" ^ prefix) ass_str_lst
  and goal_str =
    PC.string_long_of_term goal pc
  in
  prefix ^ "variables  " ^ (PC.arguments_string pc) ^ "\n"
  ^ prefix ^ "-------------------------------\n"
  ^ prefix ^ ass_str ^ "\n"
  ^ prefix ^ "-------------------------------\n"
  ^ prefix ^ goal_str ^ "\n"
  ^ (if PC.is_trace_extended pc then
       prefix ^  "  "  ^ Term.to_string goal ^ "\n"
     else
       "")



let inductive_set_case_context
    (set:  term)
    (set_expanded: term)
    (rule:term)
    (irule:int)
    (nass: int)
    (goal_pred:term)
    (pc: PC.t):
    int list * term * term * PC.t  (* assumptions, goal, inner context *)
    =
  (*
    Prepare the inner context and return the reversed list of assumptions,
    the goal and the inner context (2 levels deeper than the inspect context).

    The rule has the form

        all(rule vars) c1 ==> c2 ==> ... ==> p(e)

    The goal predicate has the form

       {ind vars: all(other vars) a1 ==> a2 ==> ... ==> user_goal}

    with [nass] assumptions before the user goal.

    The new context has the form

        all(rule vars)
            require
                c1(set)
                c1(goal_pred)    -- ind hypo 1
                c2(set)
                c2(goal_pred)    -- ind hypo 2
                ...
                e in set
            proof
                all(other vars)
                    require
                        a1
                        a2
                        ...
                    proof
                        ind hypo 1 in user terms
                        ind hypo 2 in user terms
                        ...
                        ...  -- <-- here the user proof starts to prove the
                             --     goal

    where

         ci:
             all(hypo vars) d1i ==> d2i ==> ... ==> p(ei)

         ind hypo i:
             all(hypo vars) d1i ==> ... ==> goal_pred(ei)
   *)
  let n1,fargs1,ps,goal_pred1 =
    let nvars = PC.count_variables pc in
    let imp_id = nvars + Constants.implication_index in
    let tps, ps, goal_pred1 =
      Term.induction_rule imp_id irule set_expanded set goal_pred
    in
    let n = Formals.count tps
    and tps = Formals.types tps in
    let tps_rule,_,_ = Term.all_quantifier_split_1 rule in
    assert (n = Formals.count tps_rule);
    n,(Formals.names tps_rule,tps), ps, goal_pred1
  in
  let pc1 = PC.push_typed0 (Formals.from_pair fargs1) Formals.empty pc in
  (* add induction hypotheses *)
  let ass_lst_rev, hlst_rev, _ =
    List.fold_left
      (fun (alst, hlst, is_hypo) p ->
        if is_hypo then
          let idx = PC.add_assumption p false pc1 in
          alst, idx::hlst, false
        else begin
          let idx = PC.add_assumption p true pc1 in
          idx::alst, hlst, true
        end
      )
      ([],[],false)
      ps
  in
  (* add induction hypotheses in usable form *)
  let ass_lst_rev =
    List.fold_left
      (fun alst idx ->
        let hidx = add_set_induction_hypothesis idx pc1 in
        hidx::alst
      )
      ass_lst_rev
      (List.rev hlst_rev)
  in
  PC.close pc1;
  (* Now we have context [all(rule_vars) require c1(set); c1(q); ... set(e)] *)
  let ass_lst_rev, goal, pc2 =
    inner_case_context ass_lst_rev [] goal_pred1 nass false pc1
  in
  ass_lst_rev, goal, goal_pred1, pc2


(* Support functions for transitivity proofs *)

let get_transitivity_data
    (info:info)
    (goal:term)
    (pc:PC.t)
    : int * term * term * type_term * agens * (term -> term -> term) =
  (* Analyze the goal which should have the form 'r(a,b)' (if not report an error)
     and return

     Index of the transitivity law 'all(a,b,c:G) r(a,b) ==> r(b,c) ==> r(a,c)

     The terms 'a' and 'b' and the type of the terms

     The actual generics (0 or 1) which are needed to substitute the formal
     generics (0 or 1) of the transitivity law.

     A function which maps two terms 'x' and 'y' to 'r(x,y)'
   *)
  let error str =
    error_info info ("The goal \"" ^ (PC.string_of_term goal pc) ^
                     "\" " ^ str)
  in
  let error_not_proper () =
    error "is not a proper binary boolean expression"
  in
  let c = PC.context pc in
  let nvars = Context.count_variables c
  in
  let find_law (rel:int -> term -> term -> term) (tp:type_term): int*agens =
      let a = Variable 0
      and b = Variable 1
      and c = Variable 2
      and imp_id = 3 + nvars
      and nms = standard_argnames 3
      and tps = [|tp;tp;tp|]
      in
      let pc1 = PC.push_typed0 (Formals.make nms tps) Formals.empty pc in
      let t =
        let ab = rel 3 a b
        and bc = rel 3 b c
        and ac = rel 3 a c in
        Term.binary imp_id ab (Term.binary imp_id bc ac)
      in
      let idx_law,ags =
        try
          PC.find_schematic t 3 pc1
        with Not_found ->
          let law =
            Term.all_quantified
              (Formals.make (standard_argnames 3) [|tp;tp;tp|])
              Formals.empty
              t
          in
          error_info info ("Cannot find the transitivity law\n\t\"" ^
                           (PC.string_long_of_term law pc) ^
                           "\"")
      in
      idx_law, ags
  in
  match goal with
    Application (p, [|arg|], _) ->
      let p_tp = Context.type_of_term p c in
      let cls,ags = split_type p_tp in
      if cls = Context.predicate_class c then begin
        assert (Array.length ags = 1);
        let ft = Context.feature_table c in
        let tup_tp = Class_table.domain_type p_tp in
        let args = Feature_table.args_of_tuple arg nvars ft in
        if Array.length args <> 2 then
          error_not_proper ();
        let tp = Context.type_of_term args.(0) c in
        if not (Term.equivalent tp (Context.type_of_term args.(1) c)) then
          error_not_proper ();
        let rel nb a b =
          let p = Term.up nb p in
          let arg = Feature_table.tuple_of_args [|a;b|] tup_tp (nb+nvars) ft in
          Application (p, [|arg|],false)
        in
        let idx_law,ags = find_law rel tp in
        idx_law, args.(0), args.(1), tp, ags, rel 0
      end else
        error_not_proper ()
  | VAppl (idx, [|a1;a2|], ags,oo)
    when Term.equivalent
        (Context.type_of_term a1 c)
        (Context.type_of_term a2 c)
    ->
      let tp = Context.type_of_term a1 c in
      let mkterm nb a b = VAppl (nb+idx,[|a;b|],ags,oo) in
      let idx_law,ags = find_law mkterm tp in
      idx_law, a1, a2, tp, ags, mkterm 0
  | _ ->
      error_not_proper ()


let prove_transitivity_step
    (info:info)
    (idx_law:int) (* index of 'all(a,b,c:T) r(a,b) ==> r(b,c) ==> r(a,c)' *)
    (idx_rab:int) (* index of 'r(a,b)' *)
    (a: term)
    (b: term)
    (c: term)
    (ags:agens)
    (r: term -> term -> term)
    (pc:PC.t)
    : int =
  (* It is assumed that 'r(a,b)' has been proved. The function proves 'r(b,c)'
     and uses the transitivity law to prove 'r(a,c)'.
   *)
  let rbc = r b c in
  let idx_rbc = prove_insert_report_base info rbc false pc
  in
  let spec_law = PC.specialized idx_law [|a;b;c|] ags 0 pc
  in
  let idx = PC.add_mp idx_rab spec_law false pc
  in
  PC.add_mp idx_rbc idx false pc




(* Mutually recursive prover functions *)

let rec prove_and_store
    (entlst: entities list withinfo)
    (rlst: compound)
    (elst: compound)
    (prf:  source_proof)
    (pc:PC.t)
    : unit =
  if PC.is_public pc then begin
    match prf with
      SP_Deferred | SP_Proof ([],None) ->
        ()
    | SP_Axiom ->
        error_info entlst.i "Axiom not allowed in interface file"
    | _ ->
        error_info entlst.i "Proof not allowed in interface file"
  end;
  let rlst, elst, pc1 = push entlst rlst elst pc in
  add_assumptions rlst pc1;
  let prove_goal () =
    PC.close pc1;
    let goal = one_goal elst in
    verify_preconditions goal pc1;
    let idx =
      try
        prove_one goal.v prf pc1
      with  Proof.Proof_failed msg ->
        error_info goal.i ("Cannot prove" ^ msg)
    in
    let t,pt = PC.discharged_bubbled idx pc1 in
    ignore (PC.add_proved t pt pc)
  in
  match prf with
  | SP_Axiom ->
      store_unproved false elst pc1
  | SP_Deferred ->
      store_unproved true elst pc1
  | SP_Proof([],None) ->
      if PC.is_interface_use pc1 then
        store_unproved false elst pc1
      else if PC.is_interface_check pc1 then begin
        find_goals elst pc1
      end else
        prove_goal ()
  | _ ->
        prove_goal ()


and prove_one
    (goal:term) (prf:source_proof) (pc:PC.t)
    : int =
  (* Prove [goal] with the proof expression [prf]. Assume that the preconditions
     of the goal are already verified. *)
  assert (PC.is_private pc);
  let result idx =
    let ok = Term.equivalent goal (PC.term idx pc) in
    if not ok then
      begin
        printf "proved %s  %s\n"
               (PC.string_long_of_term (PC.term idx pc) pc)
               (Term.to_string (PC.term idx pc));
        assert (PC.is_well_typed (PC.term idx pc) pc);
        printf "goal   %s  %s\n"
               (PC.string_long_of_term goal pc)
               (Term.to_string goal);
        assert (PC.is_well_typed goal pc);
      end;
    assert (Term.equivalent goal (PC.term idx pc));
    idx
  in
  match prf with
    SP_Axiom | SP_Deferred -> assert false (* cannot happen here *)
  | SP_Proof (lst, pexp) ->
      prove_sequence lst pc;
      begin match pexp with
        None ->
          result (Prover.prove_and_insert goal pc)
      | Some prf ->
          try
            result (
              match prf.v with
                PE_If (cond, sprf1, else_info, sprf2) ->
                  prove_if prf.i goal cond sprf1 else_info sprf2 pc
              | PE_Guarded_If (cond1, sprf1, cond2, sprf2) ->
                  prove_guarded_if prf.i goal cond1 sprf1 cond2 sprf2  pc
              | PE_Inspect (insp, cases) ->
                  prove_inspect prf.i goal insp cases pc
              | PE_Existential (entlst, reqs, prf1) ->
                  prove_exist_elim prf.i goal entlst reqs prf1 pc
              | PE_Contradiction (exp,prf1) ->
                  prove_contradiction prf.i goal exp prf1 pc
              | PE_Transitivity lst ->
                  prove_by_transitivity prf.i goal lst pc
            )
          with Proof.Proof_failed msg ->
            error_info prf.i ("Does not prove \"" ^
                              (PC.string_of_term goal pc) ^
                              "\"" ^ msg)
      end


and prove_sequence
    (lst: proof_step list)
    (pc: PC.t): unit =
  assert (PC.count_type_variables pc = 0);
  let expand (i:int): unit =
    PC.expand_variable_definitions i pc
  in
  List.iter
    (fun step ->
      begin match step with
        PS_Simple ie ->
          let it = get_boolean_term_verified ie pc in
          expand (prove_insert_report it true pc)
      | PS_Structured (entlst,rlst,tgt,prf) ->
          let rlst, elst, pc1 = push entlst rlst [tgt] pc in
          add_assumptions rlst pc1;
          PC.close pc1;
          let goal = List.hd elst in
          verify_preconditions goal pc1;
          let idx =
            try
              prove_one goal.v prf pc1
            with Proof.Proof_failed msg ->
              error_info goal.i ("Cannot prove" ^ msg)
          in
          let t,pt = PC.discharged_bubbled idx pc1 in
          expand (PC.add_proved t pt pc)
      end;
      PC.close pc
    )
    lst


and prove_guarded_if
    (info: info)
    (goal: term)
    (c1:expression) (prf1:source_proof)
    (c2:expression) (prf2:source_proof)
    (pc:PC.t)
    : int =
  let c1 = get_boolean_term_verified c1 pc
  and c2 = get_boolean_term_verified c2 pc
  in
  let or_exp = PC.disjunction c1.v c2.v pc in
  let or_exp_idx =
    prove_insert_report (withinfo info or_exp) false pc
  and or_elim_idx =
    try
      PC.or_elimination pc
    with Not_found ->
      error_info info "Or elimination law not available"
  in
  let result_idx =
    PC.specialized or_elim_idx [|c1.v;c2.v;goal|] [||] 0 pc in
  let result_idx =
    PC.add_mp or_exp_idx result_idx false pc in
  let result_idx =
    let branch1_idx = prove_branch c1 goal prf1 pc in
    PC.add_mp branch1_idx result_idx false pc in
  let branch2_idx = prove_branch c2 goal prf2 pc in
  PC.add_mp branch2_idx result_idx false pc


and prove_if
    (info: info)
    (goal: term)
    (c1:expression)
    (prf1:source_proof)
    (else_info:info)
    (prf2:source_proof)
    (pc:PC.t)
    : int =
  let c1 = get_boolean_term_verified c1 pc
  in
  let c1neg = PC.negation c1.v pc
  in
  let em_idx =
    try
      PC.excluded_middle pc
    with Not_found ->
      error_info info "Excluded middle law not available"
  and or_elim_idx =
    try
      PC.or_elimination pc
    with Not_found ->
      error_info info "Or elimination law not available"
  in
  let spec_em_idx =
    PC.specialized em_idx [|c1.v|] [||] 0 pc
  and spec_or_elim_idx =
    PC.specialized or_elim_idx [|c1.v;c1neg;goal|] [||] 0 pc
  in
  let result_idx =
    PC.add_mp spec_em_idx spec_or_elim_idx false pc in
  let result_idx =
    let branch1_idx = prove_branch c1 goal prf1 pc in
    PC.add_mp branch1_idx result_idx false pc in
  let branch2_idx = prove_branch (withinfo else_info c1neg) goal prf2 pc in
  PC.add_mp branch2_idx result_idx false pc


and prove_branch
    (cond:info_term) (goal:term) (prf:source_proof) (pc:PC.t)
    : int =
  let pc1 = PC.push_empty pc in
  ignore (PC.add_assumption cond.v true pc1);
  PC.close pc1;
  let idx =
    try
      prove_one goal prf pc1
    with Proof.Proof_failed msg ->
      error_info
        cond.i
        ("Cannot prove the goal\n\t\"" ^ (PC.string_of_term goal pc) ^
         "\"\nassuming\n\t\"" ^ (PC.string_of_term cond.v pc) ^ "\"")
  in
  let t,pt = PC.discharged_bubbled idx pc1 in
  PC.add_proved_term t pt false pc



and prove_inspect
    (info:info)
    (goal:term)
    (insp:expression) (cases:one_case list) (pc:PC.t): int =
  (* Decide if it is an induction proof with an inductively defined set or an
     inductive class. If instead of pure induction variables general expressions
     are used then a new context has to be introduced with variables which are
     equal to the expressions and the induction proof has to be executed in the
     new context.
   *)
  let insp = get_term insp pc in
  let c = PC.context pc in
  let cls = Context.class_of_term insp.v c in
  let open Format in
  if cls = Constants.boolean_class then
    begin (* Induction on the definition of a set *)
      match insp.v with
      | Application (set,args,_) ->
         assert (Array.length args = 1);
         let elems,elst_rev = inductive_set_elements insp.i set args.(0) c in
         if PC.is_tracing pc then
           begin
             let open Format in
             printf
               "@[<v>@,@,%s@[<v 4>%s@,%s@,    %s@,%s@,    %s@,@,@]@]@."
               (PC.trace_prefix pc)
               "Induction Proof with Inductive Set"
               "ensure"
               (PC.string_of_term goal pc)
               "inspect"
               (PC.string_of_term insp.v pc)
           end;
         if elst_rev <> [] then
           let nnew = List.length elst_rev in
           let nms1 = anon_argnames nnew
           and tps1 =
             Array.of_list
               (List.rev_map (fun i -> Context.type_of_term elems.(i) c) elst_rev)
           and earr =
             Array.of_list
               (List.rev_map (fun i -> elems.(i)) elst_rev)
           in
           PC.close pc;
           let pc1 = PC.push_typed0 (Formals.make nms1 tps1) Formals.empty pc
           and elems1 = Array.map (Term.up nnew) elems
           and set1 = Term.up nnew set
           and goal1 = Term.up nnew goal
           and insp1 = Term.up nnew insp.v
           in
           let c1 = PC.context pc1 in
           let eq_terms,_ =
             List.fold_left
               (fun (lst,i) j ->
                 let k = nnew - i - 1 in (* we are going in reverse order *)
                 let eq1 = Context.equality_term elems1.(j) (Variable k) c1
                 and eq  = Context.equality_term elems.(j) elems.(j) c
                 in
                 elems1.(j) <- Variable k;
                 (eq1,eq) :: lst, i + 1
               )
               ([],0)
               elst_rev
           in
           List.iter
             (fun (eq1,eq) -> ignore(PC.add_assumption eq1 true pc1))
             eq_terms;
           PC.close pc1;
           let elem1 = Context.tuple_of_args elems1 c1 in
           let idx =
             prove_inductive_set insp.i elem1 set1 insp1 goal1 cases pc1
           in
           let t,pt = PC.discharged idx pc1 in
           let idx = PC.add_proved_term t pt false pc in
           let idx = PC.specialized idx earr [||] 0 pc in
           List.fold_left
             (fun idx (eq1,eq) ->
               let eq_idx =
                 try
                   PC.find_goal eq pc
                 with Not_found ->
                   assert false (* 'exp = exp' must be found *)
               in
               PC.add_mp eq_idx idx false pc
             )
             idx
             eq_terms
         else
           let elem = Context.tuple_of_args elems c in
           prove_inductive_set insp.i elem set insp.v goal cases pc
      | _ ->
         error_info insp.i "Illegal inspect term"
    end
  else
    begin (* Induction on an inductive type *)
      try
        match insp.v with
        | Variable ivar ->
           prove_inductive_type insp.i goal ivar cases pc
        | _ ->
           not_yet_implemented insp.i "Induction on general expressions"
      with Not_found ->
        eprintf "@[<v>%s: %s@,%s@ \"%s\" %s %s %s]@."
                (info_string insp.i)
                "Type error"
                "The term"
                (PC.string_of_term insp.v pc)
                "has type"
                (Context.string_of_type (Context.type_of_term insp.v c) c)
                "which is not an inductive type";
        exit 1
    end

and prove_inductive_type
    (info:info)
    (goal:term)
    (ivar: int) (* induction variable *)
    (cases: one_case list)
    (pc:PC.t)
    : int =
  let ass_idx_lst, other_vars, nvars_total =
    PC.assumptions_for_variables [|ivar|] [ivar] goal pc in
  let goal_pred, other_vars =
    induction_goal_predicate
      [|ivar|] other_vars ass_idx_lst nvars_total goal pc
  and nass = List.length ass_idx_lst
  and ass_no_ivar =
    List.for_all
      (fun i ->
        Term.used_variables_filtered (PC.term i pc) (fun j -> j=ivar) false
        = []
      )
      ass_idx_lst
  in
  if PC.is_tracing pc then begin
    let prefix = PC.trace_prefix pc in
    printf "\n\n%sInduction Proof\n\n" prefix;
    printf "%sensure\n" prefix;
    printf "%s    %s\n" prefix (PC.string_long_of_term goal pc);
    printf "%sgoal predicate\n" prefix;
    printf "%s    %s\n" prefix (PC.string_long_of_term goal_pred pc);
    printf "%sinspect\n" prefix;
    printf "%s    %s\n\n"
      prefix
      (ST.string (Context.argnames (PC.context pc)).(ivar))
  end;
  (*let data = TID.make info ivar goal cases pc in*)
  let cons_set, ind_idx, tp =
    analyze_type_inspect info ivar goal_pred pc
  in
  let c  = PC.context pc in
  let ft  = Context.feature_table c in
  let proved_cases =
    (* explicitly given cases *)
    List.fold_left
      (fun map (ie,prf) ->
        let cons_idx, nms =
          analyze_type_case_pattern ie cons_set tp pc in
        let idx =
          prove_type_case ie.i cons_idx ind_idx nms tp prf
                          ivar goal_pred nass ass_no_ivar pc in
        IntMap.add cons_idx idx map
      )
      IntMap.empty
      cases
  in
  let idx_goal_redex =
    (* rest of the cases *)
    IntSet.fold
      (fun cons_idx ind_idx ->
        let idx =
          try
            IntMap.find cons_idx proved_cases
          with Not_found ->
            let n   = Feature_table.arity cons_idx ft in
            let nms = anon_argnames n in
            prove_type_case
              info cons_idx ind_idx nms tp (SP_Proof([],None))
              ivar goal_pred nass ass_no_ivar pc
        in
        PC.add_mp idx ind_idx false pc
      )
      cons_set
      ind_idx
  in
  let res =
    let idx  = PC.add_beta_reduced idx_goal_redex false pc in
    let vars = Array.map (fun i -> Variable i) other_vars in
    PC.specialized idx vars [||] 0 pc
  in
  let res =
    List.fold_left
      (fun res ass_idx ->
        PC.add_mp ass_idx res false pc
      )
      res
      ass_idx_lst
  in
  res



and prove_type_case
    (info:info)
    (cons_idx:int)
    (ind_idx:int)    (* index of the specialized induction law *)
    (nms: names)     (* The names of the arguments of the constructor *)
    (tp:type_term)   (* inductive type in the outer context *)
    (prf:source_proof)
    (ivar:int)       (* induction variable *)
    (goal_pred:term) (* in the outer context *)
    (nass:int)       (* number of assumptions in the goal predicate *)
    (ass_no_ivar:bool)
    (pc:PC.t)        (* outer context *)
    : int =
  (* Prove one case of an inductive type
   *)
  let ass_lst_rev, goal, pat, case_goal_pred, pc2 =
    inductive_type_case_context
      cons_idx ind_idx nms tp goal_pred nass ass_no_ivar pc
  in
  let pc1 = PC.pop pc2 in
  if PC.is_tracing pc then begin
    let prefix = PC.trace_prefix pc in
    printf "\n\n";
    printf "%scase %s\n" prefix (PC.string_of_term pat pc1);
    printf "%sgoal\n" prefix;
    printf "%s\n"
      (string_of_case_context (prefix ^ "    ") ass_lst_rev goal pc2);
  end;
  let gidx =
    try
      prove_one goal prf pc2
    with Proof.Proof_failed msg ->
      let casestr = string_of_case_context "    " ass_lst_rev goal pc2
      and patstr  = PC.string_of_term pat pc1 in
      error_info info ("Cannot prove case \"" ^ patstr ^ "\""
                       ^ msg ^ "\n" ^ casestr)
  in
  let t,pt = PC.discharged gidx pc2 in
  let idx = PC.add_proved_term t pt false pc1 in
  let idx = PC.add_beta_redex case_goal_pred idx false pc1 in
  let t,pt = PC.discharged idx pc1 in
  let res = PC.add_proved_term t pt false pc in
  if PC.is_tracing pc then
    printf "\n%scase succeeded\n\n" (PC.trace_prefix pc);
  res



and prove_inductive_set
      (info:info)
      (elem:term)
      (set:term)     (* elem in set *)
      (insp:term)    (* as originally in the source *)
      (goal:term)
      (cases: one_case list)
      (pc:PC.t)
    : int =
  (* Execute a proof with an inductive set:

         ensure
             ens
         inspect
             elem in set      -- 'elem in set' must be valid

         case         -- List of zero of more cases, each case represents a
             ...      -- rule for 'elem in set' to be valid
         proof
             ...

         ...
         end
   *)
  let data = inductive_set_context info elem set insp goal pc in
  let nrules = Array.length data.rules
  in
  let proved =
    List.fold_left
      (fun proved (ie,prf) ->
        let irule, rule = inductive_set_case ie data in
        let ass_lst_rev, goal, goal_pred, pc_inner =
          inductive_set_case_context
            data.set
            data.set_expanded
            rule
            irule
            (List.length data.ass_lst)
            data.goal_predicate
            data.pc in
        let idx =
          prove_inductive_set_case
            ie.i rule ass_lst_rev goal goal_pred prf pc_inner data.pc
        in
        IntMap.add irule idx proved
      )
      IntMap.empty
      cases
  in
  let ind_idx =
    interval_fold
      (fun ind_idx irule ->
        let rule_idx =
          try
            IntMap.find irule proved
          with Not_found ->
            let ass_lst_rev, goal, goal_pred, pc_inner =
              inductive_set_case_context
                data.set
                data.set_expanded
                data.rules.(irule)
                irule
                (List.length data.ass_lst)
                data.goal_predicate
                data.pc
            in
            prove_inductive_set_case
              info data.rules.(irule) ass_lst_rev goal goal_pred
              (SP_Proof([],None)) pc_inner data.pc
        in
        PC.add_mp rule_idx ind_idx false data.pc
      )
      data.induction_rule 0 nrules
  in
  let gidx = PC.add_mp data.element_in_set ind_idx false data.pc
  in
  let res =
    let idx = PC.add_beta_reduced gidx false data.pc in
    let vars = Array.map (fun i -> Variable i) data.other_vars in
    PC.specialized idx vars [||] 0 data.pc
  in
  let res =
    List.fold_left
      (fun res ass_idx ->
        PC.add_mp ass_idx res false data.pc
      )
      res
      data.ass_lst
  in
  res


and prove_inductive_set_case
    (info:info)
    (rule:term)                   (* in the outer context *)
    (ass_lst_rev: int list)
    (goal: term)                  (* in the inner context *)
    (goal_pred: term)             (* in the middle context *)
    (prf: source_proof)
    (pc1:PC.t)                    (* inner context *)
    (pc0:PC.t)                    (* outer context *)
    : int =
  if PC.is_tracing pc0 then begin
    let prefix = PC.trace_prefix pc0 in
    printf "\n\n";
    printf "%scase\n" prefix;
    printf "%s    %s\n" prefix (PC.string_long_of_term rule pc0);
    printf "%sgoal\n" prefix;
    printf "%s\n"
      (string_of_case_context (prefix ^ "    ") ass_lst_rev goal pc1);
  end;
  let gidx =
    try
      prove_one goal prf pc1
    with Proof.Proof_failed msg ->
      let casestr = string_of_case_context "    " ass_lst_rev goal pc1
      and rulestr = PC.string_of_term rule pc0 in
      error_info info ("Cannot prove case \"" ^ rulestr ^ "\""
                       ^ msg ^ "\n" ^ casestr)
  in
  let t,pt = PC.discharged gidx pc1 in
  let pc01 = PC.pop pc1 in
  let idx = PC.add_proved_term t pt false pc01 in
  let idx = PC.add_beta_redex goal_pred idx false pc01 in
  let t,pt = PC.discharged idx pc01 in
  let res = PC.add_proved_term t pt false pc0 in
  if PC.is_tracing pc0 then
    printf "\n%scase succeeded\n\n" (PC.trace_prefix pc0);
  res




and prove_exist_elim
    (info: info)
    (goal: term)
    (entlst: entities list withinfo)
    (req: expression)
    (prf: source_proof)
    (pc:PC.t)
    : int =
  PC.close pc;
  let someexp = (withinfo info (Expquantified (Existential,entlst,req))) in
  let someexp = get_boolean_term_verified someexp pc in
  let someexp_idx =
    try
      let t,pt = Prover.proof_term someexp.v pc in
      PC.add_proved_term t pt false pc
    with Proof.Proof_failed msg ->
      error_info
        someexp.i
        ("Cannot prove \"" ^ (PC.string_of_term someexp.v pc) ^
         "\"" ^ msg)
  in
  let elim_idx = PC.add_some_elim_specialized someexp_idx goal false pc in
  let fargs,t0 = Term.some_quantifier_split someexp.v in
  let n = Formals.count fargs in
  let pc1 = PC.push_typed0 fargs Formals.empty pc in
  ignore (PC.add_assumption t0 true pc1);
  PC.close pc1;
  let goal = Term.up n goal in
  let goal_idx = prove_one goal prf pc1 in
  let t,pt = PC.discharged goal_idx pc1 in
  let all_idx = PC.add_proved_term t pt false pc in
  PC.add_mp all_idx elim_idx false pc





and prove_contradiction
    (info: info)
    (goal: term)
    (exp:  expression)
    (prf:  source_proof)
    (pc:PC.t)
    : int =
  let exp = get_boolean_term exp pc in
  let pc1 = PC.push_empty pc in
  ignore (PC.add_assumption exp.v true pc1);
  PC.close pc1;
  let false_idx =
    try
      prove_one (PC.false_constant pc1) prf pc1
    with Proof.Proof_failed msg ->
      error_info
        info
        ("Cannot derive \"false\" from \"" ^
         (PC.string_of_term exp.v pc1) ^ "\"")
  in
  let t,pt = PC.discharged false_idx pc1 in
  ignore(PC.add_proved_term t pt true pc);
  PC.close pc;
  try
    prove_one goal (SP_Proof([],None)) pc
  with Proof.Proof_failed msg ->
    error_info
      info
      ("Cannot prove\n\t\"" ^
       (PC.string_of_term goal pc) ^
      "\"\nby assuming\n\t\"" ^
       (PC.string_of_term t pc) ^
       "\"")



and prove_by_transitivity
    (info:info)
    (goal:term)
    (lst: expression list)
    (pc:PC.t)
    : int =
  assert (lst <> []);
  let c = PC.context pc in
  let idx_law, first, last, tp, ags, rel =
    get_transitivity_data info goal pc
  in
  let lst = List.map (fun ie -> Typer.typed_term ie tp c) lst
  in
  match lst with
    b :: tail ->
      let rab = rel first b.v in
      let idx_rab = prove_insert_report_base b.i rab false pc in
      let idx_rax, x =
        List.fold_left
          (fun (idx_rab,b) c ->
            let idx_rac =
              prove_transitivity_step
                c.i idx_law idx_rab first b c.v ags rel pc
            in
            idx_rac, c.v
          )
          (idx_rab,b.v)
          tail
      in
      prove_transitivity_step
        info idx_law idx_rax first x last ags rel pc
  | _ ->
      assert false (* cannot happen *)
