(*
  Induction law:

      all(p:{T},x:T) pp1 ==> pp2 ==> ... ==> x in p

      ppi: all(a1,...) cond ==> ra1 in p ==> ... ==> ci(a1,...) in p

          cond: optional and sometimes necessary to make the cases non overlap

      T must be in its most general form

  Case recognizer:

     all(x:T) exp = some(a1:A1,...) cond and x = ci(a1,...)

     Signature of ci: (A1,...): T in its most general form

  Projector:

     all(a1,.,aj:Aj,..) proj_ij(ci(a1,...)) = aj

     Signature of projector: T -> Aj

  Wellfounded induction law

      Form a:
      all(p:{T},y:T)
          (all(y) (all(x) x < y ==> x in p) ==> y in p)
          ==>
          y in p

      Form b:
      all(p:{T},y:T)
          (all(y) cond ==> y in p)
          ==>
          (all(y) not cond ==> (all(x) x < y ==> x in p) ==> y in p)
          ==>
          y in p

 *)

open Term
open Container
open Signature
open Printf

exception Reject

let is_tracing_ft (ft:Feature_table.t): bool =
  Feature_table.verbosity ft > 1

let is_tracing (c:Context.t): bool =
  Context.verbosity c > 1


let has_all_recursive_arguments
      (rec_args:IntSet.t)
      (co:int)
      (args: term array)
      (c:Context.t)
    : bool =
  (* Are all recursive arguments of [c] contained in recargs? *)
  let ft = Context.feature_table c
  and nargs = Array.length args
  and nvars = Context.count_variables c in
  let tvs,s = Feature_table.signature0 (co-nvars) ft in
  assert (Sign.has_result s);
  let rt = Sign.result s in
  interval_for_all
    (fun i ->
      if Term.equivalent (Sign.arg_type i s) rt then
        match args.(i) with
        | Variable j when j < nargs ->
          IntSet.mem j rec_args
        | _ ->
           assert false (* cannot happen *)
      else
        true
    )
    0 (Sign.arity s)



let permute_arguments
      (args:arguments) (ags:agens) (t:term)
    : term =
  assert (Term.is_permutation args);
  assert (Term.is_permutation ags);
  Term.subst0 t
              (Array.length args) (Term.invert_permutation args)
              (Array.length ags)  (Term.invert_permutation ags)



let split_constructor_rule (pp:term) (c:Context.t): term * int * term list =
  (* Check if [pp] is a constructor rule of the form

         all(a1,...) pp1 ==> ... ==> c(a1,...) in p

     where each premise ppi is either a general condition not containing [p]
     or has the form [rai in p] where [rai] is a recursive argument of the
     constructor [c]. In case of success return the ghost recognizer, the
     constructor and the list of preconditions. Otherwise raise Not_found.

     Ghost recognizer: some(a1,...) cond1 and cond2 and ... x = c(a1,...)
   *)
  let open Context in
  let tps,fgs,ps_rev,t0 =
    split_general_implication_chain pp c
  in
  if Formals.count fgs > 0 then
    raise Not_found;
  let c1 = push_typed0 tps fgs c in
  match t0 with
  | Application(Variable n,[|VAppl(co,args,ags,oo)|],_)
       when Term.is_permutation args
            && Array.length args = n
            && Term.is_permutation ags ->
     let pres,recargs =
       List.fold_left
         (fun (pres,recargs) ppi ->
           match ppi with
           | Application (Variable n,[|Variable rai|],_) when rai < n ->
              if IntSet.mem rai recargs then
                raise Not_found;
              pres, IntSet.add rai recargs
           | _ ->
              (* extract a precondition *)
              try
                ignore(Term.down_from 2 n ppi);
                ppi :: pres, recargs
              with Term_capture ->
                raise Not_found
         )
         ([],IntSet.empty)
         ps_rev
     in
     if has_all_recursive_arguments recargs co args c1 then
       let reco0 =
         (* build cond1 and cond2 and ... x = c(cargs) *)
         let x_eq_co =
           Context.equality_term (Variable (n+1)) (VAppl(co,args,ags,oo)) c1
         in
         match (List.rev pres) with
         | [] ->
            x_eq_co
         | hd :: tl ->
            Context.and_term
              (List.fold_left
                 (fun res cond -> Context.and_term res cond c1)
                 hd
                 tl)
              x_eq_co
              c1
       in
       let reco =
         Context.prenex_term (Term.some_quantified tps reco0) c
       and pres =
         List.map
           (fun pre ->
             permute_arguments args ags pre |> Term.down_from 2 n)
           pres
       in
       assert (Context.is_well_typed reco c);
       reco, co - count_variables c1, pres
     else
       raise Not_found;
  | _ ->
     raise Not_found




let is_pair_mutually_exclusive
      (i:int)
      (j:int)
      (carr:(term*int*term list) array)
      (cls: int)
      (ft:Feature_table.t)
    : bool =
  try
    ignore(
        List.find
          (fun (t1,t2) ->
            let _,co_i,_ = carr.(i)
            and _,co_j,_ = carr.(j) in
            let ri = Feature_table.recognizer co_i ft
            and rj = Feature_table.recognizer co_j ft
            in
            (Term.equivalent t1 ri &&  Term.equivalent t2 rj)
            || (Term.equivalent t2 ri &&  Term.equivalent t1 rj)
          )
          (Class_table.recognizer_pairs cls (Feature_table.class_table ft))
      );
    true
  with Not_found ->
    false



let are_all_mutually_exclusive
      (carr:(term*int*term list) array)
      (cls: int)
      (ft:Feature_table.t)
    : bool =
  try
    Array.iteri
      (fun i (_,co_i,_) ->
        Array.iteri
          (fun j (_,co_j,_) ->
            if i < j then
              if is_pair_mutually_exclusive i j carr cls ft then
                ()
              else
                raise Reject
            else
              ()
          )
          carr
      )
      carr;
    true
  with Reject ->
    false


let check_class (cls:int) (ft:Feature_table.t): unit =
  (* Check that a class is a complete pseudoinductive class i.e. a class which
     can pattern match. The following conditions must be satisfied:

     - The class has an induction law.

     - All constructors have a complete set of projectors and at least one
       candidate for a computable recognizer expression.

     - The recognizer expressions must be mutually exclusive.
   *)
  let ct = Feature_table.class_table ft in
  if Class_table.is_pseudo_inductive cls ct
     && not (Class_table.can_match_pattern cls ct) then
    let law_idx, carr, cset = Class_table.primary_induction_law cls ct in
    if Array.for_all
         (fun (_,co,_) -> Feature_table.has_all_projectors co ft)
         carr then
      begin
        Array.iter
          (fun (reco,co,_) ->
            Feature_table.filter_recognizers reco co ft
          )
          carr;
        if are_all_mutually_exclusive carr cls ft then
          begin
            if is_tracing_ft ft then
              printf "Class %s can do pattern match\n\n"
                     (Class_table.class_name cls ct);
            Class_table.set_pattern_match cls ct
          end
      end





let put_potential_induction_law
      (idx:int) (t:term) (ps_rev: term list) (c:Context.t)
    : unit =
  (* A law of the form

         all(p,x) pp1 ==> ... ==> x in p has been

     encountered. Analyze if its a normal induction law i.e. that each
     premise in [ps_rev] is a constructor rule of the form

         all(a1,..) cond ==> ra1 in p ==> ... ==> c(a1,...) in p

     Note: - [p] is always the variable 0 and [x] the variable 1 in the
             context.
           - multiple preconditions might occur

    In case of a normal induction law store the law index and the constructors
    as an induction law within the corresponding class and mark the
    constructors.  *)
  let open Context in
  assert (is_toplevel c);
  try
    let tp = variable_type 1 c in
    let cls = Tvars.principal_class tp (tvars c) in
    match ps_rev with
    (* wellfounded induction law with induction hypothesis
           all(y) (all(x) x < y ==> x in p) ==> y in p *)
    | [QExp(tps1,fgs1,
            VAppl(imp1,
                  [|QExp(tps2,fgs2,
                         VAppl(imp2,
                               [|rel;
                                 Application(Variable 2,
                                             [|Variable 0|],_)
                               |],
                               [||],_),
                         true
                        );
                    Application(Variable 1,[|Variable 0|],_)|],
                  [||], _),
            true)
      ]  when Formals.count tps1 = 1 && Formals.count tps2 = 1
              && imp1 = 3 + Constants.implication_index
              && imp2 = 4 + Constants.implication_index ->
       let rel_idx =
         match rel with
         | VAppl(i,[|Variable 0; Variable 1|],_,_) when 4 <= i ->
            i - 4
         | _ ->
            raise Not_found
       in
       if is_tracing c then
         begin
           printf "\nwellfounded induction law\n";
           printf "  %s\n" (string_of_term t (pop c));
           printf "  with wellfounded relation\n";
           printf "    %s\n\n"
                  (Feature_table.string_of_signature rel_idx (feature_table c))
         end;
       Class_table.add_wellfounded_induction_law idx rel_idx cls (class_table c)

    (* Normal induction law *)
    | _ ->
       let lst =
         List.fold_left
           (fun lst pp ->
             let ghost_reco,co,pres = split_constructor_rule pp c in
             if List.for_all (fun (_,co0,_) -> co <> co0) lst then
               (ghost_reco,co,pres) :: lst
             else
               raise Not_found
           )
           []
           ps_rev
       in
       if is_tracing c then
         begin
           let ft = Context.feature_table c in
           assert (Context.is_global (pop c));
           printf "\nnormal induction law\n";
           printf "   %s\n" (string_of_term t (pop c));
           printf "constructors\n";
           List.iter
             (fun (reco,co,pres) ->
               let cf = Context.context_of_feature co (pop c) in
               printf "   %d %s\n" co (Feature_table.string_of_signature co ft);
               printf "     recognizer %s\n" (string_of_term reco c);
               List.iteri
                 (fun i pre ->
                   printf "     precondition %d %s\n" i (string_of_term pre cf)
                 )
                 pres
             )
             lst;
           printf "\n"
         end;
       let carr =
         Array.of_list
           (List.map
              (fun (reco,co,pres) ->
                Term.down 1 reco,  (* remove [p] *)
                co,
                pres
              )
              lst)
       in
       Class_table.add_induction_law idx carr cls (class_table c);
       Array.iter
         (fun (reco,co,pres) ->
           Feature_table.add_constructor_preconditions pres co (feature_table c)
         )
         carr;
       begin
         try
           let const_idx =
             (* find constant constructor *)
             Search.array_find_min
               (fun (reco,co,_) ->
                 match reco with
                 | QExp (_,_,_,false) ->
                    false
                 | _ ->
                    true
               )
               carr
           in
           let reco,co,pres = carr.(const_idx) in
           Feature_table.add_recognizer reco reco co (feature_table c);
         with Not_found ->
           ()
       end;
       check_class cls (feature_table c)
  with Not_found ->
    ()






let recognizer_condition_constructor
      (t:term) (c:Context.t)
    : term option * int * int =
  (* Try to match [t] with one of

         x = c(a1,...)
         cond and x = c(a1,...)

     which is the inner part of

         all(x) exp = some(a1,...) t

     in case of success return the optional condition and the
     constructor. Otherwise raise Not_found.
   *)
  let nvars = Context.count_variables c
  and ft = Context.feature_table c
  in
  assert (2 <= nvars); (* x and at least one variable in the existial
                          quantification *)
  let constructor (t:term): int * int =
    (* Find x = c(a1,...) and return the constructor and the class. *)
    match t with
    | VAppl(eq, [|Variable n;VAppl (co,args,ags,oo)|],_,_)
         when is_standard_substitution args
              && not (Feature_table.is_ghost_function (co-nvars) ft)
              && n = nvars - 1 ->
       co - nvars,
       Context.class_of_term (VAppl (co,args,ags,oo)) c
    | VAppl(eq, [|VAppl (co,args,_,_);Variable n|],ags,oo)
         when is_standard_substitution args
              && not (Feature_table.is_ghost_function (co-nvars) ft)
              && n = nvars - 1 ->
       co - nvars,
       Context.class_of_term (VAppl (co,args,ags,oo)) c
    | _ ->
       raise Not_found
  in
  try
    let co, cls = constructor t in
    None, co, cls
  with Not_found ->
       match t with
       | VAppl(andidx, [|cond;rest|], _, _)
            when andidx = Constants.and_index + nvars ->
          let co, cls = constructor rest in
          Some cond, co, cls
       | _ ->
          raise Not_found



let is_most_general (t:term) (c:Context.t): bool =
  (* Is the type of the term [t] in its most general form? *)
  let open Context in
  let tp = type_of_term t c
  and tvs = tvars c in
  let cls = Tvars.principal_class tp tvs in
  let _,ags = split_type tp in
  let ctp,ctvs = Class_table.class_type cls (class_table c) in
  let _,cags = split_type ctp in
  let nags = Array.length ags in
  assert (nags = Array.length cags);
  (*printf "is_most_general\n";
  printf "   t    %s\n" (string_of_term t c);
  printf "   cls  %s\n" (Class_table.class_name cls (class_table c));
  printf "   tvs  %s\n" (Class_table.string_of_tvs tvs (class_table c));
  printf "   tp   %s\n" (Class_table.string_of_type tp tvs (class_table c));
  printf "   ctvs %s\n" (Class_table.string_of_tvs ctvs (class_table c));
  printf "   ctp  %s\n" (Class_table.string_of_type ctp ctvs (class_table c));*)
  interval_for_all
    (fun i ->
      match ags.(i), cags.(i) with
      |  Variable j, Variable cj when j < Tvars.count_all tvs ->
          assert (cj < Tvars.count_all ctvs);
          Tvars.concept_class j tvs = Tvars.concept_class cj ctvs
      | _ ->
         false
    )
    0 nags




let put_assertion (idx:int) (t:term) (c0:Context.t): unit =
  (* Analyze the assertion [t] which has been entered at [idx]. Find out if it
     is an induction law, it defines a projector or it defines a potential
     case recognizer and store the corresponding information. *)
  assert (Context.is_global c0);
  let tps,fgs,ps_rev,t0 =
    Term.split_general_implication_chain t Constants.implication_index
  in
  let n = Formals.count tps in
  let ft = Context.feature_table c0
  and c =
    Context.push_typed0 tps fgs c0
  in
  match t0 with
  (* Induction law all(p,x) pp1 ==> pp2 ==> ... ==> x in p *)
  | Application(Variable 0, [|Variable 1|],_)
       when n = 2
            && ps_rev <> []
            && List.for_all
                 (fun pp ->
                   try (* premises must not contain the induction variable *)
                     ignore(Term.shift_from (-1) 1 0 0 pp);
                     true
                   with Term_capture ->
                        false)
                 ps_rev
                 && is_most_general (Variable 1) c
    ->
     put_potential_induction_law idx t ps_rev c

  (* Projector all(a1,..) proj_ij(ci(a1,..) = aj *)
  | VAppl(eq,[|VAppl(proj,[|VAppl(co,cargs,ags,oo)|],pags,_);Variable i|],_,_)
       when ps_rev = []
            && proj <> co
            && i < n
            && Feature_table.is_equality_index (eq - n) ft
            && not (Feature_table.is_ghost_function (proj - n) ft)
            && not (Feature_table.is_ghost_function (co - n) ft)
            && is_standard_substitution cargs
            && is_most_general (VAppl(co,cargs,ags,oo)) c ->
     if is_tracing c then
       begin
         let open Feature_table in
         printf "\nprojector found\n";
         printf "   constructor: %s\n" (string_of_signature (co-n) ft);
         printf "   projector:   %s\n" (string_of_signature (proj-n) ft);
         printf "   variable:    %d\n\n" i
       end;
     Feature_table.set_projector (proj-n) i (co-n) ft;
     check_class (Context.class_of_term (VAppl(co,cargs,ags,oo)) c) ft

  (* Case recognizer: all(x) exp = some(a1,...) cond and c(a1,...) = x *)
  | VAppl(eq,[|exp; QExp(tps2,fgs2,t02,false)|],_,_)
       when n = 1
            && ps_rev = []
            && Feature_table.is_equality_index (eq - n) ft
            && not (Feature_table.is_ghost_term exp n ft)
            && is_most_general (Variable 0) c ->
     begin
       let ghost_reco = QExp(tps2,fgs2,t02,false)
       and c2 =
         Context.push_typed0 tps2 fgs2 c in
       try
         let cond,co,cls = recognizer_condition_constructor t02 c2 in
         if is_tracing c then
           begin
             let open Context in
             let open Feature_table in
             printf "\npotential recognizer found for %s\n"
                    (string_of_signature co ft);
             printf "  recognizer     %s\n" (string_of_term exp c);
             printf "  equivalent to  %s\n\n"
                    (string_of_term
                       ghost_reco
                       c);
           end;
         Feature_table.add_recognizer exp ghost_reco co ft;
         check_class cls ft
       with Not_found ->
            ()
     end

  (* Mutual exclusiveness: all(x) a ==> b ==> false  *)
  | VAppl(i,[||],[||],_)
       when n = 1
            && List.length ps_rev = 2
            && i = n + Constants.false_index
            && is_most_general (Variable 0) c
            && not (Class_table.is_inductive
                      (Context.variable_class 0 c)
                      (Context.class_table c))
            && List.for_all
                 (fun t ->
                   try
                     ignore (Term.down 1 t);
                     false
                   with Term_capture ->
                        true
                 )
                 ps_rev
    ->
     let open Context in
     let a,b =
       match ps_rev with
       | [b;a] ->
           a,b
       | _ ->
          assert false
     and cls = class_of_type (Formals.typ 0 tps) c
     in
     if is_tracing c then
       begin
         printf "\n\nmutually exclusive recognizer candidates found\n";
         printf "   %s\n" (string_of_term a c);
         printf "   %s\n\n" (string_of_term b c);
       end;
     Class_table.add_recognizer_pair a b cls (class_table c);
     check_class cls (feature_table c)
  | _ ->
     ()
