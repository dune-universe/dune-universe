open Term
open Signature
open Container
open Support
open Printf


type match_result = (term array * term list, term list) result


module FT = Feature_table

let is_pattern (n:int) (t:term) (nb:int) (ft:FT.t): bool =
  (* Is the term [t] with [n] variables a pattern i.e. does it contain only variables
     or constructors?

     All variables below [n] must occur only once in a pattern.
   *)
  let is_constr i = (n+nb) <= i && FT.is_constructor (i-n-nb) ft
  in
  let free = Term.free_variables t n
  and bnd  = Term.bound_variables t n
  and bnd_lst = Term.used_variables t n in
  let nbnd = IntSet.cardinal bnd
  in
  IntSet.for_all is_constr free &&
  n = nbnd &&
  nbnd = List.length bnd_lst



let recognizer (t:term) (co:int) (ags:agens) (c:Context.t): term =
  (* A recognizer expression which expresses the fact that term [t] has been
     constructed by the constructor [co] using [ags] to substitute the formal
     generics of [co]. Raise [Not_found] if [co] has no recognizer.*)
  try
    let open Context in
    let nvars = count_variables c
    and tvs = tvars c
    and ft = feature_table c
    in
    assert (nvars <= co);
    let reco = Feature_table.recognizer (co-nvars) ft in
    let res =
      Feature_table.substituted reco 1 0 0 [|t|] nvars ags tvs ft
    in
    (*printf "recognizer\n";
      printf "    constr %s\n" (Feature_table.string_of_signature (co-nvars) c.ft);
      printf "    reco   %s\n" (string_of_term res c);*)
      res
  with Not_found ->
    assert false (* Illegal call *)



let project (t:term) (co:int) (n:int) (ags:agens) (c:Context.t): arguments =
  (* The term [t] projected onto the arguments of the constructor [co] with
     [n] arguments with the actual generics [ags]. *)
  try
    let open Context in
    let nvars = count_variables c
    and tvs = tvars c
    and ft = feature_table c
    in
    assert (nvars + n <= co);
    let tvs_co,sign_co = Feature_table.signature0 (co-n-nvars) ft
    and nfgs_c = Tvars.count_fgs tvs
    and projs = Feature_table.projectors (co-n-nvars) ft in
    assert (Sign.arity sign_co = n);
    assert (Tvars.count_fgs tvs_co = Array.length ags);
    let res =
    Array.map
      (fun proj ->
        let ags_new =
          let tvs_pr,sign_pr = Feature_table.signature0 proj ft in
          assert (Sign.arity sign_pr = 1);
          assert (Tvars.count_fgs tvs_pr = Tvars.count_fgs tvs_co);
          let subs =
            Type_substitution.make_equal
              (Sign.arg_type 0 sign_pr) tvs_pr
              (Sign.result sign_co) tvs_co
              (Context.class_table c)
          in
          Term.subst_array subs nfgs_c ags
        in
        VAppl(proj+nvars, [|t|], ags_new, false)
      )
      projs
    in
    printf "project %s\n" (Context.string_of_term t c);
    printf "  onto constructor %s\n"
           (Feature_table.string_of_signature (co-n-nvars) ft);
    printf "  %s\n" (Context.string_of_term_array "," res c);
    res
  with Not_found ->
    assert false (* Illegal call *)




let add_to_list_unique (t:term) (lst: term list): term list =
  try
    ignore (List.find (Term.equivalent t) lst);
    lst
  with Not_found ->
    t :: lst


let prepend_list_unique (lst1:term list) (lst:term list): term list =
  List.fold_left
    (fun lst t -> add_to_list_unique t lst)
    lst
    lst1


let unify_with_pattern
      (t:term) (n:int) (pat:term) (c:Context.t)
    : match_result option =
  (* Match the term [t] against the pattern [pat] with [n] pattern variables.

     Three cases are possible:

     a) Success: A substitution and a list of constructor preconditions is
                 returned.

     b) Reject: There are conflicting constructors. Only a list of
                preconditions is returned.

     c) Undecidable: Nothing is returned (i.e. None).

   *)
  let args = Array.make n empty_term
  and pres = ref []
  in
  let do_sub i t =
    assert (i < n);
    assert (args.(i) = empty_term); (* No duplicate pattern variables possible *)
    args.(i) <- t
  and args_complete () =
    Array.for_all (fun t -> t <> empty_term) args
  and add_pres co args ags =
    pres :=
      prepend_list_unique
        (Context.constructor_preconditions co args ags c)
        !pres
  in
  let rec pmatch (pat:term) (t:term): unit =
    match pat, t with
    | Variable i, _ ->               (* A pattern variable *)
       assert (i < n);
       do_sub i t
    | VAppl(i1,args1,ags1,_), VAppl(i2,args2,ags2,_)
         when i1 = i2 + n ->
       let n1 = Array.length args1 in
       assert (n1 = Array.length args2);
       add_pres i2 args2 ags2;
       for i = 0 to n1 - 1 do
         pmatch args1.(i) args2.(i)
       done
    | VAppl(i1,args1,ags1,_), VAppl(i2,args2,ags2,_)
         when Context.is_constructor i2 c  ->
       raise Reject
    | VAppl(i1,args1,ags1,_), VAppl(i2,args2,ags2,_)
         when Context.is_pseudo_constructor i2 c  ->
       add_pres i2 args2 ags2;
       raise Reject
    | VAppl _, _ ->
       raise Undecidable
    | _ ->
       assert false (* [pat] must consist only of variables and constructors. *)
  in
  try
    pmatch pat t;
    assert (args_complete ());
    Some ( Ok (args,!pres) )
  with
  | Undecidable ->
    None
  | Reject ->
     Some (Error !pres)

let decide_inspect
      (insp:term) (cases: (formals * term * term) array) (c:Context.t)
    : (int * arguments * term list) option =
  let ncases = Array.length cases
  in
  let rec decide_from (i:int) (pres:term list)
          : (int * arguments * term list) option =
    if i = ncases then
      None
    else
      let tps,pat,res = cases.(i) in
      let n = Formals.count tps in
      match unify_with_pattern insp n pat c with
      | None ->
         None
      | Some (Ok (args,pres1)) ->
         Some (i, args, prepend_list_unique pres1 pres)
      | Some (Error pres1) ->
         decide_from
           (i+1)
           (prepend_list_unique pres1 pres)
  in
  decide_from 0 []






let evaluated_primary_as_expression
      (e:term) (n:int) (tps:types) (pat:term) (c:Context.t): term =
  (* The as expression is [e as pat] where [e] cannot be matched neither
     partially against the pattern.

     The as expression has the evaluation

         all(pvars) e = pat

   *)
  let nms = anon_argnames n in
  let e = Term.up n e
  and c1 = Context.push_typed0 (Formals.make nms tps) Formals.empty c in
  Term.some_quantified (Formals.make nms tps) (Context.equality_term e pat c1)


let make_as_expression
      (e:term) (tps:formals0) (pat:term) (c:Context.t): term =
  (* Construct the as expression [e as pat] and eliminate all unused variables
     from (nms,tps).
   *)
  assert (Context.has_no_type_variables c);
  let (nms,tps),fgs,pat = Term.remove_unused tps 0 empty_formals pat in
  Asexp(e,tps,pat)



let evaluated_as_expression (t:term) (c:Context.t): term =
  (*
    If an as expression allows a partial match e.g.

        (a,b) as (0, successor(_))

    then the evaluation returns the splitted as expression

        a as 0  and  b as successor(_)

    instead of evaluating it directly to

        some(n) (a,b) = (0, successor(n))
   *)
  match t with
  | Asexp(e,tps,pat) ->
     let n = Array.length tps in
     let nms = anon_argnames n in
     let rec collect
               (e:term) (pat:term) (lst:(term*term) list): (term*term) list =
       match e, pat with
       | VAppl(i,args1,_,_), VAppl(j,args2,_,_) when i +  n = j ->
          let len = Array.length args1 in
          assert (len = Array.length args2);
          if len = 0 then
            (e,pat)::lst
          else
          interval_fold
            (fun lst k -> collect args1.(k) args2.(k) lst)
            lst 0 len
       | _ ->
          (e,pat)::lst
     in
     begin
       match collect e pat [] |> List.rev with
       | [] ->
          assert false (* cannot happen; at least one pair must be present *)
       | [e0,pat0] ->
          assert (Term.equivalent e e0);
          assert (Term.equivalent pat pat0);
          evaluated_primary_as_expression e n tps pat c
       | (e0,pat0) :: rest ->
          List.fold_left
            (fun res (e0,pat0) ->
              Context.and_term
                res
                (make_as_expression e0 (nms,tps) pat0 c)
                c
            )
            (make_as_expression e0 (nms,tps) pat0 c)
            rest
     end
  | _ ->
     assert false (* Not an as expression *)
