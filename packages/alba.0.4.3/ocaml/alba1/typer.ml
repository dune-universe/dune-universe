(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support
open Term
open Container
open Signature
open Printf


module TB = Term_builder

type ident =
    IDVar of int
  | IDFun of int list



type eterm0 =
    EVar of int
  | EGlAppl of (int list * eterm array * application_mode)
  | EAppl of (eterm * eterm array * application_mode)
  | ELam  of (eterm list * eterm * bool)  (* preconditions, term ,is_pred *)
  | EQExp of eterm * bool  (* is_all *)
  | EInspect of eterm * (eterm*eterm) list
  | EAs of eterm * eterm
  | EIf of eterm * eterm * eterm
  | EIndset of eterm list
  | ETyped of eterm * type_term withinfo

and eterm = {
    info: info;
    context: Context.t;
    term: eterm0
  }

type max_numbers = {
    max_locs: int;
    max_globs: int
  }


let prefix (level:int): string =
  String.make (2+2*level) ' '


let used_variables_0 (nargs:int) (et:eterm) (set:IntSet.t): IntSet.t =
  let rec used et nb set =
    let used_args args nb set =
      Array.fold_left
        (fun set et -> used et nb set)
        set
        args
    in
    let used_list args nb set =
      List.fold_left
        (fun set et -> used et nb set)
        set
        args
    in
    match et.term with
      EVar i ->
        if nb <= i && i < nargs + nb then
          IntSet.add (i-nb) set
        else
          set
    | EGlAppl (_,args,_) ->
        used_args args nb set
    | EAppl (f,args,_) ->
        let set = used f nb set in
        used_args args nb set
    | ELam (pres,et0,is_pred) ->
        let nb = nb + Context.count_last_variables et0.context in
        let set = used_list pres nb set in
        used et0 nb set
    | EQExp (et0,_) ->
        let nb = nb + Context.count_last_variables et0.context in
        used et0 nb set
    | ETyped (e,_) ->
        used e nb set
    | EInspect (insp,cases) ->
        let set = used insp nb set in
        List.fold_left
          (fun set (pat,res) ->
            assert (pat.context == res.context);
            let nb = nb + Context.count_last_variables pat.context in
            used pat nb (used res nb set)
          )
          set
          cases
    | EAs (insp,pat) ->
        let set = used insp nb set in
        let nb = nb + Context.count_last_variables pat.context in
        used pat nb set
    | EIf (c,e1,e2) ->
        let set = used c nb set in
        let set = used e1 nb set in
        used e2 nb set
    | EIndset rules ->
        assert (rules <> []);
        let c0 = (List.hd rules).context in
        List.fold_left
          (fun set rule ->
            assert (rule.context == c0);
            used rule nb set)
          set
          rules
  in
  used et 0 set


let used_variables (nargs:int) (et:eterm): IntSet.t =
  used_variables_0 nargs et IntSet.empty


let max_globs_of_features (flst:int list) (c:Context.t): int =
  List.fold_left
    (fun nglobs fidx ->
      let tvs,_ = Context.feature_signature fidx c in
      max nglobs (Tvars.count_fgs tvs)
    )
    0
    flst




let find_identifier (info:info) (id:int) (c:Context.t): ident =
  try
    let i = Context.variable_index id c in
    IDVar i
  with Not_found ->
    let flst = Context.find_features (FNname id) c in
    if flst = [] then
      error_info info ("Unknown \"" ^ ST.string id ^ "\"");
    IDFun flst


let find_features (info:info) (fn:feature_name) (c:Context.t): int list =
  let flst = Context.find_features fn c
  in
  if flst = [] then
    error_info
      info
      ("Unknown function \"" ^ feature_name_to_string fn ^ "\"");
  flst


let is_constant (nme:int) (c:Context.t): bool =
  let nvars = Context.count_variables c
  and ft = Context.feature_table c in
  try
    let lst   = Context.find_identifier nme 0 c in
    let lst =
      List.filter
        (fun (idx,_,_) ->
          nvars <= idx
          && Feature_table.arity (idx-nvars) ft = 0
        )
        lst in
    lst <> []
  with Not_found ->
    false

let case_variables
    (e:expression) (dups:bool) (c:Context.t)
    : expression * int array =
  let rec vars (e:expression) (nanon:int) (lst:int list)
      : expression * int * int list =
    let fvars f nanon lst =
      match f.v with
        Identifier nme -> f,nanon,lst
      | _ -> vars f nanon lst
    in
    match e.v with
      Expnumber _ | Exptrue | Expfalse | Expop _ ->
        e, nanon, lst
    | Identifier nme | Typedexp({v=Identifier nme;i=_},_)->
        let lst =
          if is_constant nme c then
            lst
          else if dups && List.mem nme lst then
            lst
          else
            nme :: lst in
        e,nanon,lst
    | Expanon ->
        let nme = ST.symbol ("$" ^ (string_of_int nanon)) in
        withinfo e.i (Identifier nme),
        1+nanon,
        nme :: lst
    | Typedexp ({v=Expanon;i=info},tp) ->
        let nme = ST.symbol ("$" ^ (string_of_int nanon)) in
        withinfo e.i (Typedexp(withinfo info (Identifier nme),tp)),
        1+nanon, nme :: lst
    | Funapp(f,args,am) ->
        let f,nanon,lst = fvars f nanon lst in
        let arglst,nanon,lst =
          List.fold_left
            (fun (arglst,nanon,lst) e ->
              let e,nanon,lst = vars e nanon lst in
              e::arglst, nanon, lst)
            ([],nanon,lst)
            args in
        withinfo e.i (Funapp(f, List.rev arglst, am)),
        nanon, lst
    | Tupleexp (e1,e2) ->
        let e1,nanon,lst = vars e1 nanon lst in
        let e2,nanon,lst = vars e2 nanon lst in
        withinfo e.i (Tupleexp (e1,e2)),
        nanon, lst
    | Expparen e ->
        vars e nanon lst
    | _ ->
        printf "case_variables %s\n" (string_of_expression e);
        raise Not_found
  in
  try
    let e, nanon, lst = vars e 0 [] in
    if (not dups) && Mylist.has_duplicates lst then
      error_info e.i ("Duplicate variable in pattern \"" ^
                       (string_of_expression e) ^ "\"");
    let nms = Array.of_list (List.rev lst) in
    e, nms
  with Not_found ->
    error_info e.i ("Cannot extract variables from pattern \"" ^
                     (string_of_expression e) ^ "\"")


let first_pass_0
    (e:expression)
    (mn:max_numbers)
    (c:Context.t)
    : max_numbers * eterm
    =
  let rec first
      (e:expression)
      (mn:max_numbers)
      (c:Context.t)
      : max_numbers * eterm
      =
    match e.v with
      Identifier id ->
        identifier e.i id mn c
    | Expanon ->
        not_yet_implemented e.i ("Expanon Typing of "^ (string_of_expression e))
    | Expnumber num ->
        global_application_fn e.i (FNnumber num) [] AMmath mn c
    | ExpResult ->
        begin
          try
            let i = Context.variable_index (ST.symbol "Result") c in
            mn, {info = e.i; context = c; term = EVar i}
          with Not_found ->
            error_info e.i "There is no variable \"Result\" in this context";
        end
    | Exptrue ->
        global_application_fn e.i FNtrue [] AMmath mn c
    | Expfalse ->
        global_application_fn e.i FNfalse [] AMmath mn c
    | Expparen e0 ->
        first e0 mn c
    | Exparrow (entlst,e0) ->
        lambda e.i entlst false None [] e0 mn c
    | Expagent (entlst,rt,pres,exp) ->
        lambda e.i entlst false rt pres exp mn c
    | Expop op ->
        global_application_fn e.i (FNoperator op) [] AMmath mn c
    | Funapp (f,args,am) ->
        application e.i f args am mn c
    | Expset e ->
        assert false (* Really needed ? *)
    | Exppred (entlst,e0) ->
        lambda e.i entlst true None [] e0 mn c
    | Expindset (entlst,rules) ->
        let c_new = Context.push entlst None false false false c in
        let nargs = Context.count_last_arguments c_new
        and nlocs = Context.count_local_type_variables c_new in
        if nargs <> 1 then
          not_yet_implemented entlst.i "Multiple inductive sets";
        assert (nlocs <= 1);
        let mn =
          {max_globs = mn.max_globs + nlocs;
           max_locs  = max mn.max_locs (Context.count_type_variables c_new)}
        in
        let mn, rules =
          List.fold_left
            (fun (mn,rules) e ->
              let mn,et = first e mn c_new in
              mn, et :: rules
            )
            (mn,[])
            rules
        in
        mn,
        {info = e.i;
         context = c;
         term = EIndset (List.rev rules)}
    | Tupleexp (e1,e2) ->
        global_application_fn e.i (FNname ST.tuple) [e1;e2] AMmath mn c
    | Typedexp (e0,tp_) ->
        let tp = Context.get_type tp_ c in
        let mn,et = first e0 mn c in
        mn,
        {info = e.i;
         context = c;
         term = ETyped (et, withinfo tp_.i tp)}
    | Expcolon (e1,e2) ->
       not_yet_implemented e.i "Colon operator" (* Really needed ? *)
    | Expif (cond,e1,e2) ->
        let mn, cond = first cond mn c in
        let mn, e1   = first e1 mn c in
        let mn, e2   = first e2 mn c in
        mn,
        {info = e.i;
         context = c;
         term = EIf (cond,e1,e2)}
    | Expas (e0,pat) ->
        let mn,einsp = first e0 mn c in
        let pat,names = case_variables pat false c in
        let c_new = Context.push_untyped names c in
        let mn =
          {mn with
           max_locs = max mn.max_locs (Context.count_type_variables c_new)}
        in
        let mn, epat = first pat mn c_new in
        mn,
        {info = e.i;
         context = c;
         term = EAs (einsp,epat)}
    | Expinspect (insp, cases) ->
        inspect e.i insp cases mn c
    | Expquantified (q,entlst,exp) ->
        quantified e.i q entlst exp mn c

  and application info f args am mn c =
    match f.v with
      Identifier id ->
        begin
          match find_identifier f.i id c with
            IDVar i ->
              let fterm = {info = f.i; context = c; term = EVar i} in
              term_application info fterm args am mn c
          | IDFun flst ->
              global_application_flst info flst args am mn c
        end
    | Expop op ->
        global_application_fn info (FNoperator op) args am mn c
    | _ ->
        let mn,f_et = first f mn c in
        term_application info f_et args am mn c

  and term_application
      (info:info)
      (f:eterm)
      (args:expression list)
      (am:application_mode)
      (mn:max_numbers)
      (c:Context.t)
      : max_numbers * eterm
      =
    let mn,args = arguments info args mn c in
    {mn with max_globs = 2 + List.length args + mn.max_globs},
    {info = info;
     context = c;
     term = EAppl (f,Array.of_list args,am)}

  and global_application_fn
      (info:info)
      (fn:feature_name)
      (args:expression list)
      (am:application_mode)
      (mn:max_numbers)
      (c:Context.t)
      : max_numbers * eterm
      =
    let flst = find_features info fn c in
    global_application_flst info flst args am mn c

  and global_application_flst info flst args am mn c =
    let nglobs_f = max_globs_of_features flst c in
    let mn = {mn with max_globs = nglobs_f + mn.max_globs} in
    let mn,args = arguments info args mn c in
    mn,
    {info = info;
     context = c;
     term = EGlAppl(flst, Array.of_list args, am)}

  and arguments
      (info:info)
      (args:expression list)
      (mn:max_numbers)
      (c:Context.t)
      : max_numbers * eterm list
      =
    let mn,lst =
      List.fold_left
        (fun (mn,lst) arg ->
          let mn,et =
            first arg mn c
          in
          mn, et::lst
        )
        (mn,[])
        args
    in
    mn, List.rev lst

  and identifier
      (info:info)
      (id:int)
      (mn:max_numbers)
      (c:Context.t)
      : max_numbers * eterm
      =
    match find_identifier info id c with
      IDVar i ->
        mn, {info = info; context = c; term = EVar i}
    | IDFun flst ->
        global_application_flst info flst [] AMmath mn c

  and lambda
      (info:info)
      (entlst:entities list withinfo)
      (is_pred:bool)
      (rt: return_type)
      (pres: compound)
      (e:expression)
      (mn: max_numbers)
      (c:Context.t)
      : max_numbers * eterm
      =
    let c_new = Context.push entlst rt is_pred (not is_pred) false c in
    if Context.count_last_formal_generics c_new > 0 then
      error_info
        entlst.i
        "Lambda expression must not introduce new formal generic";
    let mn =
      {max_locs  = max mn.max_locs (Context.count_type_variables c_new);
       max_globs = mn.max_globs + Context.count_local_type_variables c_new
     }
    in
    let pres_rev,mn =
      List.fold_left
        (fun (pres_rev,mn) e ->
          let mn,et = first e mn c_new in
          et :: pres_rev, mn
        )
        ([],mn)
        pres
    in
    let mn, et0 = first e mn c_new in
    mn,
    {info = info;
     context = c;
     term = ELam (List.rev pres_rev, et0, is_pred)}

  and quantified
      (info:info)
      (q:quantifier)
      (entlst:entities list withinfo)
      (exp:expression)
      (mn: max_numbers)
      (c:Context.t)
      : max_numbers * eterm
      =
    let c_new = Context.push entlst None false false false c in
    if Context.count_last_formal_generics c_new > 0 then
      error_info
        entlst.i
        "Quantified expression must not introduce new formal generic";
    let nargs = Context.count_last_variables c_new in
    let mn =
      {mn with
       max_locs = max mn.max_locs (Context.count_type_variables c_new)}
    in
    let mn, et0 = first exp mn c_new
    and is_all =
      begin
        match q with
          Existential -> false
        | Universal -> true
      end
    in
    let used = used_variables nargs et0 in
    if IntSet.cardinal used <> nargs then begin
      let names = Context.local_argnames c_new
      and unused =
        List.rev
          (interval_fold
             (fun unused i ->
               if not (IntSet.mem i used) then
                 i :: unused
               else
                 unused
             ) [] 0 nargs)
      in
      error_info
        info
        ("Unused variables\nThe quantified expression\n\n\t" ^
         string_of_expression e ^
         "\n\nhas the following unused " ^
         (if List.length unused = 1 then "variable" else "variables") ^
         "\n\n\t" ^
         String.concat
           ", "
           (List.map (fun i -> ST.string names.(i)) unused)
         ^
             "\n")
    end;
    mn,
    {info = info;
     context = c;
     term = EQExp (et0,is_all)}

  and inspect
      (info:info)
      (insp:expression)
      (cases: (expression*expression) list)
      (mn: max_numbers)
      (c:Context.t)
      : max_numbers * eterm
      =
    let ninspected = expression_list_length insp in
    let mn = {mn with max_globs = mn.max_globs + 1} in
    let mn,einsp = first insp mn c in
    let mn,ecases =
      List.fold_left
        (fun (mn,ecases) (pat,res) ->
          if ninspected > expression_list_length pat then
            error_info
              info
              ("Illegal pattern\n" ^
               "The pattern\n\n\t" ^
               (string_of_expression pat) ^
               "\n\ndoes not have " ^
               (string_of_int ninspected) ^
               " subpattern and therefore cannot be matched against the\n" ^
               "inspected expression\n\n\t" ^
               (string_of_expression insp) ^
               "\n");
          let pat,names = case_variables pat false c in
          let c_new = Context.push_untyped names c in
          let mn =
            {mn with
             max_locs = max mn.max_locs (Context.count_type_variables c_new)}
          in
          let mn, epat = first pat mn c_new in
          let mn, eres = first res mn c_new in
          mn, (epat,eres) :: ecases
        )
        (mn,[])
        cases
    in
    let ecases = List.rev ecases in
    mn,
    {info = info;
     context = c;
     term = EInspect (einsp,ecases)}
  in
  first e mn c


let first_pass
    (e:expression)
    (c:Context.t)
    : max_numbers * eterm
    =
  first_pass_0
    e
    {max_locs  = Context.count_type_variables c;
     max_globs = 0}
    c


let first_pass_list
      (info:info)
      (check_used: bool)
      (lst: (expression * type_term option) list)
      (c:Context.t)
    : max_numbers * (eterm*type_term option*expression) list =
  let nargs = Context.count_last_variables c in
  let first_pass_list0 mn used lst =
    let mn,used,lst =
      List.fold_left
        (fun (mn,used,lst) (e,tp) ->
          let mn,et = first_pass_0 e mn c in
          let used = used_variables_0 nargs et used in
          mn, used, (et,tp,e) :: lst)
        (mn,used,[])
        lst
    in
    mn, used, List.rev lst
  in
  let mn = {max_locs  = Context.count_type_variables c;
            max_globs = 0} in
  let mn,used,lst = first_pass_list0 mn IntSet.empty lst in
  if check_used && IntSet.cardinal used < nargs then
    begin
      let unused =
        List.rev
          (interval_fold
             (fun unused i ->
               if IntSet.mem i used then
                 unused
               else
                 ST.string (Context.variable_name i c) :: unused
             )
             [] 0 nargs)
      in
      assert (unused <> []);
      let vars =
        if List.length unused = 1 then "variable is" else "variables are" in
      error_info
        info
        ("The following " ^ vars ^ " not used: \""
         ^ String.concat "," unused ^ "\". Neither in assumptions nor in goals")
    end
  else
    mn, lst



let replicate_tbs (tbs:TB.t list) (flst:int list): (int * TB.t list) list =
  (* Provide all global functions in the list [lst] with an own copy of the
     tbs.
   *)
  let copies tbs = List.rev_map TB.copy tbs
  in
  match flst with
    [] ->
      assert false (* cannot happen; at least one global function must be present. *)
  | fidx :: rest ->
      (fidx,tbs) :: (List.map (fun fidx -> fidx, copies tbs) rest)


let trace_head_terms (level:int) (c:Context.t) (tbs:TB.t list): unit =
  let len = List.length tbs in
  List.iteri
    (fun i tb ->
      let istr = if len = 1 then "" else (string_of_int i) ^ ": " in
      printf "%s%s%s\n"
        (prefix level)
        istr
        (TB.string_of_complete_head_term tb)
    )
    tbs


let string_of_required_types (tbs:TB.t list): string =
  (if List.length tbs = 1 then
    "the required type"
  else
    "any of the required types") ^
  "\n\n\t" ^
  String.concat
    "\n\t"
    (List.map TB.string_of_required_type tbs) ^
  "\n"


let string_of_variable_types (i:int) (tbs:TB.t list): string =
  (if List.length tbs = 1 then
    "type"
  else
    "types") ^
  "\n\n\t" ^
  String.concat
    "\n\t"
    (List.map (fun tb -> TB.string_of_variable_type i tb) tbs) ^
  "\n"



let filter_global_functions
    (info:info)
    (flst:int list)            (* The possible global functions *)
    (nargs:int)                (* The actual number of arguments *)
    (tbs:TB.t list)
    : TB.t list
    =
  (* Analyze the possible global functions [flst] unify the result type with the
     required type and return each global function with a proper type context.

     Report an error if no function can be unified with any of the required
     return types.
   *)
  let flst1 = replicate_tbs tbs flst in
  let tbs1 =
    List.fold_left
      (fun tbs1 (fidx,tbs) ->
        List.fold_left
          (fun tbs1 tb ->
            try
              TB.start_global_application fidx nargs tb;
              tb :: tbs1
            with Reject ->
              tbs1
          )
          tbs1
          tbs
      )
      []
      flst1
  in
  if tbs1 = [] then
    error_info
      info
      (let one = List.length flst = 1 in
      "Type mismatch\n" ^
       (if one then
         "The function\n\n\t"
       else "None of the functions\n\n\t") ^
      (let c = TB.context (List.hd tbs) in
       String.concat
         "\n\n\t"
         (List.map (fun fidx -> Context.string_of_feature_signature fidx c) flst)
      ) ^ "\n\n" ^
      (if one then "cannot " else "can ") ^
      "accept " ^ (string_of_int nargs) ^
      " argument(s) and return an object matching\n" ^
      string_of_required_types tbs);
  tbs1


let variable_type_mismatch
    (info:info) (i:int) (c:Context.t) (tbs:TB.t list)
    : unit
    =
  error_info
    info
    ("Type mismatch\n" ^
       "The variable \"" ^ ST.string (Context.variable_name i c) ^ "\" has "
       ^ string_of_variable_types i tbs
       ^ "\n\nwhich does not match "
       ^ string_of_required_types tbs)




let iterate_tbs (f: TB.t->unit) (tbs:TB.t list): TB.t list =
  List.fold_left
    (fun tbs tb ->
      try
        f tb;
        tb :: tbs
      with Reject ->
        tbs
    )
    []
    tbs



let illegal_pattern (info:info): 'a =
  error_info
    info
    ("Illegal pattern\n"
     ^ "A pattern must consist only of variables and constructors")

let analyze_eterm
    (et_outer:eterm)
    (tbs:TB.t list)
    (level:int)
    (trace:bool)
    : TB.t list
    =
  let rec analyze
      (et:eterm)
      (tbs:TB.t list)
      (level:int)
      : TB.t list
      =
    assert (tbs <> []);
    match et.term with
      EVar i ->
        variable et i tbs level
    | EGlAppl (flst,args,am) ->
        global_application et flst args am tbs level
    | EAppl (f,args,am) ->
        term_application et f args am tbs level
    | ELam (pres,et0,is_pred) ->
        let pred_fun is_pred =
          if is_pred then "predicate" else "function"
        in
        if trace then
          printf "%s%s expression\n"
            (prefix level)
            (pred_fun is_pred);
        let tbs1 = iterate_tbs (TB.start_lambda et0.context is_pred) tbs
        in
        if tbs1 = [] then
          error_info
            et.info
            ("Type mismatch\n" ^
             "The " ^ (pred_fun is_pred) ^ " expression does match " ^
             string_of_required_types tbs);
        if trace then
          printf "%sinner term\n" (prefix level);
        let tbs2 = analyze et0 tbs1 (level+1)
        in
        let npres = List.length pres in
        if trace && npres > 0 then
          printf "%spreconditions\n" (prefix level);
        let tbs3 =
          List.fold_left
            (fun tbs et ->
              List.iter TB.expect_boolean tbs;
              analyze et tbs (level+1))
            tbs2
            pres
        in
        List.iter
          (TB.complete_lambda is_pred (List.length pres))
          tbs3;
        if trace then
          trace_head_terms level et.context tbs3;
        tbs3
    | EQExp (et0,is_all) ->
        if trace then
          printf "%s%s quantification\n"
            (prefix level)
            (if is_all then "universal" else "existential");
        let tbs1 = iterate_tbs (TB.start_quantified et0.context) tbs in
        if tbs1 = [] then
          error_info
            et.info
            ("Type mismatch\n" ^
             "A quantified expression has type\n\n\tBOOLEAN\n\n" ^
             "which does not match " ^
             string_of_required_types tbs
            );
        let tbs2 = analyze et0 tbs1 (level+1) in
        let tbs3 = iterate_tbs (TB.complete_quantified is_all) tbs2 in
        if tbs3 = [] then
          error_info
            et.info
            "Some of the variable types could not be inferred completely";
        if trace then
          trace_head_terms level et.context tbs3;
        tbs3
    | ETyped (e, tp) ->
        if trace then
          printf "%sexpect type %s\n"
            (prefix level)
            (Context.string_of_type tp.v e.context);
        let tbs1 = iterate_tbs (TB.expect_type tp.v) tbs in
        if tbs1 = [] then
          error_info
            tp.i
            ("Type mismatch\n" ^
             "The type \n\n\t" ^ (Context.string_of_type tp.v et.context) ^
             "\n\ndoes not match " ^
             string_of_required_types tbs);
        analyze e tbs1 level
    | EIf (cond,e1,e2) ->
        if trace then begin
          printf "%sif\n" (prefix level);
          printf "%sthen expression\n" (prefix level)
        end;
        let tbs1 = analyze e1 tbs (level+1)
        in
        if trace then
          printf "%selse expression\n" (prefix level);
        List.iter TB.expect_else_expression tbs1;
        let tbs2 = analyze e2 tbs1 (level+1)
        in
        if trace then
          printf "%scondition\n" (prefix level);
        List.iter TB.expect_boolean tbs2;
        let tbs3 = analyze cond tbs2 (level+1)
        in
        List.iter TB.complete_if_expression tbs3;
        if trace then
          trace_head_terms level et.context tbs3;
        tbs3
    | EAs (insp,pat) ->
        if trace then
          printf "%sas expression\n%sinspected\n" (prefix level) (prefix level);
        let tbs1 = iterate_tbs TB.start_as_expression tbs in
        let tbs2 = analyze insp tbs1 (level+1) in
        if trace then
          printf "%spattern\n" (prefix level);
        List.iter (fun tb -> TB.expect_as_pattern pat.context tb) tbs2;
        let tbs3 = analyze pat tbs2 (level+1) in
        let tbs4 = iterate_tbs TB.complete_as_expression tbs3 in
        if tbs4 = [] then
          illegal_pattern pat.info;
        if trace then
          trace_head_terms level et.context tbs4;
        tbs4

    | EInspect (insp,cases) ->
        if trace then
          printf "%sinspect\n" (prefix level);
        List.iter TB.start_inspect tbs;
        let tbs1 = analyze insp tbs (level+1) in
        List.iter TB.start_cases tbs1;
        let tbs2 =
          List.fold_left
            (fun tbs (pat,res) ->
              assert (pat.context == res.context);
              if trace then begin
                printf "%scase\n" (prefix level);
                printf "%spattern\n" (prefix (level+1))
              end;
              List.iter (fun tb -> TB.start_case pat.context tb) tbs;
              let tbs1 = analyze pat tbs  (level+2) in
              if trace then
                printf "%sresult\n" (prefix (level+1));
              List.iter TB.expect_case_result tbs1;
              let tbs2 = analyze res tbs1 (level+2) in
              let tbs3 = iterate_tbs TB.complete_case tbs2 in
              if tbs3 = [] then
                illegal_pattern pat.info;
              tbs3
            )
            tbs1
            cases
        in
        let ncases = List.length cases in
        List.iter (fun tb -> TB.complete_inspect ncases tb) tbs2;
        if trace then
          trace_head_terms level et.context tbs2;
        tbs2
    | EIndset rules ->
        if trace then
          printf "%sinductive set\n" (prefix level);
        assert (rules <> []);
        let c0 = (List.hd rules).context in
        let tbs1 = iterate_tbs (TB.start_inductive_set c0) tbs in
        let tbs2,nrules =
          List.fold_left
            (fun (tbs,i) rule ->
              if trace then
                printf "%srule %d\n" (prefix level) i;
              List.iter TB.expect_boolean tbs;
              let tbs = analyze rule tbs (level+1) in
              tbs, i+1
            )
            (tbs1,0)
            rules
        in
        List.iter (fun tb -> TB.complete_inductive_set nrules tb) tbs2;
        if trace then
          trace_head_terms level et.context tbs2;
        tbs2

  and variable et i tbs level =
    let tbs1 = iterate_tbs (TB.add_variable i) tbs in
    if tbs1 = [] then
      variable_type_mismatch et.info i et.context tbs;
    if trace then
      trace_head_terms level et.context tbs1;
    tbs1

  and term_application et f args am tbs level =
    if trace then
      printf "%sfunction term\n" (prefix level);
    let nargs = Array.length args in
    let tbs_pred = List.filter TB.required_can_be_boolean tbs in
    let tbs_pred = List.rev_map TB.copy tbs_pred in
    List.iter (fun tb -> TB.start_function_application  nargs tb) tbs;
    List.iter (fun tb -> TB.start_predicate_application nargs tb) tbs_pred;
    let tbs = tbs @ tbs_pred in
    let tbs1 = analyze f tbs (level+1) in
    let tbs2 = arguments args tbs1 level in
    List.iter (fun tb -> TB.complete_application am tb) tbs2;
    if trace then
      trace_head_terms level et.context tbs2;
    tbs2

  and global_application et flst args am tbs level =
    if trace then begin
      let len = List.length flst
      in
      List.iteri
        (fun i fidx ->
          let istr = if len = 1 then "" else (string_of_int i) ^ ": " in
          printf "%s%s%s\n"
            (prefix level)
            istr
            (Context.string_of_feature_signature fidx et.context)
        )
        flst
    end;
    let nargs = Array.length args in
    let tbs1 =
      filter_global_functions et.info flst nargs tbs
    in
    let tbs2 = arguments args tbs1 (level+1)
    in
    List.iter (fun tb -> TB.complete_application am tb) tbs2;
    if trace then
      trace_head_terms level et.context tbs2;
    tbs2

  and arguments args tbs level =
    let nargs = Array.length args in
    interval_fold
      (fun tbs i ->
        if trace then
          printf "%sargument %d\n" (prefix level) i;
        List.iter (fun tb -> TB.expect_argument i tb) tbs;
        analyze args.(i) tbs (level+1)
      )
      tbs 0 nargs
  in

  analyze et_outer tbs level




let get_difference (t1:term) (t2:term) (c:Context.t): string * string =
  let lam,_,arr1,arr2 =
    try
      Term_algo.compare t1 t2 (fun t1 t2 -> ())
    with Not_found ->
      printf "no differences found\n";
      printf "  t1 %s \n" (Context.string_long_of_term t1 c);
      printf "  t2 %s \n" (Context.string_long_of_term t2 c);
      printf "  t1 %s\n" (Term.to_string t1);
      printf "  t2 %s\n" (Term.to_string t2);
      assert false (* There must be differences *)
  in
  assert (Array.length arr1 > 0);
  assert (Array.length arr1 = Array.length arr2);
  match arr1.(0), arr2.(0) with
  | VAppl(i1,_,_,_), VAppl(i2,_,_,_) ->
     let nvars = Context.count_variables c
     and ft = Context.feature_table c in
     let str1 = Feature_table.string_of_signature (i1-nvars) ft
     and str2 = Feature_table.string_of_signature (i2-nvars) ft in
     str1, str2
  | Lam _ , Lam _ | QExp _, QExp _ ->
     Context.string_long_of_term arr1.(0) c,
     Context.string_long_of_term arr2.(0) c
  | _ ->
     assert false (* Cannot happen, there must be differences *)




let undefined_untyped (inf:info) (tb:TB.t) (c:Context.t): unit =
  let undefs = TB.undefined_untyped tb in
  match undefs with
  | [] ->
     error_info
       inf
       (" The types of some constants "
        ^ "or of some variables of inner function or predicate expressions "
        ^ "cannot be determined completely. "
        ^ "Consider adding type annotations.")
  | _ ->
     let open Context in
     let nms = List.map (fun i -> ST.string (variable_name i c)) undefs in
     let vars = if List.length nms = 1 then "variable" else "variables" in
     let str =
       "The type of the " ^ vars ^ "\n\n   "
       ^ String.concat ", " nms
       ^ "\n\ncannot be inferred completely"
     in
     error_info inf str


let different_untyped
      (e:expression)
      (tpsub1:type_term array) (tpsub2: type_term array)
      (c:Context.t)
    : unit =
  let open Context in
  let n = count_last_variables c in
  let len = Array.length tpsub1 in
  assert (len = Array.length tpsub2);
  let type_pair i =
    let tp  = variable_type i c in
    Term.subst tp len tpsub1,
    Term.subst tp len tpsub2
  in
  let i =
    interval_find
      (fun i ->
        let tp1,tp2 = type_pair i in
        not (Term.equivalent tp1 tp2)
      )
      0 n
  in
  let tp1,tp2 = type_pair i in
  let str =
    "The inferred type for the variable \""
    ^ (ST.string (variable_name i c))
    ^ "\" is ambiguous\n\nPossible types:\n"
    ^ "\n    " ^ string_of_type tp1 c
    ^ "\n    " ^ string_of_type tp2 c
  in
  error_info e.i str





let make_tb (mn:max_numbers) (c:Context.t): TB.t =
  TB.make None mn.max_locs mn.max_globs c


let extract_unique
      (info:info) (tbs:TB.t list) (c:Context.t)
    : formals0 * formals0 * bool * info_term list =
  match tbs with
  |  [] ->
      assert false (* Cannot happen; at least one term is returned, otherwise an
                      error would have been reported *)
  | [tb] ->
     if TB.has_undefined_globals tb then
       undefined_untyped info tb c;
     TB.terms_with_context tb
  | tb1 :: tb2 :: _ ->
     try
       printf "check predicate function ambiguity\n";
       let i = Term_builder.function_predicate_variable tb1 tb2 in
       error_info
         info
         ("It cannot be determined if the type of the variable \""
          ^ ST.string (Context.variable_name i c)
          ^ "\" is a PREDICATE or a FUNCTION")
     with Not_found ->
       try
         printf "check different subterms\n";
         let info,str1,str2 = Term_builder.different_subterms tb1 tb2 in
         error_info
           info
           ("The expression is ambiguous\n\n\t"
            ^ str1 ^ "\n\t" ^ str2)
       with Not_found ->
         error_info
           info
           ("Some inner context has a variable whose type is either a "
            ^ "FUNCTION or a PREDICATE; but it cannot be decided")


let check_one_pattern (info:info) (pat:term) (c:Context.t): unit =
  let nvars = Context.count_variables c in
  let rec check pat =
    match pat with
    | Variable i when i < nvars ->
       assert (i < Context.count_last_variables c); (* The typer only extracts
                                                       new variables *)
       ()
    | Variable i ->
       assert false (* Global constants cannot be variables *)
    | VAppl (i,args,ags,oo) ->
       if not (Feature_table.is_constructor (i-nvars) (Context.feature_table c)) then
         begin
           let open Format in
           eprintf "@[<v>%s Pattern error@," (info_string info);
           eprintf "%s@,@,    %s@,@,%s@,@,    %s@,@,%s@,@]@."
                  "The pattern"
                  (Context.string_of_term pat c)
                  "contains a call to"
                  (Feature_table.string_of_signature
                     (i-nvars) (Context.feature_table c))
                  "which is not a valid constructor";
           exit 1
         end;
       Array.iter check args
    | _ ->
       assert false (* Other subexpressions cannot occur in a pattern *)
  in
  if not (Context.is_interface_use c) then
    check pat



let check_pattern_match
      (tps:formals0)
      (fgs:formals0)
      (rvar:bool)
      (lst: info_term list)
      (c:Context.t)
    : unit =
  let rec check (info:info) (t:term) (c:Context.t): unit =
    let check_args args c =
      Array.iter (fun t -> check info t c) args
    and check_lst lst c =
      List.iter (fun t -> check info t c) lst
    and check0 t = check info t c
    in
    match t with
    | Variable i when i < Context.count_variables c ->
       ()
    | Variable i ->
       assert false (* Global constants cannot be variables *)
    | VAppl(i,args,ags,oo) ->
       check_args args c
    | Application (f,args,_) ->
       check info f c;
       check_args args c
    | Lam (tps,fgs,ps,t0,rt) ->
       let c1 = Context.push_typed0 tps fgs c in
       check_lst ps c1;
       check info t0 c1
    | QExp (tps,fgs,t0,is_all) ->
       check info t0 (Context.push_typed0 tps fgs c)
    | Ifexp (cond,a,b) ->
       check0 cond;
       check0 a;
       check0 b
    | Asexp(insp,tps,pat) ->
       check0 insp;
       let n = Array.length tps in
       let c1 =
         Context.push_typed0 (Formals.make (anon_argnames n) tps) Formals.empty c
       in
       check_one_pattern info pat c1
    | Inspect(insp,cases) ->
       check0 insp;
       Array.iter
         (fun (fs,pat,res) ->
           let c1 = Context.push_typed0 fs Formals.empty c in
           check_one_pattern info pat c1;
           check info res c1
         )
         cases
    | Indset (nme,tp,rules) ->
       let c1 = Context.push_typed0 (Formals.make [|nme|] [|tp|]) Formals.empty c in
       check_args rules c1
  in
  let c0 = Context.previous c in
  let c1 =
    Context.push_typed (Formals.from_pair tps) (Formals.from_pair fgs) rvar c0
  in
  List.iter (fun t -> check t.i t.v c1) lst


let analyze_term_list
      (info:info)
      (check_used:bool)
      (lst: (expression * type_term option) list)
      (c: Context.t)
    : formals0 * formals0 * bool * info_term list =
  assert (not (Context.is_global c));
  let trace = not (Context.is_interface_use c) && Context.verbosity c >= 5 in
  let analyze_list
        (elst: (eterm*type_term option*expression) list)
        (tbs:TB.t list)
      : TB.t list =
    List.fold_left
      (fun tbs (et,tp,e) ->
        if trace then
          Format.printf
            "@[<v>@,@[<v 4>%s@,@,%s@,@]@]@."
            "typer analyze term"
            (string_of_expression e);
        List.iter (TB.set_required_type tp) tbs;
        let tbs = analyze_eterm et tbs 0 trace in
        List.iter (TB.push_term et.info) tbs;
        tbs
      )
      tbs
      elst
  in
  let mn, lst_et =
    first_pass_list info check_used lst c in
  let tbs = analyze_list lst_et [make_tb mn c] in
  if trace then
    Format.printf "@[<v>%s@,@,@]@." "typer end analysis";
  let tps,fgs,rvar,lst = extract_unique info tbs c in
  check_pattern_match tps fgs rvar lst c;
  tps,fgs,rvar,lst


let structured_assertion
      (entlst: entities list withinfo)
      (rlst: compound)
      (elst: compound)
      (c: Context.t)
    : formals0 * formals0 * info_term list * info_term list =
  assert (Context.count_type_variables c = 0);
  let trace = not (Context.is_interface_use c) && Context.verbosity c >= 5 in
  if trace then
    begin
      let open Format in
      printf "@[<v>@,@[<v 2>%s@,@,@[<v 2>all(%s)@,@[<v 2>require"
              "typer analyze structured assertion"
              (string_of_formals entlst.v);
      List.iter (fun e -> printf "@,%s" (string_of_expression e)) rlst;
      printf "@]@,@[<v 2>ensure";
      List.iter (fun e -> printf "@,%s" (string_of_expression e)) elst;
      printf "@]@,end@]@]@,@,@]"
    end;
  let c1 = Context.push entlst None false false false c in
  let tp = Context.boolean c1 in
  let lst = List.map (fun e -> e, Some tp) (rlst @ elst) in
  let tps,fgs,rvar,lst = analyze_term_list entlst.i true lst c1 in
  let rlst,elst = Mylist.split_at (List.length rlst) lst in
  assert (not rvar);
  tps,fgs,rlst,elst


let general_term (e:expression) (tp:type_term option) (c:Context.t): info_term =
  assert(Context.count_type_variables c = 0);
  let _,_,_,lst = analyze_term_list e.i false [e,tp] c in
  match lst with
  |  [t] ->
      t
  | _ ->
     assert false (* cannot happen *)


let untyped_term (e:expression) (c:Context.t): info_term =
  assert(Context.count_type_variables c = 0);
  general_term e None c


let typed_term (e:expression) (tp:type_term) (c:Context.t): info_term =
  assert(Context.count_type_variables c = 0);
  general_term e (Some tp) c


let boolean_term (e:expression) (c:Context.t): info_term =
  assert(Context.count_type_variables c = 0);
  general_term e (Some (Context.boolean c)) c


let result_term (e:expression) (c:Context.t): term =
  assert(Context.count_type_variables c = 0);
  assert (not (Context.is_global c));
  assert (Context.has_result c);
  let tp = Context.result_type c in
  (general_term e (Some tp) c).v


let case_pattern (pat:expression) (tp:type_term) (c:Context.t): names * term =
  assert(Context.count_type_variables c = 0);
  let pat,nms = case_variables pat false c in
  let n = Array.length nms in
  let c1 = Context.push_untyped nms c
  and tp = Term.up n tp
  in
  let _,_,_,lst = analyze_term_list pat.i false [pat,Some tp] c1 in
  let pat =
    match lst with
    |  [t] ->
        t.v
    | _ ->
       assert false (* cannot happen *)
  in
  nms,pat
