(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Container
open Term
open Proof
open Support
open Printf

module Option = Fmlib.Option
module RD = Rule_data

type slot_data = {ndown:int;
                  sprvd: int TermMap.t}

type var_def_data = {
    definition: int option;
    used: int list
  }


type entry = {mutable prvd: Term_table.t;  (* all proved (incl. schematic) terms *)
              mutable bwd:  Term_table.t;
              mutable fwd:  Term_table.t;
              mutable left: Term_table.t;
              mutable reeval: Term_table.t;
              mutable slots: slot_data array}


type gdesc = {mutable pub: bool;
              (*deferred: int option; ( * The owner class of a deferred assertion *)
              defer: bool;
              anchor: int}

type t = {base:   Proof_table.t;
          terms:  RD.t Ass_seq.t;
          gseq:   gdesc Seq.t;
          mutable def_ass: Term_table.t;
          depth:  int;
          mutable work:   int list;
          count0: int;
          entry:  entry;
          prev:   t option;
          var_defs: var_def_data array;
          trace:  bool;
          verbosity: int}

let verbosity (pc:t): int = pc.verbosity

let is_tracing (pc:t): bool = pc.verbosity >= 3

let context (pc:t): Context.t = Proof_table.context pc.base

let feature_table (pc:t): Feature_table.t =
  let c = context pc in
  Context.feature_table c

let class_table (pc:t): Class_table.t =
  let c = context pc in
  Context.class_table c

let is_private (pc:t): bool = Proof_table.is_private pc.base
let is_public  (pc:t): bool = Proof_table.is_public  pc.base
let is_interface_use   (pc:t): bool = Proof_table.is_interface_use  pc.base
let is_interface_check (pc:t): bool = Proof_table.is_interface_check  pc.base

let add_used_module (m:Module.M.t) (pc:t): unit =
  Proof_table.add_used_module m pc.base

let add_current_module (m:Module.M.t) (pc:t): unit =
  Proof_table.add_current_module m pc.base

let set_interface_check (pc:t): unit =
  Proof_table.set_interface_check pc.base



let make_entry () =
  let e = Term_table.empty in
  {prvd=e; bwd=e; fwd=e; left=e; reeval=e;
   slots = Array.make 1 {ndown = 0; sprvd = TermMap.empty}}


let copied_entry (e:entry): entry =
  {prvd  = e.prvd;
   bwd   = e.bwd;
   fwd   = e.fwd;
   left  = e.left;
   reeval = e.reeval;
   slots = Array.copy e.slots}




let make (comp:Module.Compile.t): t  =
  let verbosity = Module.Compile.verbosity comp in
  let res =
    {base     = Proof_table.make comp;
     terms    = Ass_seq.empty ();
     gseq     = Seq.empty ();
     def_ass  = Term_table.empty;
     depth    = 0;
     prev     = None;
     var_defs = [||];
     work     = [];
     count0   = 0;
     entry    = make_entry ();
     trace    = verbosity >= 3;
     verbosity= verbosity}
  in
  res


let is_global (at:t): bool =
  Proof_table.is_global at.base

let is_local (at:t): bool =
  Proof_table.is_local at.base

let is_toplevel (at:t): bool =
  Proof_table.is_toplevel at.base

let nbenv (at:t): int = Proof_table.count_variables at.base

let count_all_type_variables (pc:t): int =
  Context.count_all_type_variables (context pc)

let count_type_variables (pc:t): int =
  Context.count_type_variables (context pc)


let count_variables (at:t): int =
  Proof_table.count_variables at.base


let count_last_arguments (pc:t): int =
  Proof_table.count_last_arguments pc.base

let count_last_variables (pc:t): int =
  Proof_table.count_last_variables pc.base

let count_last_type_variables (pc:t): int =
  Proof_table.count_last_type_variables pc.base

let local_argnames (pc:t): int array =
  Proof_table.local_argnames pc.base

let local_formals (pc:t): formals0 =
  Proof_table.local_formals pc.base

let local_fgs (pc:t): formals0 =
  Proof_table.local_fgs pc.base

let count_base (pc:t): int = Proof_table.count pc.base

let count (pc:t): int = Ass_seq.count pc.terms

let is_consistent (pc:t): bool =
  count_base pc = count pc

let count_previous (pc:t): int = Proof_table.count_previous pc.base
let count_global(pc:t): int = Proof_table.count_global pc.base

let imp_id(pc:t): int = Proof_table.imp_id pc.base

let term (i:int) (pc:t): term =
  (** The [i]th proved term in the current environment.
   *)
  assert (i < count pc);
  assert (count pc <= count_base pc);
  Proof_table.local_term i pc.base


let depth (pc:t): int = pc.depth

let trace_prefix_0 (pc:t): string =
  assert (not (is_global pc));
  String.make (3 + 2*(pc.depth-1)) ' '


let trace_prefix (pc:t): string =
  String.make (3 + 2*pc.depth) ' '

let trace_pop (pc:t): unit =
  printf "%send\n" (trace_prefix_0 pc)


let pop (pc:t): t =
  assert (is_local pc);
  match pc.prev with
    None -> assert false
  | Some x -> x


let trace_push (pc:t): unit =
  let str = Proof_table.last_arguments_string pc.base in
  let prefix = trace_prefix_0 pc in
  if str <> "" then printf "%sall%s\n" prefix str;
  printf "%srequire\n" prefix


let push_slots (nbenv:int) (pc:t): unit =
  pc.entry.slots <-
    if nbenv=0 then
      Array.copy pc.entry.slots
    else
      let len = Array.length pc.entry.slots in
      Array.init
        (len+1)
        (fun i ->
          if i<len then
            let sd = pc.entry.slots.(i) in
            {sd with ndown = sd.ndown+nbenv}
          else
            {ndown=0; sprvd=TermMap.empty})


let push0 (base:Proof_table.t) (pc:t): t =
  let nbenv = Proof_table.count_variables base
  and nargs = Proof_table.count_last_variables base in
  let var_defs =
    Array.init
      nbenv
      (fun i ->
        if i < nargs then
          {definition = None; used = []}
        else
          pc.var_defs.(i-nargs)
      )
  in
  let res = {pc with
             base  = base;
             terms = Ass_seq.clone pc.terms;
             work  = pc.work;
             depth = 1 + pc.depth;
             count0 = count pc;
             entry  = copied_entry pc.entry;
             var_defs = var_defs;
             prev   = Some pc} in
  push_slots nbenv res;
  if res.trace then
    trace_push res;
  res


let push
    (entlst:entities list withinfo)
    (rt:return_type)
    (is_pred:bool)
    (is_func:bool)
    (rvar: bool)
    (pc:t): t =
  let base = Proof_table.push entlst rt is_pred is_func rvar pc.base in
  push0 base pc



let push_typed (fargs:Formals.t) (fgs:Formals.t) (rvar:bool) (pc:t): t =
  let base = Proof_table.push_typed fargs fgs rvar pc.base in
  push0 base pc

let push_typed0 (fargs:Formals.t) (fgs:Formals.t) (pc:t): t =
  push_typed fargs fgs false pc


let push_empty (pc:t): t =
  let base = Proof_table.push_empty pc.base in
  push0 base pc


let arguments_string (pc:t): string =
  Context.arguments_string (context pc)

let rec global (pc:t): t =
  if is_global pc then
    pc
  else
    global (pop pc)


let type_of_term (t:term) (pc:t): type_term =
  Context.type_of_term t (context pc)


let rule_data (idx:int) (pc:t): RD.t =
  assert (idx < count pc);
  Ass_seq.elem idx pc.terms

let is_fully_specialized (idx:int) (pc:t): bool =
  RD.is_fully_specialized (rule_data idx pc)


let is_assumption (i:int) (pc:t): bool =
  assert (i < count pc);
  Proof_table.is_assumption i pc.base


let is_local_assumption (i:int) (pc:t): bool =
  Proof_table.is_local_assumption i pc.base


let count_local_assumptions (pc:t): int =
  Proof_table.count_local_assumptions pc.base

let tvars (pc:t): Tvars.t =
  Context.tvars (context pc)

let signature (pc:t): Signature.Sign.t =
  Context.signature (context pc)

let string_of_term (t:term) (pc:t): string =
  Context.string_of_term t (context pc)

let string_long_of_term (t:term) (pc:t): string =
  Context.string_long_of_term t (context pc)


let string_of_term_anon (t:term) (nb:int) (pc:t): string =
  Context.string_of_term0 t true false nb (context pc)


let string_of_term_i (i:int) (pc:t): string =
  assert (i < count pc);
  Proof_table.string_of_term_i i pc.base


let string_long_of_term_i (i:int) (pc:t): string =
  assert (i < count pc);
  Proof_table.string_long_of_term_i i pc.base


let string_of_term_array (args: term array) (pc:t): string =
  "[" ^
  (String.concat ","
     (List.map (fun t -> string_of_term t pc) (Array.to_list args)))
  ^
    "]"



let string_of_tvs (pc:t): string =
  Class_table.string_of_tvs (tvars pc) (class_table pc)


let string_of_type (tp:type_term) (pc:t): string =
  Context.string_of_type tp (context pc)

let string_of_ags (ags:agens) (pc:t): string =
  Context.string_of_ags ags (context pc)


let is_visible (i:int) (pc:t): bool =
  not (is_interface_check pc) ||
  let ft = feature_table pc
  and t,c  = Proof_table.term i pc.base in
  let nb = Context.count_variables c in
  Feature_table.is_term_visible t nb ft


let split_implication (t:term) (pc:t): term * term =
  Proof_table.split_implication t pc.base

let implication (a:term) (b:term) (pc:t): term =
  Proof_table.implication a b pc.base

let negation (a:term) (pc:t): term =
  let nb = nbenv pc in
  Term.unary (nb + Constants.not_index) a

let disjunction (a:term) (b:term) (pc:t): term =
  let nb = nbenv pc in
  Term.binary (nb + Constants.or_index) a b


let false_constant (pc:t): term =
  let nb = nbenv pc in
  Feature_table.false_constant nb


let negation_expanded (a:term) (pc:t): term =
  implication a (false_constant pc) pc



let split_general_implication_chain
    (t:term) (pc:t): Formals.t * Formals.t * term list * term =
  Context.split_general_implication_chain t (context pc)


let all_quantified (tps:Formals.t) (fgs:Formals.t) (t:term) (pc:t): term =
  Proof_table.all_quantified tps fgs t pc.base


let prenex_term (t:term) (pc:t): term =
  Proof_table.prenex_term t pc.base

let implication_chain (ps:term list) (tgt:term) (pc:t): term  =
  Proof_table.implication_chain ps tgt pc.base


let assumptions (pc:t): term list =
  Proof_table.assumptions pc.base

let assumption_indices (pc:t): int list =
  Proof_table.assumption_indices pc.base


let assumptions_chain (tgt:term) (pc:t): term =
  implication_chain (List.rev (assumptions pc)) tgt pc


let assumptions_for_variables_0
    (vars: int array)      (* The variables *)
    (pc:t)
    : int list * int =
  (* All assumptions of the contexts which are needed to define the variables
     [vars] and the number of variables in these contexts.
   *)
  let nvars = Array.length vars in
  assert (0 < nvars);
  let var_max =
    Array.fold_left (fun v i -> max v i) (-1) vars
  in
  let rec collect (nargs:int) (ass:int list) (pc:t): int list * int =
    (* collect while [nargs <= var_max] *)
    let idx_lst_rev = assumption_indices pc in
    let ass = List.rev_append idx_lst_rev ass
    in
    let nargs_new = nargs + count_last_variables pc in
    if var_max < nargs_new then
      ass, nargs_new
    else
      collect nargs_new ass (pop pc)
  in
  collect 0 [] pc


let assumptions_for_variables
    (ind_vars: int array)  (* The induction variables *)
    (insp_vars: int list)  (* All variables of the inspect expression *)
    (goal: term)
    (pc:t)
    : int list * int list * int =
  (* All assumptions of the contexts which are needed to define the variables
     [ind_vars], all the other variables which are not in [insp_vars] but
     in the contexts plus the variables in the goal and the total number of
     variables encounterd in the contexts.
   *)
  let ass, nvars = assumptions_for_variables_0 ind_vars pc in
  let used_lst =
      let used_lst_rev =
        List.fold_left
          (fun lst idx -> Term.used_variables_0 (term idx pc) nvars lst)
          (Term.used_variables goal nvars)
          ass
      in
      let insp_vars = Array.of_list insp_vars in
      Array.sort Pervasives.compare insp_vars;
      List.filter
        (fun i ->
          try ignore(Search.binsearch i insp_vars); false
          with Not_found -> true
        )
        (List.rev used_lst_rev)
  in
  ass, used_lst, nvars



let work (pc:t): int list = pc.work

let has_work (pc:t): bool = pc.work <> []

let clear_work (pc:t): unit =
  pc.work <- []


let has_result (pc:t): bool = Proof_table.has_result pc.base

let has_result_variable (pc:t): bool = Proof_table.has_result_variable pc.base

let seed_function (pc:t): int -> int =
  Feature_table.seed_function (feature_table pc)

let is_well_typed (t:term) (pc:t): bool =
  Context.is_well_typed t (context pc)


let transformed_to_current (t:term) (idx:int) (pc:t): term =
  Proof_table.transformed_to_current t idx pc.base



let unify_0 (t:term) (tab:Term_table.t) (pc:t): (int * Term_sub.t) list =
  List.rev (Term_table.unify t (nbenv pc) (seed_function pc) tab)



let args_and_ags_of_substitution
      (idx:int) (sub:Term_sub.t) (pc:t)
    : arguments * agens =
  let rd = rule_data idx pc in
  let args = Term_sub.arguments (Term_sub.count sub) sub in
  try
    let ags = RD.verify_specialization args (context pc) rd in
    args,ags
  with Not_found ->
    (*printf "cannot unify types of actual arguments with formal arguments\n";
      printf "   actuals %s\n" (string_of_term_array args pc);
      printf "   rule    %s\n" (string_long_of_term_i idx pc);*)
    raise Not_found


let unify
    (t:term) (tab:Term_table.t) (pc:t)
    : (int * arguments * agens) list =
  let lst = unify_0 t tab pc in
  List.fold_left
    (fun lst (idx,sub) ->
      try
        let args,ags = args_and_ags_of_substitution idx sub pc in
        (idx,args,ags) :: lst
      with Not_found ->
        lst
    )
    []
    lst


let unify_with_0
    (t:term) (nargs:int) (tab:Term_table.t) (pc:t)
    : (int * Term_sub.t) list =
  (* Find the terms which can be unified with [t] which has [nargs] arguments
     and comes from the current environment.
   *)
  let nvars = nbenv pc
  and tps,_,t0 = Term.all_quantifier_split_1 t
  in
  let nb = Formals.count tps in
  Term_table.unify_with t0 nb nargs nvars true (seed_function pc) tab



let unify_with
    (t:term) (nargs:int) (tps:types) (tab:Term_table.t) (pc:t)
    : (int * arguments) list =
  (* Find the terms which can be unified with [t] which has [nargs] arguments
     with types [tps] and comes from the current environment.
   *)
  let lst = unify_with_0 t nargs tab pc in
  List.fold_left
    (fun lst (idx,sub) ->
      (* [sub] is valid in the environment of [idx] *)
      let args = Term_sub.arguments (Term_sub.count sub) sub in
      let args = Array.map (fun t -> transformed_to_current t idx pc) args in
      let argtps = Array.map (fun t -> type_of_term t pc) args in
      if Term.equivalent_array tps argtps then
        (idx,args)::lst
      else
        lst
    )
    []
    lst


let is_trace_extended (pc:t): bool = 3 < pc.verbosity

let trace_term (t:term) (rd:RD.t) (search:bool) (dup:bool) (pc:t): unit =
  let str = string_long_of_term t pc
  and cnt    = count pc
  and prefix = trace_prefix pc in
  assert (cnt + 1 = count_base pc);
  let ext =
    if is_trace_extended pc then
      let pt = Proof_table.proof_term cnt pc.base in
      let ptstr = Proof_term.short_string pt
      and rdstr = RD.short_string rd
      and cntstr =
        (string_of_int cnt) ^
        (if is_global pc then "global" else "")
      in
      let str =
        (if search then "" else "n") ^
        (if dup then "d" else "") in
      let rstr = str ^ rdstr in
      cntstr ^ "'" ^
      ptstr ^
      (if rstr <> "" then "," else "") ^
      rstr ^
      "' "
    else
      ""
  in
  printf "%s%s%s\n" prefix ext str;
  if is_trace_extended pc then
    printf "%s\t%s\n" prefix (Term.to_string t);
  if is_global pc then printf "\n"



let find (t:term) (pc:t): int =
  let tps,fgs,t0 = Term.all_quantifier_split_1 t in
  let n = Formals.count tps
  and fgtps = Formals.types fgs in
  let nfgs = Array.length fgtps in
  let sublst =
    Term_table.find t0 n (nbenv pc) (seed_function pc) pc.entry.prvd
  in
  let idx,sub =
    List.find
      (fun (idx,sub) ->
        assert (Term_sub.is_empty sub);
        if nfgs = 0 then
          true
        else
          let _,fgs,_ =
            Term.all_quantifier_split_1 (term idx pc) in
          let fgtps_found = Formals.types fgs in
          Array.length fgtps_found = nfgs
          && Term.equivalent_array fgtps fgtps_found
      )
      sublst
  in
  idx



let has (t:term) (pc:t): bool =
  (** Is the term [t] already in the proof context [pc]?
   *)
  try
    ignore(find t pc);
    true
  with Not_found ->
    false


let has_in_view (t:term) (pc:t): bool =
  assert (is_global pc);
  try
    let i = find t pc in
    assert (i < count_global pc);
    if is_private pc then
      true
    else
      let gdesc = Seq.elem i pc.gseq in
      gdesc.pub
  with Not_found ->
    false




let equal_symmetry (pc:t): int =
  find (Feature_table.equal_symmetry_term ()) (global pc)


let leibniz_term (pc:t): int =
  find (Feature_table.leibniz_term ()) (global pc)


let split_equality (t:term) (pc:t): int * term * term =
  Proof_table.split_equality t 0 pc.base



let split_specific_equality (t:term) (pc:t): term * term =
  let nargs,left,right = split_equality t pc in
  if nargs = 0 then
    left, right
  else
    raise Not_found




let equality_data (i:int) (pc:t):  int * term * term =
  assert (count_variables pc = Proof_table.nbenv_term i pc.base);
  let rd = rule_data i pc in
  let nargs, eq_id, left, right = Rule_data.equality_data rd in
  if nargs = 0 then
    eq_id, left, right
  else
    raise Not_found


let add_variable_definition (v:int) (idx:int) (pc:t): unit =
  (* The assertion at [idx] is a valid definition assertion of the form

     v = exp
   *)
  assert (v < count_variables pc);
  if pc.trace then
    printf "%svariable definition %d %s\n"
           (trace_prefix pc)
           idx
           (string_of_term_i idx pc);
  let def = pc.var_defs.(v) in
  assert (not (Option.has def.definition));
  pc.var_defs.(v) <- {def with definition = Some idx}



let add_variable_usage (v:int) (idx:int) (pc:t): unit =
  (* The variable [v] is part of the definition term of another variable and
     has been used to substitute the other variable in an assertion to get the
     assertion at [idx]
   *)
  assert (v < count_variables pc);
  let def = pc.var_defs.(v) in
  assert (not (Option.has def.definition));
  if List.mem idx def.used then
    ()
  else
    pc.var_defs.(v) <- {def with used = idx :: def.used}




let variable_definition (v:int) (pc:t): int =
  (* The index of the defining assertin of the variable [v]. Raise [Not_found]
     if the variable has no definition.
   *)
  assert (v < count_variables pc);
  match pc.var_defs.(v).definition with
    None ->
      raise Not_found
  | Some idx ->
      idx


let variable_has_definition (v:int) (pc:t): bool =
  try
    ignore(variable_definition v pc);
    true
  with Not_found ->
    false


let extended_variable_definition (v:int) (pc:t): int * term =
  (* The index of the defining assertion of the variable [v] and its
     definition term.
   *)
  try
    let idx = variable_definition v pc in
    let t = term idx pc in
    let left,right = split_specific_equality t pc in
    assert (left = Variable v);
    idx, right
  with Not_found ->
    assert false (* Illegal call *)






let get_variable_definitions (t:term) (pc:t): (int*int*term) list =
  (* Get the list of all applicable variable definitions in the term [t].
     The definition consists of the index of the defining assertion, the
     variable and the definition term.

     The list must be applied in reversed form.
   *)
  let nvars = count_variables pc in
  let filter v = v < nvars && variable_has_definition v pc
  in
  let get_used t lst = Term.used_variables_filtered_0 t filter false lst
  in
  let get_defs var_lst =
    List.rev_map
      (fun v ->
        let idx,exp = extended_variable_definition v pc in
        idx,v,exp
      )
      var_lst
  in
  let rec get_definitions
      (rcnt:int)
      (vars_new: int list)
      (defs:(int*int*term) list)
      : (int*int*term) list =
    assert (rcnt <= nvars);
    match vars_new with
      [] ->
        defs
    | _ ->
        let vars_new, defs =
          List.fold_left
            (fun (vs,defs) (idx,v,exp) ->
              let vs = get_used exp vs
              and defs = (idx,v,exp)::defs in
              vs,defs
            )
            ([],defs)
            (get_defs vars_new)
        in
        get_definitions (rcnt+1) vars_new defs
  in
  get_definitions 0 (get_used t []) []









let complexity (t:term) (pc:t): int =
  Context.complexity t (context pc)


let add_to_equalities (t:term) (idx:int) (pc:t): unit =
  (* Check the assertion [t] at [idx] if it is a simplifying equality. If yes, add it
     to the equality table.
   *)
  let nbenv = nbenv pc in
  try
    let nargs, left,right = split_equality t pc in
    let is_simpl =
      if 0 < nargs then false (*Term.nodes right < Term.nodes left*)
      else
        complexity right pc < complexity left pc
    in
    if is_simpl then begin
      (*printf "add_to_equalities %d %s   <%s>\n"
        idx (string_of_term t pc) (Term.to_string t);*)
      pc.entry.left <-
        Term_table.add left nargs nbenv idx (seed_function pc) pc.entry.left
    end
  with Not_found ->
    ()


let has_public_deferred (t:term) (pc:t): bool =
  assert (is_global pc);
  try
    let sublst = Term_table.find t 0 0 (seed_function pc) pc.def_ass in
    match sublst with
      [] ->
      false
    | [idx,sub] ->
       true
    | _ ->
       assert false (* no duplicates *)
  with Not_found ->
    false



let add_to_public_deferred (t:term) (idx:int) (pc:t): unit =
  assert (is_global pc);
  if not (has_public_deferred t pc) then
    let sfun = seed_function pc in
    pc.def_ass  <- Term_table.add t 0 0 idx sfun pc.def_ass


let add_to_proved (t:term) (idx:int) (pc:t): unit =
  let sfun = seed_function pc in
  let tps,_,t0 = Term.all_quantifier_split_1 t in
  let n = Formals.count tps in
  pc.entry.prvd <- Term_table.add t0 n (count_variables pc) idx sfun pc.entry.prvd



let add_to_forward (rd:RD.t) (idx:int) (pc:t): unit =
  if not (RD.is_forward rd) then
    ()
  else begin
    let nargs,_,nbenv,t = RD.schematic_premise rd in
    pc.entry.fwd <-
      Term_table.add t nargs nbenv idx (seed_function pc) pc.entry.fwd
  end


let add_to_backward (rd:RD.t) (idx:int) (pc:t): unit =
  if not (RD.is_backward rd) then begin
    ()
  end else begin
    let nargs,nbenv,t = RD.schematic_target rd in
    pc.entry.bwd <-
      Term_table.add t nargs nbenv idx (seed_function pc) pc.entry.bwd
  end




let add_last_to_tables (pc:t): unit =
  assert (0 < count pc);
  let idx = count pc - 1 in
  let t = term idx pc
  and rd = rule_data idx pc in
  assert (not (has t pc));
  add_to_proved   t idx pc;
  add_to_forward  rd idx pc;
  add_to_backward rd idx pc;
  add_to_equalities t idx pc;
  assert (has t pc)





let filter_tables (pred:int->bool) (pc:t): unit =
  assert (is_global pc);
  let e = pc.entry in
  e.prvd  <- Term_table.filter pred e.prvd;
  e.bwd   <- Term_table.filter pred e.bwd;
  e.fwd   <- Term_table.filter pred e.fwd;
  e.left  <- Term_table.filter pred e.left


let filter_and_remap_tables (pred:int->bool) (pc:t): unit =
  assert (is_global pc);
  let e = pc.entry
  and f = seed_function pc in
  e.prvd  <- Term_table.filter_and_remap pred f e.prvd;
  e.bwd   <- Term_table.filter_and_remap pred f e.bwd;
  e.fwd   <- Term_table.filter_and_remap pred f e.fwd;
  e.left  <- Term_table.filter_and_remap pred f e.left



let add_to_work (idx:int) (pc:t): unit =
  pc.work <- idx :: pc.work

let add_last_to_work (pc:t): unit =
  assert (0 < count pc);
  if not (is_global pc || is_interface_use pc) then
    let idx = count pc - 1 in
    pc.work <- idx :: pc.work




let get_rule_data (t:term) (pc:t): RD.t =
  RD.make t (context pc)


let raw_add0 (t:term) (rd:RD.t) (search:bool) (pc:t): int =
  assert (count pc + 1 = count_base pc);
  let cnt = count pc in
  let res =
    try find t pc
    with Not_found -> cnt
  in
  let dup = res <> cnt in
  if pc.trace && ((search && not dup) || is_trace_extended pc) then
    trace_term t rd search dup pc
  else if is_global pc && verbosity pc > 1 then begin
      printf "\n%s\n" (string_of_term t pc);
      flush_all ()
    end;
  Ass_seq.push rd pc.terms;
  if search && not dup then
    begin
      add_last_to_tables pc;
      if not dup && is_global pc then
        Feature_table.add_involved_assertion cnt t (feature_table pc);
      List.iter
        (fun (i,_) ->
          add_to_work i pc;
          if is_tracing pc then
            begin
              printf "reevaluate %s\n" (string_of_term_i i pc);
              printf "  because of %s\n" (string_of_term t pc)
            end
        )
        (unify_0 t pc.entry.reeval pc)
    end;
  if not dup && is_global pc then
    Induction.put_assertion res t (context pc);
  res


let raw_add (t:term) (search:bool) (pc:t): int =
  raw_add0 t (get_rule_data t pc) search pc



let raw_add_work (t:term) (search:bool) (pc:t): int =
  let idx = raw_add t search pc in
  if search && idx + 1 =  count pc then
    add_last_to_work pc;
  idx



let arguments_of_sub (sub:Term_sub.t): term array =
  let nargs = Term_sub.count sub in
  let args = Term_sub.arguments nargs sub in
  args



let specialized
    (idx:int) (args:arguments) (ags:agens)
    (reason:int) (* 0: match, 1: fwd, 2: bwd *)
    (pc:t): int =
  (* The schematic rule [idx] specialized by the arguments [args] and the
     actual generics [ags].

     Note: The arguments [args] are valid in the current enviroment!
   *)
  assert (is_consistent pc);
  assert (idx < count pc);
  let rd    = rule_data idx pc in
  if RD.is_specialized rd then
    begin
      if Array.length args <> 0 then
        begin
          printf "specialize\n";
          printf "    idx %d  term %s\n" idx (string_long_of_term_i idx pc);
          printf "    |args| %d\n" (Array.length args);
        end;
      assert (Array.length args = 0); idx
    end
  else if Array.length args = 0 && RD.count_args_to_specialize rd > 0 then
    idx
  else begin
    let rd    = RD.specialize rd args ags idx (context pc) in
    let t     = RD.term rd in
    try
      find t pc
    with Not_found ->
      let search =
        if reason = 0 then
          false
        else if reason = 1 then
          not (RD.is_forward rd) && RD.is_backward rd
        else
          true
      in
      Proof_table.add_specialize t idx args ags pc.base;
      raw_add0 t rd search pc
  end



let find_schematic (t:term) (nargs:int) (pc:t): int * agens =
  (* Find a universally quantified assertion and a substitution for it so that
     the substituted assertion is the same as the term 't' and the substitution
     is the indentity substitution with [nargs] arguments.
   *)
  let sublst = unify t pc.entry.prvd pc in
  let sublst =
    List.filter
      (fun (_,args,_) ->
        Array.length args = nargs &&
        interval_for_all
          (fun i ->
            args.(i) = Variable i
          )
          0 nargs
      )
      sublst
  in
  match sublst with
    [] ->
      raise Not_found
  |  (idx,_,ags):: tail ->
      idx, ags



let find_match (g:term) (pc:t): int =
  try
    find g pc
  with Not_found ->
    let sublst = unify g pc.entry.prvd pc in
    if sublst = [] then
      raise Not_found;
    try
      let idx,_,_ =
        List.find
          (fun (_,args,_) -> Array.length args = 0)
          sublst in
      idx
    with Not_found ->
      let idx,args,ags = List.hd sublst in
      try
        specialized idx args ags 0 pc
      with Not_found ->
        assert false (* specialization not type safe ? *)



let find_match_list (lst:term list) (pc:t): int list =
  List.fold_left
    (fun lst t -> (find_match t pc) :: lst)
    []
    (List.rev lst)


let simplified_term (t:term) (below_idx:int) (pc:t): term * Eval.t * bool =
  (* Simplify the term [t] and return the simplified term, the corresponding Eval
     structure and a flag which tells if the term [t] and its simplification are
     different.

     [below_idx]: consider only rules below [below_idx] for equality.

     Note: [t] is valid in the current environment!
   *)
  let rec simp t =
    let do_subterms t =
      let simpl_args args modi =
        let arglst, modi =
          Array.fold_left
            (fun (arglst,modi) a ->
              let asimp,ae,amodi = simp a in
              (asimp,ae)::arglst, modi || amodi)
            ([],modi)
            args
        in
        let args, argse = Myarray.split (Array.of_list (List.rev arglst)) in
        args, argse, modi
      in
      match t with
        Variable _ ->
          t, Eval.Term t, false
      | VAppl (i,args,ags,oo) ->
          let args, argse, modi = simpl_args args false in
          VAppl(i,args,ags,oo),
          Eval.VApply(i,argse,ags),
          modi
      | Application(f,args,inop) ->
          let fsimp,fe,fmodi = simp f in
          let args, argse, modi = simpl_args args fmodi in
          let res  = Application(fsimp, args, inop) in
          let rese = Eval.Term res in
          res,
          Eval.Apply(fe,argse,rese),
          modi
      | Lam _ | QExp _ | Ifexp _ | Asexp _ | Inspect _ | Indset _ ->
         t, Eval.Term t, false
    in
    let sublst = unify t pc.entry.left pc in
    let sublst =
      List.filter (fun (idx,sub,_) -> idx < below_idx && Array.length sub = 0) sublst
    in
    match sublst with
      (idx,_,ags) :: _ ->
        (* Note: This is a workaround. Only single entries in the equality table
                 should be accepted. But multiple entries are possible as long we
                 do not make specializations type safe. *)
        let eq = term idx pc in
        let nargs, left, right = Proof_table.split_equality eq 0 pc.base in
        assert (nargs = 0);
        assert (Term.equivalent t left);
        right, Eval.Simpl(Eval.Term t,idx,[||],ags), true
    | _ ->
        do_subterms t
  in
  let tsimp, te, modi = simp t in
  let ta, tb = Proof_table.reconstruct_evaluation te pc.base in
  assert (Term.equivalent ta t);
  if not (Term.equivalent tb tsimp) then begin
    printf "simplified_term  %s\n" (string_of_term t pc);
    printf "           tb    %s\n" (string_of_term tb pc);
    printf "           tsimp %s\n" (string_of_term tsimp pc);
    assert (is_well_typed t pc);
  end;
  assert (Term.equivalent tb tsimp);
  assert (modi = not (Term.equivalent tsimp t));
  (*if modi then begin
    printf "simplification found\n";
    printf "  term    %s\n" (string_of_term t pc);
    printf "  simpl   %s\n" (string_of_term tsimp pc);
    end;*)
  tsimp, te, modi




let triggers_eval (i:int) (nb:int) (pc:t): bool =
  (* Does the term [Variable i] trigger a full evaluation when used as a top
     level function term, i.e. is it a variable which describes a function
     which has no expansion and is not owned by BOOLEAN? *)
  let nbenv = nb + nbenv pc
  and ft    = feature_table pc in
  i < nbenv ||
  let idx = i - nbenv in
  idx = Constants.or_index ||
  Feature_table.owner idx ft <> Constants.boolean_class



let beta_reduce
    (n:int) (t:term) (tup_tp:type_term) (args:term array) (nb:int) (pc:t)
    : term =
  Proof_table.beta_reduce n t tup_tp args nb pc.base


let beta_reduce_term (t:term) (pc:t): term =
  match t with
  | Application (Lam(tps,_,_,t0,_), args, _) ->
     let n = Formals.count tps
     and tup_tp =
       Context.tuple_type_of_types
         (Formals.types tps)
         (context pc) in
      beta_reduce n t0 tup_tp args 0 pc
  | _ ->
      printf "beta_reduce_term %s\n" (string_long_of_term t pc);
      assert false (* Is not a redex *)


let make_lambda
    (tps:Formals.t) (fgs:Formals.t) (ps: term list) (t:term)
    (rt: type_term option)
    (pc:t)
    : term =
  let c = context pc in
  Context.make_lambda tps fgs ps t rt c


let make_application
    (f:term) (args:term array) (tup:type_term) (nb:int) (pc:t)
    : term =
  let c = context pc in
  Context.make_application f args tup nb c


let is_inductive_set (i:int) (pc:t): bool =
  Proof_table.is_inductive_set i pc.base


let inductive_set (t:term) (pc:t): term =
  Proof_table.inductive_set t pc.base


let definition_term (i:int) (nb:int) (ags:agens) (pc:t): int * int array * term =
  if i < nb || is_inductive_set (i-nb) pc then
    raise Not_found
  else
    Proof_table.definition_term i nb ags pc.base

let arity (i:int) (nb:int) (pc:t): int =
  Proof_table.arity i nb pc.base


exception Undecidable of term
exception No_evaluation of term option
exception No_branch_evaluation of term option

let decide (t:term) (pc:t): bool * int =
  try
    true, find_match t pc
  with Not_found ->
       try
         false, find_match (negation_expanded t pc) pc
       with  Not_found ->
         raise (Undecidable t)

let string_of_term_option (t:term option) (pc:t): string =
  match t with
  | None -> "None"
  | Some t ->
     "Some(" ^ string_of_term t pc ^ ")"

let chain_term_option (t1:term option) (t2:term option): term option =
  match t1 with
  | None ->
     t2
  | Some _ ->
     t1

let eval_term (t:term) (pc:t): term * Eval.t * term option =
  (* Evaluate the term [t] and return the evaluated term, the corresponding Eval
     structure. Raise [No_evaluation] if the term does not have an evaluation.
   *)
  let rec eval
            (t:term) (lazy_:bool) (depth:int) (pc:t)
          : term * Eval.t * term option =
    if depth > 500 then
      raise (No_evaluation None);
    let depth = depth + 1 in
    let nvars = nbenv pc in
    let domain_id = nvars + Constants.domain_index
    in
    match t with
      Variable i ->
        assert (i < nvars);
        raise (No_evaluation None)
    | VAppl (i,[|Lam(tps,fgs,pres,t0,rt)|],ags,inop) when i = domain_id ->
       assert (rt <> None);
       let args = [|Eval.Term (Lam(tps,fgs,pres,t0,rt))|]
       and dom = Context.domain_of_lambda tps fgs pres 0 (context pc) in
       dom, Eval.Exp(i, ags, args, Eval.Term dom),None
    | VAppl(i,args,ags,oo) ->
        eval_vappl t i args ags oo lazy_ depth pc
    | Application (Lam(tps,fgs,_,t0,_), args, inop) ->
       let tup_tp =
         Context.tuple_type_of_types (Formals.types tps) (context pc)
       and n = Formals.count tps
       in
        let reduct = beta_reduce n t0 tup_tp args 0 pc
        and te = Eval.Term t in
        begin try
          let res,rese,cond = maybe_eval reduct lazy_ depth pc in
          let e = Eval.Beta (te, rese) in
          res, e, cond
        with No_branch_evaluation cond ->
          raise (No_evaluation cond)
        end
    | Application (f,args,inop) ->
        assert (Array.length args = 1);
        begin
          try
            let f_exp,fe,fcond = eval f lazy_ (depth - 1) pc
            and argse = [| Eval.Term args.(0) |] in
            let t_exp,te,tcond =
              maybe_eval (Application(f_exp,args,inop)) lazy_ depth pc in
            let cond = match fcond with
              | None -> tcond
              | Some _ -> fcond in
            t_exp, Eval.Apply(fe,argse,te), cond
          with No_evaluation cond ->
            if depth = 1 then
              let args,argse,cond = eval_args args (depth - 1) pc in
              let t_exp = Application(f,args,inop) in
              let fe = Eval.Term f
              and te = Eval.Term t_exp
              in
              t_exp, Eval.Apply(fe, argse,te), cond
            else
              raise (No_evaluation cond)
        end
    | Lam _ ->
        raise (No_evaluation None)
    | QExp _ ->
        raise (No_evaluation None)
    | Ifexp (c, a, b) ->
       eval_if c a b lazy_ depth pc
    | Asexp (insp, tps, pat) ->
       eval_as t insp tps pat lazy_ depth pc
    | Inspect (insp, cases) ->
       eval_inspect t insp cases lazy_ depth pc
    | Indset _ ->
        raise (No_evaluation None)

  and eval_vappl
      (t:term) (i:int) (args:arguments) (ags:agens) (oo:bool)
      (lazy_:bool) (depth:int) (pc:t)
      : term * Eval.t * term option =
    let nvars = nbenv pc in
    let and_id    = nvars + Constants.and_index
    and or_id     = nvars + Constants.or_index
    and imp_id    = nvars + Constants.implication_index
    in
    let is_lazy i = i = and_id || i = or_id || i = imp_id
    in
    try
      let n,nms,t0 = definition_term i 0 ags pc
      and argse = Array.map (fun t -> Eval.Term t) args
      in
      assert (n = Array.length args);
      let t_expanded = Proof_table.apply_term t0 args 0 pc.base in
      begin
        try
          let res, rese,cond = maybe_eval t_expanded lazy_ depth pc in
          res, Eval.Exp(i,ags,argse,rese), cond
        with No_branch_evaluation cond_t ->
             try
               let args,argse,cond = eval_args args depth pc in
               let cond = chain_term_option cond_t cond in
               VAppl(i,args,ags,oo), Eval.VApply(i,argse,ags), cond
             with No_evaluation cond ->
               let cond = chain_term_option cond_t cond in
               let argse = Array.map (fun t -> Eval.Term t) args in
               VAppl(i,args,ags,oo), Eval.VApply(i,argse,ags), cond
          (*let args,argse,cond = eval_args args depth pc in
          let cond = match cond_t with
            | None -> cond
            | Some _ -> cond_t
          in
          VAppl(i,args,ags,oo), Eval.VApply(i,argse,ags), cond*)
      end
    with Not_found -> (* No definition *)
      if Array.length args = 0 || (lazy_ && depth > 1) || is_lazy i then
        raise (No_evaluation None)
      else
        let args,argse,cond = eval_args args depth pc in
        VAppl(i,args,ags,oo), Eval.VApply(i,argse,ags), cond

  and eval_args
      (args:arguments) (depth:int) (pc:t)
      : arguments * Eval.t array * term option =
    let len = Array.length args in
    let args  = Array.copy args
    and argse = Array.map (fun t -> Eval.Term t) args in
    let modi,cond =
      interval_fold
        (fun (modi,cond) i ->
          try
            let t, te, cond_i = eval args.(i) false depth pc in
            args.(i)  <- t;
            argse.(i) <- te;
            true,
            if cond = None then cond_i else cond
          with No_evaluation cond_i ->
            if cond = None then
              modi,cond_i
            else modi, cond
        )
        (false,None) 0 len
    in
    if modi then
      args, argse, cond
    else
      raise (No_evaluation cond)

  and eval_if
      (cond:term) (a:term) (b:term) (lazy_:bool) (depth:int) (pc:t)
      : term * Eval.t * term option =
    let conde = Eval.Term cond in
    try
      let res,idx = decide cond pc in
      if res then
        let fst,fste,fst_cond = maybe_eval a lazy_ depth pc in
        fst, Eval.If (true, idx, [|conde; fste; Eval.Term b|]), fst_cond
      else
        let snd,snde,snd_cond = maybe_eval b lazy_ depth pc in
        snd, Eval.If (false, idx, [|conde; Eval.Term a; snde|]), snd_cond
    with Undecidable cond ->
      raise (No_branch_evaluation (Some cond))

  and eval_inspect
        (t:term)
        (insp:term)
        (cases: (formals*term*term) array)
        (lazy_:bool)
        (depth:int)
        (pc:t)
      : term * Eval.t * term option =
    let insp, inspe, icond = maybe_eval insp true depth pc
    and c = context pc
    in
    match Pattern.decide_inspect insp cases c with
    | None ->
       raise (No_branch_evaluation None)
    | Some (i, args, pres) ->
       try
         ignore(find_match_list pres pc); (* nyi: include in proof term *)
         let _,_,res = cases.(i) in
         let res = Term.apply res args in
         let res,rese,rcond = maybe_eval res lazy_ depth pc in
         let cond = chain_term_option icond rcond in
         res, Eval.Inspect(t,inspe,i,rese), cond
       with Not_found ->
         let pre =
           try
             List.find
               (fun pre ->
                 try
                   ignore(find_match pre pc);
                   false
                 with Not_found ->
                   true)
               pres
           with Not_found ->
             assert false (* Cannot happen *)
         in
         raise (No_branch_evaluation (Some pre))

  and eval_as
        (t:term) (insp:term) (tps:types) (pat:term) (lazy_:bool) (depth:int) (pc:t)
      : term * Eval.t * term option =
    let nvars = nbenv pc in
    let n = Array.length tps in
    let insp,inspe,cond = maybe_eval insp lazy_ depth pc in
    let c = context pc in
    let undecide () =
      Pattern.evaluated_as_expression t (context pc),
      Eval.AsExp t,
      cond
    in
    let find_pres pres =
      try
        find_match_list pres pc
      with Not_found ->
        raise Support.Undecidable
    in
    try
      begin
        match Pattern.unify_with_pattern insp n pat c with
        | None ->
           undecide ()
        | Some (Error (pres) ) ->
           ignore(find_pres pres); (* nyi: include in proof term *)
           Feature_table.false_constant nvars,
           Eval.As(false,inspe,tps,pat),
           cond
        | Some ( Ok (args,pres) ) ->
           ignore(find_pres pres); (* nyi: include in proof term *)
           Feature_table.true_constant nvars,
           Eval.As(true,inspe,tps,pat),
           None
      end
    with Support.Undecidable ->
      undecide ()

  and maybe_eval
        (t:term) (lazy_:bool) (depth:int) (pc:t)
      : term * Eval.t * term option=
    try
      eval t lazy_ depth pc
    with No_evaluation cond ->
      t, Eval.Term t, cond
  in
  try
    eval t true 0 pc
  with No_branch_evaluation cond ->
    raise (No_evaluation cond)



let evaluated_term (t:term) (pc:t): term * Eval.t * term option * bool =
  try
    let t,e,cond = eval_term t pc in
    t, e, cond, true
  with No_evaluation cond ->
    t, Eval.Term t, cond, false




let add_mp0 (t:term) (i:int) (j:int) (search:bool) (pc:t): int =
  (* Add the term [t] by applying the modus ponens rule with [i] as the premise
     and [j] as the implication. *)
  let cnt = count pc
  and rd  = RD.drop (rule_data j pc) (context pc)
  in
  Proof_table.add_mp t i j pc.base;
  (if RD.is_implication rd then
    let _ = raw_add0 t rd search pc in ()
  else
    let _ = raw_add t search pc in ());
  cnt



let add_mp (i:int) (j:int) (search:bool) (pc:t): int =
  (* Apply the modus ponens rule with [i] as the premise and [j] as the
     implication. *)
  assert (i < count pc);
  assert (j < count pc);
  let rdj   = rule_data j pc
  and c     = context pc
  in
  if not (RD.is_specialized rdj) then
    printf "add_mp %s\n" (string_long_of_term_i j pc);
  assert (RD.is_specialized rdj);
  assert (RD.is_implication rdj);
  let t = RD.term_b rdj c in
  if not (Term.equivalent (term i pc) (RD.term_a rdj c))
  then begin
    printf "add_mp premise     %d %s %s\n" i
      (string_long_of_term_i i pc) (Term.to_string (term i pc));
    printf "       implication %d %s %s\n"
      j (string_long_of_term_i j pc) (Term.to_string (term j pc));
    printf "       term_a         %s %s\n"
      (string_long_of_term (RD.term_a rdj c) pc)
      (Term.to_string (RD.term_a rdj c))
  end;
  assert (Term.equivalent (term i pc) (RD.term_a rdj c));
  try
    find t pc
  with Not_found ->
    add_mp0 t i j search pc



let try_add_beta_reduced (idx:int) (search:bool) (pc:t): int =
  let t = term idx pc in
  match t with
  | Application(Lam(tps,fgs,_,t0,_), [|arg|], _) ->
     let n = Formals.count tps
     and tup_tp =
       Context.tuple_type_of_types (Formals.types tps) (context pc)
     in
     let reduct = beta_reduce n t0 tup_tp [|arg|] 0 pc in
     let pt = Eval(idx, Eval.Beta (Eval.Term t, Eval.Term reduct)) in
     Proof_table.add_proved reduct pt 0 pc.base;
     raw_add_work reduct search pc
  | _ ->
     raise Not_found



let add_beta_reduced (idx:int) (search:bool) (pc:t): int =
  (* [idx] must represent a term which can be beta reduced *)
  try
    try_add_beta_reduced idx search pc
  with Not_found ->
    printf "add_beta_reduced\n";
    printf "   The term %d \"%s\" is not a beta redex\n"
           idx
           (string_of_term_i idx pc);
    assert false (* The term [idx] is not a beta redex *)



let add_beta_redex (t:term) (idx:int) (search:bool) (pc:t): int =
  (* The term [t] must be beta reducible and its beta reduction is the
     term [idx]. The term [t] is added.
   *)
  match t with
  | Application(Lam(tps,fgs,_,t0,_), [|arg|], _) ->
     let n = Formals.count tps
     and tup_tp = Context.tuple_type_of_types (Formals.types tps) (context pc)
     in
     let reduced = beta_reduce n t0 tup_tp [|arg|] 0 pc in
     let e1,e2 = Eval.Term t, Eval.Term reduced in
     let pt = Eval_bwd(t,Eval.Beta (e1,e2))     (* proves the implication
                                                   [t_idx ==> t] *)
     in
     let impl = implication reduced t pc in
     Proof_table.add_proved impl pt 0 pc.base;
     let idx_impl = raw_add  impl false pc in
     add_mp idx idx_impl search pc
  | _ ->
      assert false



let add_some_elim (i:int) (search:bool) (pc:t): int =
  (* The term [i] has the form [some(a,b,...) t0]. Add the elimination law

         all(e) (all(a,b,..) t0 ==> e) ==> e
   *)
  let t =
    try
      Proof_table.someelim i pc.base
    with Not_found ->
      assert false
  in
  Proof_table.add_proved t (Someelim i) 0 pc.base;
  raw_add t search pc




let add_some_elim_specialized (i:int) (goal:term) (search:bool) (pc:t): int =
  (* The term [i] has the form [some(a,b,...) t0]. Add the elimination law

         all(e) (all(a,b,..) t0 ==> e) ==> e

     and the specialized version

         (all(a,b,...) t0 ==> goal) ==> goal
   *)
  let idx = add_some_elim i search pc in
  specialized idx [|goal|] [||] 0 pc



let add_mp_fwd (i:int) (j:int) (pc:t): unit =
  let rdj = rule_data j pc in
  if RD.is_forward rdj then begin
    let cnt = count pc in
    let res = add_mp i j true pc in
    if res = cnt then
      add_last_to_work pc
  end


let is_nbenv_current (i:int) (pc:t): bool =
  assert (i < count pc);
  let nbenv_i = RD.count_variables (rule_data i pc) in
  nbenv_i = nbenv pc


let add_consequence
    (i:int ) (j:int) (args:arguments) (ags:agens) (pc:t): unit =
  (* Add the consequence of [i] and the implication [j]. The term [j] might
     be a schematic implication which has to be converted into a specific
     implication by using the substitution [sub].

     Note: The arguments [args] and the actual generics [ags] are valid in the
     current environment.
   *)
  (*printf "add_consequence\n";
  printf "    premise %d %s\n" i (string_long_of_term_i i pc);
  printf "    impl    %d %s\n" j (string_long_of_term_i j pc);
  printf "    args       %s\n"   (string_of_term_array args pc);*)
  assert (is_consistent pc);
  assert (i < count pc);
  assert (j < count pc);
  let nbenv_sub = Proof_table.nbenv_term i pc.base in
  assert (nbenv_sub <= nbenv pc);
  try
    let j = specialized j args ags 1 pc
    in
    add_mp_fwd i j pc
  with Not_found ->
    ()



let add_consequences_premise (i:int) (pc:t): unit =
  (** Add the consequences of the term [i] by using the term as a premise for
      already available implications.
   *)
  assert (i < count pc);
  if not (is_nbenv_current i pc) then
    printf "add_consequences_premise %s\n" (string_of_term_i i pc);
  assert (is_nbenv_current i pc);
  assert (not (RD.is_intermediate (rule_data i pc)));
  let nbenv = nbenv pc in
  let t,c_t = Proof_table.term i pc.base in
  assert (nbenv = Context.count_variables c_t);
  let sublst = unify t pc.entry.fwd pc in
  let sublst = List.rev sublst in
  List.iter
    (fun (idx,sub,ags) ->
      assert (is_consistent pc);
      assert (idx < count pc);
      if is_visible idx pc then
        add_consequence i idx sub ags pc)
    sublst





let add_consequences_implication (i:int) (rd:RD.t) (pc:t): unit =
  (* Add the consequences of the term [i] by using the term as an
     implication and searching for matching premises.
   *)
  assert (i < count pc);
  assert (is_nbenv_current i pc);
  let rd = rule_data i pc
  and nbenv = nbenv pc
  in
  assert (RD.is_implication rd);
  assert (not (RD.is_generic rd));
  let gp1,tps,nbenv_a,a = RD.schematic_premise rd in
  assert (nbenv_a = nbenv);
  if RD.is_schematic rd then (* the implication is schematic *)
    if RD.is_forward rd then begin
      let sublst = unify_with a gp1 tps pc.entry.prvd pc
      in
      let sublst = List.rev sublst in
      List.iter
        (fun (idx,args) ->
          if not (RD.is_intermediate (rule_data idx pc)) then
            add_consequence idx i args [||] pc
        )
        sublst
    end else
      ()
  else (* the implication is not schematic *)
    try
      let idx = find a pc in   (* check for exact match *)
      add_mp_fwd idx i pc
    with Not_found -> (* no exact match *)
      let sublst = unify a pc.entry.prvd pc
      in
      match sublst with
        [] -> ()
      | (idx,sub,ags)::_ ->
          (* the schematic rule [idx] matches the premise of [i]*)
          begin
            try
              let idx_premise = specialized idx sub ags 1 pc in
              add_mp_fwd idx_premise i pc
            with Not_found ->
              ()
          end



let add_fwd_evaluation (t:term) (i:int) (e:Eval.t) (full:bool) (pc:t): int =
  (* Add the term [t] which is an evaluation of the term [i] to the proof context
     if it is not yet in and return the index  *)
  try
    find t pc
  with Not_found ->
    let rd = get_rule_data t pc in
    Proof_table.add_eval t i e pc.base;
    let res = raw_add0 t rd full pc in ();
    if full then add_last_to_work pc;
    res



let add_consequences_evaluation (i:int) (pc:t): unit =
  (* Add the simplification and the evaluation of the term [i] in case that
     there is one if it is not yet in the proof context [pc] to the proof
     context and to the work items.
   *)
  let t = term i pc in
  let add_eval t e = add_fwd_evaluation t i e true pc
  in
  let t1,e,modi = simplified_term t i pc in
  if modi then
    ignore(add_eval t1 e);
  let t1,e,reeval_term,modi = evaluated_term t pc in
  let idx =
    if modi then
      add_eval t1 e
    else
      i
  in
  begin
    match reeval_term with
    | None ->
       ()
    | Some cond ->
       if is_tracing pc then
         begin
           printf "more evaluation would be possible of\n";
           printf "  %s\n" (string_of_term t pc);
           printf "if the following condition where decidable\n";
           printf "  %s\n" (string_of_term cond pc)
         end;
       let nbenv = count_variables pc in
       pc.entry.reeval <-
         Term_table.add cond 0 nbenv idx (seed_function pc) pc.entry.reeval;
       pc.entry.reeval <-
         let cond = negation_expanded cond pc in
         Term_table.add cond 0 nbenv idx (seed_function pc) pc.entry.reeval;
  end



let add_consequences_someelim (i:int) (pc:t): unit =
  try
    let some_consequence = Proof_table.someelim i pc.base in
    if has some_consequence pc then
      ()
    else begin
      Proof_table.add_someelim i some_consequence pc.base;
      let _ = raw_add some_consequence true pc in ();
      add_last_to_work pc
    end
  with Not_found ->
    ()



let type_of_term (t:term) (pc:t): type_term =
  Context.type_of_term t (context pc)

let predicate_of_type (tp:type_term) (pc:t): type_term =
  Context.predicate_of_type tp (context pc)



let add_assumption_or_axiom (t:term) (is_axiom: bool) (search:bool) (pc:t): int =
  (** Add the term [t] as an assumption or an axiom to the context [pc].
   *)
  assert (is_consistent pc);
  let cnt = count pc in
  if is_axiom then
    Proof_table.add_axiom t pc.base
  else
    Proof_table.add_assumption t pc.base;
  ignore(raw_add t search pc);
  if not is_axiom && search then
    add_last_to_work pc;
  cnt





let add_assumption (t:term) (search:bool) (pc:t): int =
  (** Add the term [t] as an assumption to the context [pc].
   *)
  add_assumption_or_axiom t false search pc



let add_axiom (t:term) (pc:t): int =
  (** Add the term [t] as an axiom to the context [pc].
   *)
  add_assumption_or_axiom t true false pc



let specialize_induction_law
      (idx:int)    (* induction law *)
      (p: term)    (* goal predicate *)
      (ivar: int)  (* induction variable *)
      (pc:t)
    : int =
  (* Specialize the induction law [idx] with the goal predicate [p].

     An induction law has the form

         all(p0,x) p01 ==> p02 ==> ... ==> x in p0

     where each p0i is a constructor rule.

     The specialized induction law has the form

         p1 ==> p2 ==> ... ivar in p

     where p0i and p0 are specialized with ivar and p
   *)
  let sub = [|p; Variable ivar|] in
  let ags =
    try
      RD.verify_specialization sub (context pc) (rule_data idx pc)
    with Not_found ->
      assert false
  in
  specialized idx sub ags 0 pc





let add_set_induction_law (set:term) (q:term) (elem:term) (pc:t): int =
  try
    let indlaw = Proof_table.set_induction_law set pc.base
    and pt     = Indset_ind set in
    Proof_table.add_proved_0 indlaw pt pc.base;
    let idx = raw_add indlaw false pc in
    let rd  = rule_data idx pc in
    let args = [|q;elem|]
    and ags  = [||]  (* is not generic *) in
    let rd  = RD.specialize rd args ags idx (context pc) in
    assert (RD.is_specialized rd);
    let t   = RD.term rd in
    Proof_table.add_specialize t idx args ags pc.base;
    raw_add0 t rd false pc
  with Not_found ->
    invalid_arg "Not an inductive set"


let add_inductive_set_rules (fwd:bool) (t:term) (pc:t): unit =
  match t with
    Application(set,args,_) ->
      assert (Array.length args = 1);
      begin try
        let nme,tp,rs =
          let indset = inductive_set set pc in
          match indset with
            Indset(nme,tp,rs) ->
              nme,tp,rs
          | _ -> assert false in
        let len = Array.length rs in
        for i = 0 to len-1 do
          let rule = Term.apply rs.(i) [|set|] in
          if has rule pc then begin
            ()
          end else begin
            let pt = Indset_rule (set,i) in
            Proof_table.add_proved_0 rule pt pc.base;
            ignore(raw_add rule true pc);
            add_last_to_work pc
          end
        done
      with Not_found ->
        ()
      end
  | _ ->
      ()



let equality_swapped (i:int) (left:term) (right:term) (pc:t): int =
  (* The term at [i] is a equality of the form [left = right]. Add the
     term [right = left] to the context and return its index.
   *)
  let eq_sym = equal_symmetry pc
  and tp = type_of_term left pc in
  let eq_sym = specialized eq_sym [|left;right|] [|tp|] 0 pc in
  add_mp i eq_sym false pc


let leibniz (i:int) (left:term) (right:term) (pc:t): int =
  (* The term at [i] is a equality of the form [left = right]. Add the
     term [all(p) p(left) ==> p(right)] derived from the leibniz condition

         all(a,b,p) a = b ==> p(a) ==> p(b)

     to the context and return its index.
   *)
  let idx = leibniz_term pc
  and tp = type_of_term left pc in
  let idx = specialized idx [|left;right|] [|tp|] 0 pc in
  add_mp i idx false pc



let can_be_variable_definition (v:int) (t:term) (pc:t): bool =
  not (variable_has_definition v pc) &&
  let nvars = count_variables pc in
  let used = Term.used_variables t nvars in
  not (List.mem v used) &&
  List.for_all
    (fun v -> not (variable_has_definition v pc))
    used


let variable_substitution_implication
    (v:int)
    (idx:int)
    (exp:term)
    (t:term)
    (search:bool)
    (pc:t)
    :term * int =
  (* Insert the term [t[v:=exp] ==> t] and return the term [t[v:=exp]] and
     the index of the implication.

     where [idx] is the assertion with the variable definition [v = exp].

     Raise [Not_found] if the symmetry law of equality is not available or if
     the leibniz law is not available.

     The term [t] must be a boolean term.
   *)
  let idx = equality_swapped idx (Variable v) exp pc in
  let pterm = Term.lambda_inner t v
  and tp = type_of_term (Variable v) pc in
  let ptp = predicate_of_type tp pc in
  let tps = Formals.make (standard_argnames 1) [|tp|] in
  let p = make_lambda tps Formals.empty [] pterm None pc in
  let redex1 = make_application p [|exp|] tp 0 pc
  and tr = beta_reduce 1 pterm ptp [|exp|] 0 pc
  and idx_leib = leibniz idx exp (Variable v) pc in
  let idx_leib = specialized idx_leib [|p|] [||] 0 pc in
  let pc0 = push_empty pc in
  let idx_a = add_assumption tr false pc0 in
  let idx_redex1 = add_beta_redex redex1 idx_a false pc0 in
  let idx_redex2 = add_mp idx_redex1 idx_leib false pc0 in
  let idx_t = add_beta_reduced idx_redex2 false pc0 in
  assert (Term.equivalent (term idx_t pc0) t);
  let imp,pt = Proof_table.discharged idx_t pc0.base in
  Proof_table.add_proved imp pt 0 pc.base;
  let res_idx = raw_add imp search pc in
  tr,res_idx


let substitute_variable
    (var:int) (def_idx:int) (def_term:term)
    (t:term) (idx:int) (search:bool) (pc:t): int =
  (* The variable [var] has the defining assertion [def_idx] with the
     definition term [def_term]. Use this definition to substitute the
     variable in the assertion [t] at [idx]
   *)
  if pc.trace then
    begin
      let prefix = trace_prefix pc in
      printf "%ssubstitute variable %s by %s\n"
             prefix
             (string_of_term (Variable var) pc)
             (string_of_term def_term pc);
      printf "%s  in %d %s\n"
             prefix
             idx
             (string_of_term t pc)
    end;
  let leib = leibniz def_idx (Variable var) def_term pc
                  (* leib: all(p:{T} p(var) ==> p(def_term) *)
  and pterm = Term.lambda_inner t var
  and tp = type_of_term (Variable var) pc
  in
  let tps = Formals.make (standard_argnames 1) [|tp|] in
  let p = make_lambda tps Formals.empty [] pterm None pc in
  let imp_idx = specialized leib [|p|] [||] 0 pc
      (* {var: term}(var) ==> {var:term}(def_term) *)
  and redex1 = make_application p [|Variable var|] tp 0 pc
  in
  let idx_redex1 = add_beta_redex redex1 idx false pc in
  let idx_redex2 = add_mp idx_redex1 imp_idx false pc in
  let res = add_beta_reduced idx_redex2 search pc in
  if pc.trace then
    printf "%ssubstituted term %s\n"
           (trace_prefix pc)
           (string_of_term_i res pc);
  res





let add_consequences_variable_definition (i:int) (pc:t): unit =
  (* If the assertion [i] is a variable definition of the form [v = exp] or
     [exp = v] where [exp] does not contain [v] then add all the consequences
     of the variable definition.

     - Use symmetry of [=] and swap the operands if necessary to get the
       expression into the form [v = exp] so that [v] has no definition and
       does not occur in [exp] and none of the variables in [exp] has a
       definition. If this is not possible then do nothing.

     - Add [exp] as a defintion of [v].

     - Scan all assumptions which contain [v] and add the assumptions with
       [v] substituted by [exp] to the context. Furthermore add the substituted
       assumptions to all variables occurring in [exp].

     - Scan all assertions where [v] has already been used in the substitution
       term of another variable. *)
  let var_def (i:int): int * int * term  =
    let eq_id, left, right = equality_data i pc in
    match left,right with
      Variable j, Variable k ->
        if can_be_variable_definition j right pc then
          i,j,right
        else if can_be_variable_definition k left pc then
          let i = equality_swapped i left right pc in
          i,k,left
        else
          raise Not_found
    | Variable j, _ ->
        if can_be_variable_definition j right pc then
          i, j, right
        else
          raise Not_found
    | _, Variable k ->
        if can_be_variable_definition k left pc then
          let i = equality_swapped i left right pc in
          i, k, left
        else
          raise Not_found
    | _,_ ->
        raise Not_found
  in
  try
    let idx, ivar, exp = var_def i in
    assert (not (variable_has_definition ivar pc));
    let ass_lst, _ = assumptions_for_variables_0 [|ivar|] pc in
    add_variable_definition ivar idx pc;
    let nvars = count_variables pc in
    let exp_used = Term.used_variables exp nvars in
    List.iter
      (fun ass_idx ->
        if ass_idx <> idx then
          let ass  = term ass_idx pc in
          let used = Term.used_variables ass nvars in
          if List.mem ivar used then begin
            try
              let idx_sub = substitute_variable ivar idx exp ass ass_idx true pc in
              (* add idx_sub to all variables in exp *)
              List.iter
                (fun v -> add_variable_usage v idx_sub pc)
                exp_used
            with Not_found ->
              ()
          end
      )
      ass_lst;
    List.iter
      (fun ass_idx ->
        ignore(substitute_variable ivar idx exp (term ass_idx pc) ass_idx true pc)
      )
      pc.var_defs.(ivar).used
  with Not_found ->
    ()



let expand_variable_definitions (i:int) (pc:t): unit =
  (* Check if the assertion at [i] has variables which have a definition. If yes
     then use these variable definitions and add the substituted assertion.*)
  let subst (i:int) (idx:int) (v:int) (exp:term) (search:bool): int =
    let t = term i pc in
    substitute_variable v idx exp t i search pc
  in
  let t = term i pc
  in
  let defs = get_variable_definitions t pc in
  match defs with
    [] ->
      ()
  | (idx,v,exp)::tail ->
      let i =
        List.fold_right
          (fun (idx,v,exp) i ->
            subst i idx v exp false
          )
          tail
          i
      in
      try
        ignore(subst i idx v exp true)
      with Not_found ->
        (* The leibniz law is not yet available *)
        ()




let add_consequences (i:int) (pc:t): unit =
  (** Add the consequences of the term [i] which are not yet in the proof
      context [pc] to the proof context and to the work items.
   *)
  (*printf "add_consequences %d %s\n" i (string_long_of_term_i i pc);*)
  let t  = term i pc
  and rd = rule_data i pc in
  add_inductive_set_rules true t pc;
  if not (RD.is_intermediate rd) then
    add_consequences_premise i pc;
  if RD.is_implication rd then
    add_consequences_implication i rd pc;
  add_consequences_evaluation i pc;
  add_consequences_someelim  i pc;
  if is_local_assumption i pc then
    expand_variable_definitions i pc;
  add_consequences_variable_definition i pc


let clear_work (pc:t): unit =
  pc.work <- []


let close_step (pc:t): unit =
  assert (has_work pc);
  let i = List.hd pc.work in
  pc.work <- List.tl pc.work;
  add_consequences i pc


let prefix (pc:t): string = String.make (2*(depth pc)+2) ' '


let close (pc:t): unit =
  if is_global pc then
    ()
  else begin
    let rec cls (round:int): unit =
      if has_work pc then begin
        let lst = List.rev pc.work in
        pc.work <- [];
        List.iter
          (fun i ->
            add_consequences i pc
          )
          lst;
        if is_interface_check pc then
          pc.work <- []
        else
          cls (1+round)
      end
    in
    cls 0
  end


let close_assumptions (pc:t): unit =
  (*pc.work <- List.rev pc.work;*)
  if pc.trace then
    printf "%sproof\n" (trace_prefix_0 pc);
  close pc



let trying_goal (g:term) (pc:t): unit =
  if pc.trace then begin
    let prefix = trace_prefix pc in
    printf "%strying to prove: %s\n"
      prefix (string_of_term g pc);
    if is_trace_extended pc then
      printf "%s\t%s\n" prefix (Term.to_string g);
  end


let failed_goal (g:term) (pc:t): unit =
  if pc.trace then
    printf "%sfailure: %s\n"
      (trace_prefix pc) (string_of_term g pc)


let proved_goal (g:term) (pc:t): unit =
  if pc.trace then
    printf "%ssuccess: %s\n"
      (trace_prefix pc) (string_of_term g pc)


let print_work (pc:t): unit =
  if has_work pc then begin
    printf "open work to close\n";
    List.iter
      (fun i -> printf "  %d %s\n" i (string_of_term_i i pc))
      pc.work
  end


let boolean_type (nb:int) (pc:t): type_term =
  let ntvs = Context.count_all_type_variables (context pc) in
  Variable (Constants.boolean_class + nb + ntvs)


let check_deferred (pc:t): unit = Context.check_deferred (context pc)

let owner (pc:t): int =
  (* The owner class of the signature *)
  assert (is_toplevel pc);
  let ct = class_table pc
  and tvs = tvars pc
  and s   = signature pc in
  match Class_table.dominant_class tvs s ct with
  | None ->
     -1
  | Some cls ->
     cls


let variant (i:int) (bcls:int) (cls:int) (pc:t): term =
  Proof_table.variant i bcls cls pc.base


let add_global (defer:bool) (anchor:int) (pc:t): unit =
  assert (is_global pc);
  let cnt = count pc in
  if cnt <> Seq.count pc.gseq + 1 then
    printf "add_global count pc = %d, Seq.count pc.gseq = %d\n"
      cnt (Seq.count pc.gseq);
  assert (cnt = Seq.count pc.gseq + 1);
  Seq.push
    {pub = is_public pc;
     defer = defer;
     anchor = anchor}
    pc.gseq;
  assert (count pc = Seq.count pc.gseq)






let eval_backward (tgt:term) (imp:term) (e:Eval.t) (pc:t): int =
  (* Add [imp] as an evaluation where [imp] has the form [teval ==> tgt] and
     [teval] is the term [tgt] evaluated with [e]. *)
  Proof_table.add_eval_backward tgt imp e pc.base;
  raw_add imp false pc



let predicate_of_term (t:term) (pc:t): type_term =
  Context.predicate_of_term t (context pc)


let find_equality (t1:term) (t2:term) (pc:t): int =
  let eq = Context.equality_term t1 t2 (context pc) in
  find_match eq pc


let find_leibniz_for (t1:term) (t2:term) (pc:t): int =
  let gen_leibniz = leibniz_term pc in
  let eq_idx = find_equality t1 t2 pc in
  let tp = Context.type_of_term t1 (context pc) in
  let spec_leibniz = specialized gen_leibniz [|t1;t2|] [|tp|] 0 pc in
  add_mp eq_idx spec_leibniz false pc


(* Subterm equality:

      The goal has the form             lhs  = rhs
      which we can transform into       f(a1,a2,..) = f(b1,b2,..)
      as a lambda term [f]
      and two argument arrays [a1,a2,..], [b1,b2,..]

      and we have found the leibniz rules  all(p) p(ai) ==> p(bi) for all
      arguments

   start:   f(a1,a2,..) = f(a1,a2,..)                        reflexivity

   step i:  f(a1,a2,..) = f(b1,b2,..,ai,ai+1,..)             start point

            {x: f(a1,a2,..) = f(b1,b2,..,x,ai+1,..)}(ai)     Eval_bwd

            {x:..}(ai) ==> {x:..}(bi)                        specialize leibniz

            {x:..}(bi)                                       modus ponens

            f(a1,a2,..) = f(b1,b2,..,bi,ai+1,..)             Eval

   last:    f(a1,a2,..) = f(b1,b2,..)

   result:  lhs = rhs                                        Eval

 *)
let prove_equality (g:term) (pc:t): int =
  let c = context pc in
  let eq_id, left, right, ags =
    match g with
      VAppl (eq_id, [|left;right|], ags, _)
      when Context.is_equality_index eq_id c ->
        eq_id, left, right, ags
    | _ ->
        raise Not_found
  in
  let find_leibniz t1 t2 = find_leibniz_for t1 t2 pc
  in
  let tlam, leibniz, args1, args2 =
    Term_algo.compare left right find_leibniz in
  let nargs = Array.length args1 in
  let tup  = Context.tuple_type_of_terms args1 c
  and tps  = Array.map (fun t -> Context.type_of_term t c) args1
  and r_tp = Context.type_of_term left c in
  let tps = Formals.make (standard_argnames nargs) tps in
  let lam = make_lambda tps Formals.empty [] tlam (Some r_tp) pc in
  assert (nargs = Array.length args2);
  assert (0 < nargs);
  let lam_1up = Term.up 1 lam
  and args1_up1 = Term.array_up 1 args1
  and args2_up1 = Term.array_up 1 args2 in
  try
    let flhs_1up = make_application lam_1up args1_up1 tup 1 pc
    and frhs_x i =
      let args =
        Array.init nargs
          (fun j ->
            if j < i then args2_up1.(j)
            else if j = i then Variable 0
            else args1_up1.(j)) in
      make_application lam_1up args tup 1 pc in
    let pred_inner i =
      VAppl (eq_id+1, [|flhs_1up; (frhs_x i)|], ags, false)
    in
    let start_term =
      let t = make_application lam args1 tup 0 pc in
      VAppl (eq_id, [|t;t|], ags, false)
    in
    let start_idx  = find_match start_term pc in
    let result = ref start_idx in
    for i = 0 to nargs - 1 do
      let pred_inner_i = pred_inner i
      and tp  = type_of_term args1.(i) pc in
      let tps = Formals.make [|ST.symbol "$1"|] [|tp|] in
      let pred_i = Lam(tps,Formals.empty,[],pred_inner_i,None) in
      let ai_abstracted =
        make_application pred_i [|args1.(i)|] tp 0 pc
      and ai_reduced = term !result pc in
      let imp = implication ai_reduced ai_abstracted pc in
      let idx2 =
        eval_backward ai_abstracted imp
          (Eval.Beta (Eval.Term ai_abstracted, Eval.Term ai_reduced)) pc in
      let idx = add_mp !result idx2 false pc in
      let sub = [|pred_i|] in
      let idx2 = specialized leibniz.(i) sub [||] 0 pc in
      let idx = add_mp idx idx2 false pc in
      let t = Term.apply pred_inner_i [|args2.(i)|] in
      let e = Eval.Beta (Eval.Term (term idx pc), Eval.Term t) in
      Proof_table.add_eval t idx e pc.base;
      result := raw_add t false pc
    done;
    let e =
      let ev args t =
        Eval.Beta (Eval.Term (make_application lam args tup 0 pc),
                   Eval.Term t)
      in
      Eval.VApply(eq_id, [|ev args1 left; ev args2 right|], ags)
    in
    result := add_fwd_evaluation g !result e false pc;
    !result
  with Not_found ->
    assert false (* cannot happen *)



let backward_witness (t:term) (pc:t): int =
  (* Find a witness for the existentially quantified term [t] or raise [Not_found]
     if there is no witness or [t] is not existentially quantified.
   *)
  let tps,t0 = Term.some_quantifier_split t in
  let nargs = Formals.count tps
  and nms = Formals.names tps
  and tps = Formals.types tps in
  let sublst  = unify_with t0 nargs tps pc.entry.prvd pc in
  let idx,args = List.find (fun (idx,args) -> Array.length args = nargs) sublst
  in
  let witness = term idx pc in
  let impl    = implication witness t pc in
  Proof_table.add_witness impl idx nms tps t0 args pc.base;
  let idx_impl = raw_add impl false pc in
  add_mp0 t idx idx_impl false pc



let find_goal (g:term) (pc:t): int =
  (* Find either an exact match of the goal or a schematic assertion which can
     be fully specialized to match the goal. *)
  add_inductive_set_rules false g pc;
  close pc;
  try
    find_match g pc
  with Not_found ->
    try backward_witness g pc
    with Not_found ->
      prove_equality g pc




module Backward =
  struct
    type rule = {
        idx: int;
        tps: Formals.t;
        fgs: Formals.t;
        mutable ps: term list;          (* remaining premises *)
        mutable subs: Term_sub.t list;  (* set of substitutions *)
        pc: t
      }

    let has_some (r:rule): bool =
      r.subs <> []

    let is_complete (r:rule): bool =
      match r.subs with
      | [] ->
         true
      | sub :: _ ->
         Term_sub.count sub = Formals.count r.tps
                                  (* all the others must have the same variables
                                     substituted, because the same subterms have
                                     been used to find them *)


    let merge
          (sub:Term_sub.t) (old_subs:Term_sub.t list) (cumulated:Term_sub.t list)
        : Term_sub.t list =
      (* Merge the new substitution [sub] with each substitutions of [old_subs]
         and prepend the merged substitutions in front of [cumulated]. *)
      List.fold_left
        (fun cumulated old_sub ->
          try
            (Term_sub.merge sub old_sub) :: cumulated
          with Not_found ->
            cumulated
        )
        cumulated
        old_subs


    let rec complete_rule (r:rule): rule =
      (* Returns a rule with a nonempty list of complete substitutions or
         raises Not_found, if not possible.*)
      assert (has_some r);
      if is_complete r then
        r
      else
        match r.ps with
        | [] ->
           assert false (* As long as the substitutions are not yet complete there
                           must be premises *)
        | p :: ps when Term.is_variable_below (Formals.count r.tps) p ->
           printf "\n\nRule %s\nhas a premise which is catch all\n\n"
                  (string_of_term_i r.idx r.pc);
           r.ps <- ps;
           complete_rule r
        | p :: ps ->
           r.ps <- ps;
           let sublst =
             unify_with_0 p (Formals.count r.tps) r.pc.entry.prvd r.pc in
           r.subs <-
             List.fold_left
               (fun subs (idx,new_sub) ->
                 let new_sub =
                   Term_sub.map
                     (fun t -> transformed_to_current t idx r.pc)
                     new_sub
                 in
                 merge new_sub r.subs subs
               )
               []
               sublst;
           if r.subs = [] then
             raise Not_found; (* The rule cannot be completed *)
           complete_rule r

    let find_rules (g:term) (blacklst:IntSet.t) (pc:t): rule list =
      List.fold_left
        (fun lst (idx,sub) ->
          if IntSet.mem idx blacklst || not (is_visible idx pc) then
            lst
          else
            let tps,fgs,ps_rev,tgt =
              split_general_implication_chain (term idx pc) pc
            in
            assert (ps_rev <> []); (* has to be a backward rule
                                      i.e. an implication *)
            let ps = List.rev ps_rev in
            try
              {tps; fgs; idx; ps; subs = [sub]; pc} :: lst
            with Not_found ->
              lst
        )
        []
        (unify_0 g pc.entry.bwd pc)

    let specialize_rule (r:rule) (lst:int list): int list =
      (* Specialized the rule [r] for all its substitutions and append
         the successfully specialized rules to the list [lst]. *)
      assert (RD.is_backward (rule_data r.idx r.pc));
      List.fold_left
        (fun lst sub ->
          try
            let args,ags = args_and_ags_of_substitution r.idx sub r.pc in
            if Array.length args = 0 then
              r.idx :: lst
            else
              let cnt = count r.pc in
              let idx = specialized r.idx args ags 2 r.pc in
              if idx = cnt then (* no duplicate *)
                cnt :: lst
              else
                lst
          with Not_found ->
            lst
        )
        lst
        r.subs

    let complete_rules (rules:rule list): int list =
      List.fold_left
        (fun lst r ->
          try
            let r = complete_rule r in
            specialize_rule r lst
          with Not_found ->
            lst
        )
        []
        rules


    let find (g:term) (blacklst:IntSet.t) (pc:t): int list =
      let rules = find_rules g blacklst pc in
      let lst = complete_rules rules in
      List.sort
        (fun i j ->
          let rdi = rule_data i pc
          and rdj = rule_data j pc in
          compare (RD.count_premises rdi) (RD.count_premises rdj))
        lst
  end (* Backward *)


(*
let backward_in_table (g:term) (blacklst: IntSet.t) (pc:t): int list =
  let sublst = unify g pc.entry.bwd pc in
  let lst =
    List.fold_left
      (fun lst (idx,sub,ags) ->
        if IntSet.mem idx blacklst || not (is_visible idx pc) then
          lst
        else if Array.length sub = 0 then
          if RD.is_backward (rule_data idx pc) then
            idx :: lst
          else
            lst
        else begin
          let cnt = count pc in
          let idx = specialized idx sub ags 2 pc in
          if idx = cnt && RD.is_backward (rule_data idx pc) then begin
            cnt :: lst
          end else begin
            lst
          end
        end)
      []
      sublst
  in
  List.sort
    (fun i j ->
      let rdi = rule_data i pc
      and rdj = rule_data j pc in
      compare (RD.count_premises rdi) (RD.count_premises rdj))
    lst
 *)

let eval_reduce (g:term) (lst:int list) (pc:t): int list =
  let add_eval t e lst =
    let impl = implication t g pc in
    if has impl pc then
      lst
    else
      (eval_backward g impl e pc) :: lst
  in
  let t1,e,modi = simplified_term g (count pc) pc in
  let lst = if modi then add_eval t1 e lst else lst in
  let t1,e,_,modi = evaluated_term g pc in
  if modi then add_eval t1 e lst else lst


let substituted_goal (g:term) (lst:int list) (pc:t): int list =
  (* Apply all variable substitutions in backward direction and add
     [gsub ==> g] to the context and add its index to the list *)
  try
    let gsub,imps =
      List.fold_left
        (fun (gsub,imps) (idx,v,exp) ->
          let gsub,imp =
            variable_substitution_implication v idx exp gsub false pc in
          gsub, imp::imps
        )
        (g,[])
        (get_variable_definitions g pc)
        (*(List.rev (get_variable_definitions g pc))*)
    in
    if imps = [] then
      lst
    else
      let pc0 = push_empty pc in
      let idx_gsub = add_assumption gsub false pc0 in
      let idx_g =
        List.fold_left
          (fun g imp -> add_mp g imp false pc0)
          idx_gsub
          imps
      in
      let imp,pt = Proof_table.discharged idx_g pc0.base in
      Proof_table.add_proved imp pt 0 pc.base;
      let imp_idx = raw_add imp false pc in
      if pc.trace then
        begin
          let prefix = trace_prefix pc in
          printf "%sgoal substitution backward rule\n" prefix;
          printf "%s   %d %s\n" prefix imp_idx (string_of_term_i imp_idx pc)
        end;
      imp_idx :: lst
  with Not_found ->
    lst



let find_backward_goal (g:term) (blacklst:IntSet.t) (pc:t): int list =
  (*assert (is_well_typed g pc);*)
  let lst = substituted_goal g [] pc in
  if lst <> [] then
    lst
  else begin
    let lst = Backward.find g blacklst pc in
    (*let lst = backward_in_table g blacklst pc in*)
    let lst = eval_reduce g lst pc in
    if pc.trace && is_trace_extended pc then begin
      let prefix = trace_prefix pc
      and str = string_of_intlist lst in
      printf "%salternatives %s\n" prefix str;
      if not (IntSet.is_empty blacklst) then
        printf "%s   blacklist %s\n" prefix (string_of_intset blacklst) end;
    lst
  end


let discharged (i:int) (pc:t): term * proof_term =
  (** The [i]th term of the current environment with all local variables and
      assumptions discharged together with its proof term.
   *)
  Proof_table.discharged i pc.base


let discharged_bubbled (i:int) (pc:t): term * proof_term =
  (** The [i]th term of the current environment with all local variables and
      assumptions discharged together with its proof term.
   *)
  Proof_table.discharged_bubbled i pc.base



let is_proof_pair (t:term) (pt:proof_term) (pc:t): bool =
  Proof_table.is_proof_pair t pt pc.base


let add_proved_term (t:term) (pt:proof_term) (search:bool) (pc:t): int =
  (* Add the proof pair [t,pt] to the table and include it into the search
     tables if [search] is flagged and the term [t] is not a duplicate.

     Note: Not allowed as a global assertion! *)
  assert (not (is_global pc));
  let cnt = count pc in
  Proof_table.add_proved t pt 0 pc.base;
  let idx = raw_add t search pc in
  if search && idx = cnt then
    add_last_to_work pc;
  idx


let add_proved_0
    (defer:bool) (anchor:int)
    (t:term) (pterm:proof_term) (delta:int) (pc:t)
    : int =
  assert (not defer || anchor <> 0);
  let cnt = count pc
  and ct = class_table pc
  in
  Proof_table.add_proved t pterm delta pc.base;
  let idx = raw_add t true pc in
  let dup = idx < cnt
  and is_glob = idx < count_global pc
  in
  if not dup || is_glob then (* duplicates of globals must be added to work,
                                because globals are not closed *)
    add_last_to_work pc;
  if defer && is_interface_check pc then begin
    assert (is_global pc);
    add_to_public_deferred t cnt pc
  end;
  if is_global pc then begin
    add_global defer anchor pc;
    if defer && not dup then begin
      assert (idx = cnt);
      Class_table.add_generic idx true anchor ct;
      let gdesc = Seq.elem idx pc.gseq in
      if is_interface_check pc && not gdesc.pub then
        gdesc.pub <- true
    end else if dup && is_interface_check pc
    then
      (Seq.elem idx pc.gseq).pub <- true
  end;
  cnt

let add_proved_with_delta
      (t:term)
      (pterm:proof_term)
      (delta: int)
      (pc:t)
    : int =
  add_proved_0 false (-1) t pterm delta pc


let add_proved
    (t:term)
    (pterm:proof_term)
    (pc:t)
    : int =
  add_proved_0 false (-1) t pterm 0 pc




let add_proved_list
    (defer:bool)
    (anchor:int)
    (lst: (term*proof_term) list)
    (pc:t)
    : unit =
  let cnt = count pc in
  List.iter
    (fun (t,pt) ->
      let delta = count pc - cnt in
      let _ = add_proved_0 defer anchor t pt delta pc in ())
    lst


let remove_or_remap (set:IntSet.t) (pc:t): unit =
  (* All assertions in the set [set] have some features which have got a new
     seed.

     If an assertion can still be found then there is a more general assertion in
     the search tables and the assertion has to be removed from the search tables.

     If an assertion cannot be found anymore then the search tables have to be
     remapped to find them again. Furthermore an assertion which has to be remapped
     might be an equality assertion. If yes, its left hand side has to be added to
     the equality table.
   *)
  let index i =
    try find (term i pc) pc
    with Not_found -> -1
  in
  let remap_set =
    IntSet.filter
      (fun i ->
        let idx = index i in
        assert (idx <> i); (* Some features have new seeds *)
        idx = -1)
      set
  in
  let pred i = IntSet.mem i remap_set || not (IntSet.mem i set)
  in
  filter_and_remap_tables pred pc;
  IntSet.iter
    (fun i ->
      let t = term i pc in
      add_to_equalities t i pc)
    remap_set


let add_induction_law0 (cls:int) (pc:t): unit =
  assert (is_global pc);
  let law = Proof_table.type_induction_law cls pc.base
  and pt  = Indtype cls
  and defer = Class_table.is_deferred cls (class_table pc)
  in
  ignore(add_proved_0 defer cls law pt 0 pc)



let previous_schematics (idx:int) (pc:t): int list =
  assert (idx < count pc);
  let rd = rule_data idx pc in
  RD.previous_schematics rd


let premises (idx:int) (pc:t): (term*bool) list =
  assert (idx < count pc);
  let rd    = rule_data idx pc
  in
  assert (RD.is_fully_specialized rd);
  assert (RD.is_implication rd);
  RD.premises rd (context pc)



let check_interface (pc:t): unit =
  assert (is_interface_check pc);
  let ft = feature_table pc in
  Feature_table.check_interface ft;
  assert (count pc = Seq.count pc.gseq);
  for i = 0 to count pc - 1 do
    let gdesc = Seq.elem i pc.gseq in
    if gdesc.defer
        && not gdesc.pub
        && Class_table.is_class_public gdesc.anchor (class_table pc)
        && not (has_public_deferred (term i pc) pc)
    then
      begin
        let open Module in
        let m = Compile.current (Feature_table.compilation_context ft) in
        assert (M.has_interface m);
        let fn = Src.path (M.interface m) in
        error_info
          (FINFO (1,0,fn))
          ("deferred assertion \"" ^ (string_of_term (term i pc) pc) ^
             "\" is not public")
      end
  done

let excluded_middle (pc:t): int =
  let nvars = nbenv pc in
  let or_id  = 1 + nvars + Constants.or_index
  and not_id = 1 + nvars + Constants.not_index in
  let em = Term.binary or_id (Variable 0) (Term.unary not_id (Variable 0)) in
  let nms = standard_argnames 1
  and tps = [| boolean_type 1 pc |] in
  let em = Term.all_quantified (Formals.make nms tps) Formals.empty em in
  find em pc


let indirect_proof_law (pc:t): int =
  let nvars = nbenv pc in
  let not_id   = 1 + nvars + Constants.not_index
  and imp_id   = 1 + nvars + Constants.implication_index
  and false_const = Feature_table.false_constant (1 + nvars)
  in
  (* all(a) (not a ==> false) ==> a *)
  let nota = Term.unary not_id (Variable 0) in
  let nota_imp_false = Term.binary imp_id nota false_const in
  let t = Term.binary imp_id nota_imp_false false_const in
  let btp = boolean_type 1 pc in
  let nms = standard_argnames 1
  and tps = [| btp |] in
  let indirect = Term.all_quantified (Formals.make nms tps) Formals.empty t in
  find indirect pc


let or_elimination (pc:t): int =
  let nvars = nbenv pc in
  let or_id  = 3 + nvars + Constants.or_index
  and imp_id = 3 + nvars + Constants.implication_index in
  let a_or_b = Term.binary or_id (Variable 0) (Variable 1)
  and a_imp_c = Term.binary imp_id (Variable 0) (Variable 2)
  and b_imp_c = Term.binary imp_id (Variable 1) (Variable 2) in
  let or_elim = Term.binary imp_id
      a_or_b
      (Term.binary imp_id
         a_imp_c
         (Term.binary imp_id
            b_imp_c
            (Variable 2))) in
  let btp = boolean_type 3 pc in
  let nms = standard_argnames 3
  and tps = [| btp; btp; btp |] in
  let or_elim = Term.all_quantified (Formals.make nms tps) Formals.empty or_elim in
  find or_elim pc


let has_excluded_middle (pc:t): bool =
  try
    ignore(excluded_middle pc); true
  with Not_found ->
    false


let has_or_elimination (pc:t): bool =
  try
    ignore(or_elimination pc); true
  with Not_found ->
    false
