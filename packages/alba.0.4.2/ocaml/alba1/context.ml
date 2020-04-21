(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
 *)

open Container
open Signature
open Term
open Support
open Printf

module Option = Fmlib.Option

type formal = Class_table.formal

type entry = {
    tvs:          Tvars.t;            (* cumulated *)
    fargs:        Formals.t;          (* cumulated *)
    ntvs_delta:   int;
    nfgs_delta:   int;
    nargs_delta:  int;
    rvar:         bool;
    result:       Result_type.t;
    info:         info;
  }


type t = {
    entry: entry;
    prev:  t option;
    depth: int;
    ft:            Feature_table.t;
    verbosity:     int
  }


let empty_entry: entry =
  {tvs          = Tvars.empty;
   fargs        = Formals.empty;
   ntvs_delta   = 0;
   nfgs_delta   = 0;
   nargs_delta  = 0;
   rvar         = false;
   result   = Result_type.empty;
   info      = UNKNOWN}



let class_table(c:t): Class_table.t     = Feature_table.class_table c.ft
let feature_table(c:t): Feature_table.t = c.ft



let add_used_module (m:Module.M.t) (c:t): unit =
  Feature_table.add_used_module m c.ft

let add_current_module (m:Module.M.t) (c:t): unit =
  Feature_table.add_current_module m c.ft

let set_interface_check (c:t): unit =
  Feature_table.set_interface_check c.ft


let is_private (c:t): bool = Feature_table.is_private c.ft
let is_public  (c:t): bool = Feature_table.is_public  c.ft
let is_interface_check (c:t): bool = Feature_table.is_interface_check  c.ft

let is_interface_use (c:t): bool = Feature_table.is_interface_use c.ft


let is_global (c:t): bool =
  c.prev = None


let is_local (c:t): bool =
  c.prev <> None

let is_toplevel (c:t): bool =
  match c.prev with
    Some prev -> is_global prev
  | _   -> false


let previous (c:t): t =
  assert (not (is_global c));
  Option.value c.prev


let rec is_outer (c_outer:t) (c:t): bool =
  if c_outer == c then
    true
  else
    match c.prev with
      None ->
        false
    | Some c ->
        is_outer c_outer c



let entry_arity (e:entry): int = e.nargs_delta

let arity     (c:t): int = entry_arity c.entry

let verbosity (c:t): int = c.verbosity

let info (c:t): info = c.entry.info

let has_result_variable (c:t): bool = c.entry.rvar

let has_result (c:t): bool = Result_type.has_result c.entry.result

let result_type (c:t): type_term =
  (* The result type of the context *)
  assert (has_result c);
  Result_type.result c.entry.result


let count_type_variables (c:t): int =
  (** The number of cumulated type variables in this context and all
      preceeding contexts
   *)
  Tvars.count c.entry.tvs

let has_type_variables (c:t): bool =
  count_type_variables c > 0

let has_no_type_variables (c:t): bool =
  count_type_variables c = 0


let count_local_type_variables (c:t): int =
  c.entry.ntvs_delta


let entry_nfgs (e:entry): int = Tvars.count_fgs e.tvs

let count_formal_generics (c:t): int =
  (** The cumulated number of formal generics in this context and all
      previous contexts
   *)
  entry_nfgs c.entry


let count_last_formal_generics (c:t): int =
  (* The number of  formal generics introduced in this context.
   *)
  c.entry.nfgs_delta

let count_last_type_variables (c:t): int =
  c.entry.ntvs_delta

let count_last_arguments (c:t): int = c.entry.nargs_delta

let count_last_variables (c:t): int =
  c.entry.nargs_delta +
  if has_result c then 1 else 0

let count_variables (c:t): int = Formals.count c.entry.fargs


let implication_index (c:t): int =
  count_variables c + Constants.implication_index

let and_index (c:t): int =
  count_variables c + Constants.and_index

let or_index (c:t): int =
  count_variables c + Constants.or_index

let not_index (c:t): int =
  count_variables c + Constants.not_index

let domain_index (c:t): int =
  count_variables c + Constants.domain_index

let tuple_index (c:t): int =
  count_variables c + Constants.tuple_index



let is_equality_index (i:int) (c:t): bool =
  Feature_table.is_equality_index (i - count_variables c) c.ft


let variable_name (i:int) (c:t): int =
  assert (i < count_variables c);
  (Formals.names c.entry.fargs).(i)


let variable_type (i:int) (c:t): type_term =
  assert (i < count_variables c);
  (Formals.types c.entry.fargs).(i)

let count_all_type_variables (c:t): int =
  (** The cumulated number of formal generics and type variables in
      this context and all previous contexts
   *)
  (count_formal_generics c) + (count_type_variables c)


let function_class (c:t): int =
  count_all_type_variables c + Constants.function_class

let predicate_class (c:t): int =
  count_all_type_variables c + Constants.predicate_class

let tuple_class (c:t): int =
  count_all_type_variables c + Constants.tuple_index


let function_type (a_tp: type_term) (r_tp: type_term) (c:t): type_term =
  make_type (function_class c) [|a_tp;r_tp|]

let predicate_type (a_tp: type_term) (c:t): type_term =
  make_type (predicate_class c) [|a_tp|]

let entry_local_argnames (e:entry): names =
  Array.sub (Formals.names e.fargs) 0 e.nargs_delta

let entry_local_types (e:entry): term array =
  Array.sub (Formals.types e.fargs) 0 e.nargs_delta

let entry_argnames (e:entry): names =
  Formals.names e.fargs

let entry_argtypes (e:entry): types =
  Formals.types e.fargs


let local_argnames (c:t): names = entry_local_argnames c.entry

let local_varnames (c:t): names =
  let nvars = count_last_variables c in
  Array.sub (Formals.names c.entry.fargs) 0 nvars

let local_vartypes (c:t): types =
  let nvars = count_last_variables c in
  Array.sub (Formals.types c.entry.fargs) 0 nvars

let local_argtypes (c:t): term array = entry_local_types c.entry

let local_formals (c:t): formals0 =
  entry_local_argnames c.entry,
  entry_local_types c.entry

let local_fgs (c:t): formals0 =
  let tvs  = c.entry.tvs in
  let nfgs = c.entry.nfgs_delta in
  let fgnms = Array.sub (Tvars.fgnames tvs) 0 nfgs
  and fgcon = Array.sub (Tvars.fgconcepts tvs) 0 nfgs in
  fgnms,fgcon

let argnames (c:t): names =
  entry_argnames c.entry

let argtypes (c:t): types =
  entry_argtypes c.entry


let local_type_reduced (i:int) (c:t): type_term =
  (* Requires that the type of argument i does not contain local type variables *)
  assert (i < count_last_arguments c);
  let ntvs = count_last_type_variables c in
  let tp = variable_type i c in
  try
    Term.down ntvs tp
  with
    Term_capture ->
      assert false (* precondition violated *)



let local_types_reduced (c:t): types =
  let nargs = count_last_arguments c in
  Array.init nargs (fun i -> local_type_reduced i c)



let entry_varnames (e:entry): int array =
  Formals.names e.fargs


let varnames (c:t): int array = entry_varnames c.entry



let entry_fgnames (e:entry): int array = Tvars.fgnames e.tvs

let entry_fgconcepts (e:entry): type_term array = Tvars.fgconcepts e.tvs

let fgnames (c:t): int array = entry_fgnames c.entry

let fgconcepts (c:t): type_term array = entry_fgconcepts c.entry

let tvars (c:t): Tvars.t = c.entry.tvs

let variable_class (i:int) (c:t): int =
  Tvars.principal_class (variable_type i c) (tvars c)


let string_of_signature (s:Sign.t) (c:t): string =
  Class_table.string_of_signature
    s
    (tvars c)
    (class_table c)


let string_of_term0 (t:term) (norm:bool) (long:bool) (nanon:int) (c:t): string =
  let tvs = tvars c in
  Feature_table.term_to_string t norm long nanon (varnames c) tvs c.ft

let string_of_term (t:term) (c:t): string =
  string_of_term0 t true false 0 c

let string_long_of_term (t:term) (c:t): string =
  string_of_term0 t true true 0 c


let string_of_term_anon (t:term) (nb:int) (c:t): string =
  string_of_term0 t true false nb c


let string_long_of_term_anon (t:term) (nb:int) (c:t): string =
  string_of_term0 t true true nb c


let string_of_term_array (sep:string) (arr: term array) (c:t): string =
  String.concat sep
    (List.map (fun t -> string_of_term t c) (Array.to_list arr))

let string_of_arguments (arr: term array) (c:t): string =
  "(" ^ (string_of_term_array "," arr c) ^ ")"


let string_of_type (tp:type_term) (c:t): string =
  Class_table.string_of_type tp (tvars c) (class_table c)


let string_of_type_array (sep:string) (tps:types) (c:t): string =
  String.concat sep
    (List.map (fun tp -> string_of_type tp c) (Array.to_list tps))

let string_of_ags (ags:agens) (c:t): string =
  "["
  ^ (string_of_type_array "," ags c)
  ^ "]"



let string_of_variables (c:t): string =
  let nms = Formals.names c.entry.fargs
  and tps = Formals.types c.entry.fargs
  in
  let str =
    String.concat
      ","
      (List.rev
         (interval_fold
            (fun lst i ->
              (ST.string nms.(i) ^ ":" ^ string_of_type tps.(i) c) :: lst
            )
            [] 0 (Array.length nms)
         )
      )
  in
  if str = "" then
    ""
  else
    "(" ^ str ^ ")"


let string_of_tvs (c:t): string =
  Class_table.string_of_tvs (tvars c) (class_table c)

let is_constructor (i:int) (c:t): bool =
  Feature_table.is_constructor (i - count_variables c) c.ft

let is_pseudo_constructor (i:int) (c:t): bool =
  Feature_table.is_pseudo_constructor (i - count_variables c) c.ft

let constructor_preconditions
      (co:int) (args:arguments) (ags:agens) (c:t)
    : term list =
  let nvars = count_variables c
  and nargs = Array.length args in
  assert (nvars <= co);
  List.map
    (fun pre ->
      Feature_table.substituted pre nargs 0 0 args nvars ags (tvars c) c.ft)
    (Feature_table.constructor_preconditions (co - nvars) c.ft)


let make_application
    (f:term) (args:term array) (tup:type_term) (nb:int)(c:t)
    : term =
  let nbenv = count_variables c in
  let res = Feature_table.make_application f args tup (nb+nbenv) c.ft in
  res


let beta_reduce
    (n:int) (tlam:term) (tup_tp:type_term) (args:term array) (nb:int )(c:t)
    : term =
  Feature_table.beta_reduce
    n tlam tup_tp args
    (nb+count_variables c)
    (count_all_type_variables c)
    c.ft


let quantified (is_all:bool) (tps:formals) (fgs:formals) (t:term) (c:t)
    : term =
  Term.quantified is_all tps fgs t

let all_quantified (tps:formals) (fgs:formals) (t:term) (c:t): term =
  quantified true tps fgs t c

let some_quantified (tps:formals) (fgs:formals) (t:term) (c:t): term =
  quantified false tps fgs t c


let prenex_term (t:term) (c:t): term =
  Term.prenex
    t
    (count_variables c)
    (count_all_type_variables c)
    Constants.implication_index

let prenex_sort_term (t:term) (c:t): term =
  Term.prenex_sort
    t
    (count_variables c)
    (count_all_type_variables c)
    Constants.implication_index

let prenex_term_bubble_one (t:term) (c:t): term =
  Term.prenex_bubble_one
    t
    (count_variables c)
    (count_all_type_variables c)
    Constants.implication_index



let entry_signature (e:entry) (c:t): Sign.t =
  (** The signature of the entry [e] in the context [c].  *)
  let argtypes = Array.sub (Formals.types e.fargs) 0 (entry_arity e) in
  Sign.make argtypes e.result




let signature (c:t): Sign.t =
  (** The signature of the context [c].  *)
  entry_signature c.entry c


let signature_string (c:t): string =
  (** Print the signature of the context [c].  *)
  string_of_signature (signature c) c



let feature_signature (fidx:int) (c:t): Tvars.t * Sign.t =
  let nvars = count_variables c in
  assert (nvars <= fidx);
  Feature_table.signature0 (fidx-nvars) c.ft


let string_of_feature_signature (fidx:int) (c:t): string =
  let nvars = count_variables c in
  assert (nvars <= fidx);
  Feature_table.string_of_signature (fidx-nvars) c.ft



let variable_index (nme:int) (c:t): int =
  Search.array_find_min (fun n -> n = nme) (Formals.names c.entry.fargs)




let split_equality (t:term) (nb:int) (c:t): int * int * term * term =
  Feature_table.split_equality t (nb + count_variables c) c.ft


let check_deferred (c:t): unit =
  assert (is_toplevel c);
  let ct  = class_table c
  and tvs = c.entry.tvs
  and s   = signature c
  in
  let dominant_cls = Class_table.dominant_class tvs s ct in
  let dominant_fg  = Class_table.dominant_formal_generic tvs dominant_cls ct
  in
  Class_table.check_deferred dominant_cls dominant_fg c.entry.info ct


let split_general_implication_chain
    (t:term) (c:t): Formals.t * Formals.t * term list * term =
  let nvars = count_variables c in
  let imp_id = nvars + Constants.implication_index in
  Term.split_general_implication_chain t imp_id




let depth (c:t): int = c.depth

let rec ith_entry (i:int) (c:t): entry =
  assert (i <= depth c);
  if i = 0 then c.entry
  else ith_entry (i-1) (previous c)

let is_untyped (i:int) (c:t): bool =
  (* Is the variable [i] untyped? *)
  assert (i < count_variables c);
  let tp = variable_type i c in
  match tp with
    Variable j when j < count_type_variables c -> true
  | _ -> false



let variable_data (i:int) (c:t): Tvars.t * Sign.t =
  let nvars = count_variables c in
  if i < nvars then
    c.entry.tvs,
    Sign.make_const (variable_type i c)
  else
    let idx = i - nvars in
    let tvs,s = Feature_table.signature0 idx (feature_table c) in
    Tvars.fgs_to_global tvs, s


let variable (name:int) (c:t): int * Tvars.t * Sign.t =
  (** The term and the signature of the argument named [name] *)
  let i = variable_index name c in
  let tvs,s = variable_data i c in
  i,tvs,s


let make (comp:Module.Compile.t): t =
  {entry = empty_entry;
   prev  = None;
   depth = 0;
   ft    = Feature_table.base_table comp;
   verbosity = Module.Compile.verbosity comp
 }


let push
    (entlst: entities list withinfo)
    (rt: return_type)
    (is_pred: bool)
    (is_func: bool)
    (rvar:    bool)
    (c: t)
    : t =
  (** Push the new type variables, formal generics and the formal arguments of
      [entlst,rt] to the context [c]. *)
  assert (not (is_pred && is_func));
  let entry      = c.entry
  and ct         = class_table c in
  let tvs  =
    Class_table.formal_generics entlst rt is_func entry.tvs ct in
  let ntvs0 = Tvars.count_local entry.tvs
  and nfgs0 = Tvars.count_fgs entry.tvs
  in
  let ntvs1 = Tvars.count_local tvs - ntvs0
  and nfgs1 = Tvars.count_fgs tvs   - nfgs0
  in
  let fargs1, res =
    Class_table.analyze_signature entlst rt is_pred is_func rvar tvs ct in
  let nms1,tps1 = Myarray.split fargs1 in
  let fargs =
    Formals.make
      (Term.prepend_names nms1 (Formals.names entry.fargs))
      (Array.append
         tps1
         (Array.map
            (fun tp -> Term.up ntvs1 (Term.up_from nfgs1 ntvs0 tp))
            (Formals.types entry.fargs)))
  and nargs_delta = Array.length fargs1 -
    if rvar then 1 else 0 (*variables*)
  in
  assert (0 <= nargs_delta);
  {c with
   entry =
   {tvs          = tvs;
    fargs        = fargs;
    ntvs_delta   = ntvs1;
    nfgs_delta   = nfgs1;
    nargs_delta  = nargs_delta;
    rvar         = Result_type.has_result res;
    result       = res;
    info         = entlst.i};
   prev  = Some c;
   depth = 1 + c.depth}




let push_untyped (names:int array) (c:t): t =
  let entlst = withinfo UNKNOWN [Untyped_entities (Array.to_list names)] in
  push entlst None false false false c


let push_typed
      (tps:Formals.t) (fgs:Formals.t) (rvar:bool) (c:t)
    : t =
  assert (count_type_variables c = 0 );
  let nfgs_new  = Formals.count fgs
  and nargs_new = Formals.count tps in
  assert (not rvar || nargs_new > 0);
  let tvs  = Tvars.push_fgs fgs c.entry.tvs
  in
  let fargs =
    Formals.prepend
      tps
      (Formals.map (Term.up nfgs_new) c.entry.fargs)
  in
  {c with
   entry =
   {tvs     = tvs;
    fargs   = fargs;
    ntvs_delta  = 0;
    nfgs_delta  = nfgs_new;
    nargs_delta = nargs_new - if rvar then 1 else 0;
    rvar    = rvar;
    result  = Result_type.empty;
    info    = UNKNOWN};
   prev = Some c;
   depth = 1 + c.depth}



let push_typed0
      (tps:Formals.t) (fgs:Formals.t) (c:t)
    : t =
  push_typed tps fgs false c

let push_empty (c:t): t =
  push_typed Formals.empty Formals.empty false c


let context_of_feature (idx:int) (c:t): t =
  assert (is_global c);
  let tvs,sign = Feature_table.signature0 idx c.ft
  and nms = Feature_table.argument_names idx c.ft in
  push_typed0
    (Formals.make nms (Sign.arguments sign))
    (Formals.make (Tvars.fgnames tvs) (Tvars.fgconcepts tvs))
    c



let extract_from_tuple (n:int) (tp:type_term) (c:t): types =
  Class_table.extract_from_tuple n (count_all_type_variables c) tp


let push_lambda (n:int) (nms:names) (tp:type_term) (c:t): t =
  assert (0 < n);
  assert (n = Array.length nms);
  let cls,ags = split_type tp
  and all_ntvs = count_all_type_variables c
  in
  let cls0 = cls - all_ntvs in
  assert (cls0 = Constants.predicate_class && Array.length ags = 1
        || cls0 = Constants.function_class && Array.length ags = 2);
  let tps = extract_from_tuple n ags.(0) c in
  push_typed0 (Formals.make nms tps) Formals.empty c




let pop (c:t): t =
  (** Pop the last context
   *)
  assert (not (is_global c));
  previous c



let boolean (c:t): term =
  Class_table.boolean_type (count_all_type_variables c)


let tuple_type_of_types (argtps:types) (c:t): type_term =
  (* Convert the type array [argtps = [A,B,...]] into the tuple type
     [(A,B,...)] *)
  let ntvs = count_all_type_variables c in
  Class_table.to_tuple ntvs 0 argtps


let make_lambda
    (tps:Formals.t) (fgs:Formals.t) (ps:term list) (t:term)
    (rt:type_term option)
    (c:t)
    : term =
  let nbenv = count_variables c
  and ntvs  = count_all_type_variables c
  in
  Feature_table.make_lambda tps fgs ps t rt nbenv ntvs c.ft


let rec type_of_term (t:term) (c:t): type_term =
  let nvars = count_variables c in
  match t with
    Variable i when i < nvars -> variable_type i c
  | Variable i -> assert false (* Global constants are not variables *)
  | VAppl(i,args,ags,_) ->
      assert (nvars <= i);
      Feature_table.result_type (i-nvars) ags (count_all_type_variables c) c.ft
  | Application(f,args,_) ->
      let f_tp = type_of_term f c in
      let cls,ags = split_type f_tp in
      if cls = function_class c then begin
        assert (Array.length ags = 2);
        ags.(1)
      end else if cls = predicate_class c then begin
        assert (Array.length ags = 1);
        boolean c
      end else
        assert false (* cannot happen, must be either a predicate or a function *)
  | Lam(tps,fgs,_,_,rt) ->
     let tup_tp = tuple_type_of_types (Formals.types tps) c in
     begin
       match rt with
       | None ->
          predicate_type tup_tp c
       | Some rt ->
          function_type tup_tp rt c
     end
  | QExp _            -> boolean c
  | Indset (_,tp,_)   -> tp
  | Ifexp (cond,a,b) ->
     type_of_term a c
  | Asexp _ ->
     boolean c
  | Inspect (insp,cases) ->
     assert (Array.length cases > 0);
     let fs,pat,res = cases.(0) in
     let c1 = push_typed0 fs Formals.empty c in
     type_of_term res c1



let class_of_type (tp:type_term) (c:t): int =
  Tvars.principal_class tp (tvars c)


let class_of_term (t:term) (c:t): int =
  class_of_type (type_of_term t c) c


let predicate_of_type (tp:type_term) (c:t): type_term =
  let pred_idx = predicate_class c in
  make_type pred_idx [|tp|]


let predicate_of_term (t:term) (c:t): type_term =
  let tp = type_of_term t c in
  predicate_of_type tp c



let tuple_type_of_terms (args:arguments) (c:t): type_term =
  let argtps = Array.map (fun t -> type_of_term t c) args in
  tuple_type_of_types argtps c


let function_of_types (argtps:types) (r_tp:type_term) (c:t): type_term =
  let fidx = function_class c
  and tup  = tuple_type_of_types argtps c in
  make_type fidx [|tup;r_tp|]




let function_of_terms (args:arguments) (result:term) (c:t): type_term =
  let r_tp = type_of_term result c
  and argtps = Array.map (fun t -> type_of_term t c) args in
  function_of_types argtps r_tp c




let and_term (t1:term) (t2:term) (c:t): term =
  Term.binary (and_index c) t1 t2


let not_term (t:term) (c:t): term =
  Term.unary (not_index c) t


let implication_term (t1:term) (t2:term) (c:t): term =
  Term.binary (implication_index c) t1 t2


let implication_chain (ps_rev:term list) (tgt:term) (c:t): term =
  Term.make_implication_chain ps_rev tgt (implication_index c)


let equality_term (t1:term) (t2:term) (c:t): term =
  let tp = type_of_term t1 c
  and nvars = count_variables c
  and tvs = tvars c in
  if not (Term.equivalent tp (type_of_term t2 c)) then
    raise Not_found;
  Feature_table.equality_term t1 t2 nvars tp tvs c.ft


let update_types (subs:type_term array) (c:t): unit =
  let len = Array.length subs in
  assert (len = Tvars.count_local c.entry.tvs);
  let tps = Formals.types c.entry.fargs  in
  Array.iteri
    (fun i tp ->
      let tp = Term.subst tp len subs in
      tps.(i) <- tp)
    tps



let entry_arguments_string (e:entry) (ct:Class_table.t): string =
  (* The string "(a:A, b1,b2:B, ... )" of all local arguments of the entry [e].
     In case that there are no arguments the empty string is returned and
     not "()". In case that there are formal generics they are prefixed.
   *)
  let nargs = entry_arity e
  and tvs   = e.tvs
  in
  let nms = Array.sub (Formals.names e.fargs) 0 nargs
  and tps = Array.sub (Formals.types e.fargs) 0 nargs
  in
  let args = Myarray.combine nms tps in
  Class_table.arguments_string tvs args ct



let entry_full_arguments_string (e:entry) (ct:Class_table.t): string =
  (* The string "(a:A, b1,b2:B, ... )" of all arguments of the entry [e].
     In case that there are no arguments the empty string is returned and
     not "()". In case that there are formal generics they are prefixed.
   *)
  let tvs   = e.tvs
  in
  let nms = Formals.names e.fargs
  and tps = Formals.types e.fargs
  in
  let args = Myarray.combine nms tps in
  Class_table.arguments_string tvs args ct



let ith_arguments_string (i:int) (c:t): string =
  assert (i <= depth c);
  let e = ith_entry i c
  and ct = class_table c
  in
  entry_arguments_string e ct


let local_arguments_string (c:t): string =
  let ct = class_table c in
  entry_arguments_string c.entry ct


let arguments_string (c:t): string =
  let ct = class_table c in
  entry_full_arguments_string c.entry ct


let result_string (e:entry) (ct:Class_table.t): string =
  if Result_type.has_result e.result then
    Class_table.type2string
      (Result_type.result e.result) 0 (entry_fgnames e) ct
  else ""


let named_signature_string (c:t): string =
  (** Print the signature of the context [c] with all argument names.
   *)
  let ct = class_table c in
  let argsstr = entry_arguments_string c.entry ct
  and resstr  = result_string    c.entry ct
  in
  let has_args = argsstr <> ""
  and has_res  = resstr <> ""
  in
  if has_args && has_res then
    argsstr ^ ": " ^ resstr
  else if has_args then
    argsstr
  else
    resstr





let string_of_assertion (t:term) (c: t): string =
  "all"
  ^ (named_signature_string c) ^ " "
  ^ (string_of_term t c)


let find_features (fn:feature_name) (c:t): int list =
  Feature_table.find_features fn (count_variables c) c.ft


let find_funcs
    (fn:feature_name)
    (nargs:int)
    (c:t)
    : (int*Tvars.t*Sign.t) list =
  (** Find all the functions with name [fn] and [nargs] arguments in the
      global feature table and transform them into the context.
   *)
  let lst = Feature_table.find_funcs fn nargs c.ft
  in
  let lst = List.rev_map
      (fun (i,tvs,s) -> i+(count_variables c), tvs, s)
      lst
  in
  lst


let find_identifier
    (name:int)
    (nargs_id:int)
    (c:t)
    : (int * Tvars.t * Sign.t) list =
  (** Find the identifier named [name] which accepts [nargs] arguments
      in one of the local contexts or in the global feature table. Return
      the list of variables together with their signature
   *)
  if is_global c then
    find_funcs (FNname name) nargs_id c
  else
    try
      [variable name c]
    with
      Not_found ->
        find_funcs
          (FNname name) nargs_id c



let find_feature
    (fn:feature_name)
    (nargs_feat:int)
    (c:t)
    : (int * Tvars.t * Sign.t) list =
  (** Find the feature named [fn] which accepts [nargs] arguments global
      feature table. Return the list of variables together with their
      signature.
   *)
  find_funcs fn nargs_feat c




let definition_term (idx:int) (nb:int) (ags:agens) (c:t)
    : int * int array * term =
  let nbenv = count_variables c in
  if idx < nb + nbenv then
    raise Not_found
  else
    Feature_table.definition_term idx (nb + nbenv) ags (tvars c) (feature_table c)



let arity (idx:int) (nb:int) (c:t): int =
  let nbenv = count_variables c in
  if idx < nb + nbenv then
    0
  else
    Feature_table.arity (idx-nb-nbenv) c.ft



let complexity (t:term) (c:t): int =
  let nvars = count_variables c
  and tvs   = tvars c in
  Feature_table.complexity t nvars tvs c.ft



let is_inductive_set (i:int) (c:t): bool =
  let nb = count_variables c in
  nb <= i &&
  Feature_table.is_inductive_set i nb c.ft



let inductive_set (t:term) (c:t): term =
  (* The inductive set represented by the term [t]. *)
  let nb = count_variables c
  and tvs  = tvars c in
  let indset i args ags =
    if i < nb then
      raise Not_found
    else
      Feature_table.inductive_set i args ags nb tvs c.ft in
  match t with
    Indset _ ->
      t
  | VAppl (i,args,ags,_) ->
      indset i args ags
  | _ ->
      raise Not_found


let preconditions (idx:int) (nb:int) (c:t): int * int array * term list =
  let nbenv = count_variables c in
  if idx < nb + nbenv then
    0, [||], []
  else
    Feature_table.preconditions idx (nb+nbenv) (feature_table c)



let postconditions (idx:int) (nb:int) (c:t): int * int array * term list =
  let nbenv = count_variables c in
  if idx < nb + nbenv then
    0, [||], []
  else
    Feature_table.postconditions idx (nb+nbenv) (feature_table c)


let function_property (idx:int) (i:int) (args:term array) (c:t): term =
  let nbenv = count_variables c in
  if idx < nbenv then invalid_arg "variables don't have properties";
  Feature_table.function_property idx i nbenv args c.ft


let has_preconditions (idx:int) (nb:int) (c:t): bool =
  let _,_,lst = preconditions idx nb c in
  lst <> []



let domain_type (tp:type_term) (c:t): type_term =
  (* [tp] is either a function type [A->B] or a predicate type {A}. The domain
     type is in both cases A.
   *)
  Class_table.domain_type tp




let domain_of_lambda
      (tps:Formals.t) (fgs:Formals.t) (pres:term list) (nb:int) (c:t)
    : term =
  (* Construct the domain of a lambda expression with the preconditions [pres] where
     the lambda expression is within an environment with [nb] variables more than the
     context [c].

     The lambda expression must have function type because the function 'domain'
     requires a function argument.

         agent (a:A): B
             require
                 p1; p2; ...
             ensure
                 -> ...
             end

     The domain is the set {a:A: p1 and p2 and ... } or {a:A: true} in case that
     there are no preconditions.
   *)
  let nbenv = count_variables c + Formals.count tps
  in
  match pres with
    [] ->
      let true_const = Feature_table.true_constant (nb+nbenv) in
      Lam(tps,fgs, [], true_const, None)
  | p::pres ->
      let and_id = nb + nbenv + Constants.and_index in
      let inner =
        List.fold_left
          (fun t p -> Term.binary and_id t p)
          p
          pres in
      Lam(tps,fgs, [], inner, None)



let domain_of_feature (idx:int) (nb:int) (ags:agens) (c:t): term =
  (* Construct the domain of feature [idx] in an environment with [nb] variables more
     than the context [c].
   *)
  let nbenv = count_variables c in
  if idx < nb + nbenv then
    assert false (* nyi: local features *)
  else
    let tvs = tvars c in
    Feature_table.domain_of_feature idx (nb+nbenv) ags tvs c.ft


let tuple_of_args (args:arguments) (c:t): term =
  (* The arguments [a,b,...] transformed to a tuple (a,b,...).
   *)
  let tp = tuple_type_of_terms args c
  and nvars = count_variables c in
  Feature_table.tuple_of_args args tp nvars c.ft


let args_of_tuple (t:term) (c:t): term array =
  Feature_table.args_of_tuple t (count_variables c) c.ft


let nargs_of_tuple (t:term) (n:int) (tp:type_term) (c:t): term array =
  Feature_table.args_of_tuple_ext t tp (count_variables c) n c.ft


let is_case_match_expression (t:term) (c:t): bool =
  (* Is the term [t] a match expression (i.e. does it consist only of variables of
     the inner context and constructors)? *)
  let nvars0 = count_last_variables c
  and nvars  = count_variables c
  in
  let rec is_match t =
    let is_match_args res args =
      Array.fold_left (fun res arg -> res && is_match arg) res args in
    match t with
      Variable i when i < nvars0 -> true
    | Variable i when i < nvars  -> false
    | Variable i ->
        Feature_table.is_constructor (i-nvars) c.ft
    | VAppl(i,args,_,_) ->
        let res = Feature_table.is_constructor (i-nvars) c.ft in
        is_match_args res args
    | _ ->
        false
  in
  is_match t


exception Type_error of string




let rec type_of_term_full
    (t:term)
    (req_tp: type_term option)
    (trace:bool)
    (c:t)
    : type_term =
  let nvars = count_variables c
  and ntvs  = count_all_type_variables c in
  let split_function_or_predicate
      (tp:type_term)
      : type_term * type_term option =
    let cls,ags = split_type tp in
    assert (ntvs <= cls);
    if cls = predicate_class c then begin
      assert (Array.length ags = 1);
      ags.(0), None
    end else if cls = function_class c then begin
      assert (Array.length ags = 2);
      ags.(0), Some ags.(1)
    end else
      assert false
  in
  let feature_signature (i:int) (ags:agens): Sign.t =
    assert (nvars <= i);
    let tvs,s = Feature_table.signature0 (i-nvars) c.ft in
    if Tvars.count_fgs tvs <> Array.length ags then
      raise (Type_error
               ("The feature \"" ^
                Feature_table.string_of_signature (i-nvars) c.ft ^
                "\" does not have " ^ string_of_int (Array.length ags) ^
                " formal generic(s)"));
    let trans tp = Term.subst tp ntvs ags in
    Sign.map trans s
  in
  let trace_tp tp =
    if trace then
      printf "  %s : %s\n" (string_long_of_term t c) (string_of_type tp c);
    tp
  in
  let check_and_trace tp =
    begin match req_tp with
      Some (req_tp) when req_tp <> tp ->
        raise (Type_error
                 ("\n    term:     " ^ string_of_term t c ^
                  "\n    required type: " ^
                  string_of_type req_tp c ^
                  "\n    actual type:   " ^
                  string_of_type tp c))
    | _ ->
        ()
    end;
    trace_tp tp
  in
  let check_and_trace_sign (s:Sign.t): type_term =
    let req_tp =
      match req_tp with
        None -> assert false
      | Some tp -> tp
    in
    let argtp, rtp = split_function_or_predicate req_tp in
    let act_tp =
      let pr =
        match rtp with
          None ->
            if Sign.result s <> boolean c then
              raise (Type_error
                 ("\n    term:     " ^  string_of_term t c ^
                  "\n    signature: " ^ (string_of_signature s c) ^
                  string_of_type req_tp c ^
                  "\n    cannot be upgraded to: " ^
                  string_of_type req_tp c));
            true
        | Some rtp ->
            false
      in
      Class_table.upgrade_signature ntvs pr s
    in
    trace_tp act_tp
  in
  let check_args reqtps args =
    Array.iteri
      (fun i t -> ignore (type_of_term_full t (Some reqtps.(i)) trace c))
      args
  in
  if trace then begin
    printf "  type of %s\n" (string_long_of_term t c);
    begin match req_tp with
      Some tp ->
        printf "    required type %s\n" (string_of_type tp c)
    | _ ->
        ()
    end;
    printf "    actual type %s\n" (string_of_type (type_of_term t c) c)
  end;
  match t with
    Variable i ->
      assert (i < nvars);
      check_and_trace (variable_type i c)
  | VAppl (i,args,ags,_) ->
      assert (nvars <= i);
      if trace then
        begin
          printf "  feature application %d %s\n"
                 (i-nvars)
                 (Feature_table.string_of_signature (i-nvars) (feature_table c));
          printf "     ags        %s\n" (string_of_ags ags c);
          printf "     signature  %s\n"
                 (string_of_signature (feature_signature i ags) c);
        end;
      let len = Array.length args
      and s   = feature_signature i ags in
      assert (Sign.has_result s);
      if len = Sign.arity s then begin
        check_args (Sign.arguments s) args;
        check_and_trace (Sign.result s)
      end else begin
          if len <> 0 then
            raise (Type_error "len <> 0");
        assert (len = 0);
        check_and_trace_sign s
      end
  | Application (f,args,_) ->
      assert (Array.length args = 1);
      let ftp = type_of_term_full f None trace c in
      let argtp,rtp = split_function_or_predicate ftp in
      ignore (type_of_term_full args.(0) (Some argtp) trace c);
      begin
        match rtp with
          None ->
            check_and_trace (boolean c)
        | Some tp ->
            check_and_trace tp
      end
  | Lam (tps,fgs,ps,t0,rt) ->
     let argtp = tuple_type_of_types (Formals.types tps) c in
     let c1 = push_typed0 tps fgs c in
     ignore (type_of_term_full
               t0
               (if rt = None then
                  Some (boolean c1)
                else
                  rt)
               trace
               c1);
     let tp =
       match rt with
       | None ->
          predicate_type argtp c
       | Some rt ->
          function_type argtp rt c
     in
     check_and_trace tp
  | QExp (tps,fgs,t0,is_all) ->
      assert (ntvs = 0 || fgs = Formals.empty);
      let c1 = push_typed0 tps fgs c in
      ignore (type_of_term_full t0 (Some (boolean c1)) trace c1);
      check_and_trace (boolean c)
  | Indset (nme,tp,rs) ->
      (* Missing: Verify types of the rules!! *)
      check_and_trace tp
  | Ifexp (cond, a, b) ->
     ignore (type_of_term_full cond (Some (boolean c)) trace c);
     let if_tp = type_of_term_full a req_tp trace c in
     ignore (type_of_term_full b (Some if_tp) trace c);
     check_and_trace if_tp
  | Asexp (insp, tps, pat) ->
     let tp = type_of_term_full insp None trace c in
     let nms = anon_argnames (Array.length tps) in
     let c1 = push_typed0 (Formals.make nms tps) Formals.empty c in
     ignore (type_of_term_full pat (Some tp) trace c1);
     check_and_trace (boolean c)
  | Inspect (insp,cases) ->
     let insp_tp = type_of_term_full insp None trace c
     and ncases = Array.length cases
     in
     let rec check_cases_from i req_tp =
       if i = ncases then
         req_tp
       else
         let fs,pat,res = cases.(i) in
         let c1 = push_typed0 fs Formals.empty c in
         ignore(type_of_term_full pat (Some insp_tp) trace c1);
         let req_tp = type_of_term_full res req_tp trace c1 in
         check_cases_from (i+1) (Some req_tp)
     in
     let tp = check_cases_from 0 req_tp in
     begin
       match tp with
       | Some tp ->
          check_and_trace tp
       | _ ->
          assert false
     end



let check_well_typed (t:term) (c:t): unit =
  let check trace = ignore(type_of_term_full t None trace c) in
  try
    check false
  with Type_error str ->
    printf "check_well_typed\n  \"%s\"\n  \"%s\"\n"
      (string_long_of_term t c) (Term.to_string t);
    printf "  type error: %s\n" str;
    check true


let is_well_typed (t:term) (c:t): bool =
  try check_well_typed t c; true
  with Type_error _ -> false



let transformed_term0 (t:term) (nargs:int) (c0:t) (c:t): term =
  (* The term [t] with [nargs] arguments valid in the context [c0] transformed
     to the inner context [c].  *)
  assert (is_outer c0 c);
  assert (count_type_variables c0 = 0);
  assert (count_type_variables c  = 0);
  let nvars0 = count_variables c0
  and nvars  = count_variables c
  and nfgs0  = count_formal_generics c0
  and nfgs   = count_formal_generics c
  in
  assert (nvars0 <= nvars);
  assert (nfgs0  <= nfgs);
  Term.shift_from (nvars-nvars0) nargs (nfgs-nfgs0) 0 t



let transformed_term (t:term) (c0:t) (c:t): term =
  (* The term [t] valid in the context [c0] transformed to the inner context
     [c].
   *)
  transformed_term0 t 0 c0 c


(* Calculation of preconditions:

       a ==> b:   pa0, pa1, ..., a ==> pb0, a ==> pb1, ...

       a and b:   same as 'a ==> b'


       a or b:    pa0, pa1, ..., not a ==> pb0, not a ==> pb1, ...

       if c then a else b end:
                  pc0,pc1,...,
                  c or not c,
                  c ==> pa0,c==>pa1,...,
                  not c ==> pb0, not c ==> pb1,...


       all(x,y,...) e:
                  all(x,y,...) pe0, all(x,y,...) pe1, ...

       some(x,y,...) e:
                  same as 'all(x,y,...) e'

       {x,y,...: e}:
                  same as 'all(x,y,...) e'

       (x,y,...) -> e:
                  no preconditions

       f(x,y,...): px, py, ...               -- function object application
                   (x,y,...) in f.domain

       p(x,y,...): px, py, ...               -- predicate (relation) application
                   no preconditions

       f(x,y,...):
                  pf0, pf1, ...
                  px0,...,py0,...,...
                  (x,y,...).(f.domain)

       agent(x:X,y:Y,...)
           require r0;r1;...
           ensure  -> t
           end

   Lambda terms

      A lambda term is either

          {a,b,...: t}

      or

          agent (a,b,...)
              require
                  r0; r1; ...
              ensure
                  -> t
              end

      The preconditions are:

             all(a,b,..) p0r0     -- The preconditions of 'r0' have to be
             all(a,b,..) p1r0     -- satisfied
             ...
             all(a,b,..) r0 ==> p0r1
             all(a,b,..) r0 ==> p1r1
             ...
             all(a,b,..) r0 ==> r1 ==> p0r2
             all(a,b,..) r0 ==> r1 ==> p1r2
             ...
             ...
             all(a,b,..) r0 ==> r1 ==> ... ==> p0t
             all(a,b,..) r0 ==> r1 ==> ... ==> p1t
             ...

      Note: A set expression does not have inner preconditions, therefore we
            get as a special case (which is included in the above scheme):

             all(a,b,..) p0t
             all(a,b,..) p1t
             ...
   *)


let case_preconditions
      (insp:term) (insp_tp:type_term)      (* Inspected term *)
      (tps: Formals.t)                     (* Case context *)
      (pat:term)
      (pres:term list)            (* Preconditions of the result expression *)
      (lst:term list)             (* List to prepend preconditions *)
      (c:t)
    : term list =
  (*  Generate from pres the list of terms of the form

          all(x,y,...) insp = pat ==> pre

      If the inspected expression and the pattern are tuples [i1,i2,...]
      [p1,p2,...], then the precondition term is

          all(x,y,...) i1 = p1 ==> i2 = p2 ==> ... ==> pre

   *)
  let n = Formals.count tps in
  let insp   = Term.up n insp
  and c1 = push_typed0 tps Formals.empty c
  in
  let insp_arr = args_of_tuple insp c1 in
  let ntup = Array.length insp_arr in
  let pat_arr = nargs_of_tuple pat ntup insp_tp c1
  in
  assert (ntup = Array.length pat_arr);
  List.fold_left
    (fun lst pre ->
      let eqlst_rev =
        interval_fold
          (fun eqlst i ->
            (equality_term insp_arr.(i) pat_arr.(i) c1) :: eqlst
          )
          [] 0 ntup
      in
      let chn = implication_chain eqlst_rev pre c1 in
      (Term.all_quantified tps Formals.empty chn) :: lst
    )
    lst
    pres





let term_preconditions (t:term)  (c:t): term list =
  (* The preconditions of the term [t] *)
  assert (has_no_type_variables c);
  let all_ntvs = count_all_type_variables c
  in
  let rec pres
      (t:term) (lst:term list) (c:t)
      : term list =
    let imp_id = implication_index c
    and or_id  = or_index c
    and and_id = and_index c
    and not_id = not_index c in
    let pres_args args lst =
      Array.fold_left
        (fun lst t -> pres t lst c)
        lst
        args
    in
    match t with
    | Variable i ->
       lst
    | VAppl (i,args,_,_) when i = imp_id || i = and_id ->
        assert (Array.length args = 2);
        let lst1 = pres args.(0) lst c
        and lst2 = pres args.(1) [] c in
        List.fold_right
          (fun t lst -> (Term.binary imp_id args.(0) t) :: lst)
          lst2
          lst1
    | VAppl (i, args,_,_) when i = or_id ->
        assert (Array.length args = 2);
        let lst1  = pres args.(0) lst c
        and lst2  = pres args.(1) [] c
        and not_t = Term.unary not_id args.(0) in
        List.fold_right
          (fun t lst -> (Term.binary imp_id not_t t)::lst)
          lst2
          lst1
    | VAppl (i,args,ags,_) ->
        if Array.length args = 0 && arity i 0 c > 0 then
          lst
        else
          let tvs = tvars c
          and nb  = count_variables c in
          let n,nms,lst1 = preconditions i 0 c in
          assert (n = Array.length args);
          let lst = pres_args args lst in
          List.fold_left
            (fun lst t ->
              let t =
                Feature_table.substituted t n nb 0 args 0 ags tvs c.ft
              in
              t :: lst
            )
            lst
            lst1
    | Application (f,args,_) ->
        let lst  = pres f lst c in
        let lst = pres_args args lst
        in
        let f_tp = type_of_term f c in
        let cls,ags = split_type f_tp in
        if cls = predicate_class c then
          lst
        else if cls = function_class c then
          let dom = VAppl (domain_index c,[|f|],ags,false) in
          Application(dom,args,false) :: lst
        else
          assert false (* Cannot happen *)
    | Lam (tps,fgs,pres0,t0,rt) ->
       let n = Formals.count tps
       in
       let c      = push_typed0 tps fgs c
       and imp_id = n + imp_id
       in
       let prepend_pres
             (ps_rev:term list) (tgt:term) (lst:term list): term list =
         let chn = Term.make_implication_chain ps_rev tgt imp_id in
         QExp (tps, fgs, chn, true) :: lst
       in
       let prepend_pres_of_term
             (ps_rev:term list) (p:term) (lst:term list)
           : term list =
         let pres_p_rev = pres p [] c in
         List.fold_left
           (fun lst p -> prepend_pres ps_rev p lst)
           lst
           (List.rev pres_p_rev)
       in
       let ps_rev,lst =
         List.fold_left
           (fun (ps_rev,lst) p ->
             p :: ps_rev,
             prepend_pres_of_term ps_rev p lst
           )
           ([],lst)
           pres0
       in
       prepend_pres_of_term ps_rev t0 lst
    | QExp (tps,fgs,t0,is_all) ->
        let c = push_typed0 tps fgs c in
        let lst0 = pres t0 [] c in
        List.fold_right
          (fun t lst ->
            QExp(tps,fgs,t,true) :: lst
          )
          lst0
          lst
    | Indset (nme,tp,rs) ->
       let tps = Formals.make [|nme|] [|tp|] in
        let c = push_typed0 tps Formals.empty c in
        let lst =
          Array.fold_left
            (fun lst r ->
              let lst_r = pres r [] c in (* reversed *)
              let lst_r =
                List.rev_map
                  (fun p -> Term.all_quantified tps Formals.empty p)
                  lst_r in
              List.rev_append lst_r lst)
            lst
            rs in
        lst
    | Ifexp (cond, a, b) ->
       let lstcond = pres cond lst c
       and lsta    = pres a [] c
       and lstb    = pres b [] c
       and negcond = not_term cond c
       in
       let reslst =
         List.fold_right
           (fun t lst -> (implication_term cond t c) :: lst)
           lsta
           lstcond
       in
       List.fold_right
         (fun t lst -> (implication_term negcond t c) :: lst)
         lstb
         reslst
    | Asexp (insp, tps, pat) ->
       pres insp lst c
    | Inspect (insp,cases) ->
       let nvars = count_variables c
       and insp_tp = type_of_term insp c in
       let lst = pres insp lst c (* inspected term *)
       in
       let lst = (* all unmatched cases *)
         List.fold_left
           (fun lst (n,tps,pat) ->
             (not_term (Asexp (insp,Array.of_list tps,pat)) c) :: lst
           )
           lst
           (Feature_table.unmatched_inspect_cases cases nvars all_ntvs c.ft)
       in
       Array.fold_left (* all preconditions of the results *)
         (fun lst (fs,pat,res) ->
           let c1 = push_typed0 fs Formals.empty c in
           let lst_inner = List.rev (pres res [] c1) in
           case_preconditions insp insp_tp
                              fs
                              pat lst_inner lst c
         )
         lst
         cases
  in
  List.rev_map
    (fun p -> prenex_term p c)
    (pres t [] c)

let existence_condition (posts:term list) (c:t): term =
  (* Generate the existence condition

         some(x:RT) e1_x and e2_x and ...

     where [ei_x] is the ith postcondition with the variable [Result] substituted
     by [x] and [RT] is the result type of the function.
   *)
  assert (has_result_variable c);
  assert (posts <> []);
  let nargs   = count_last_arguments c
  and and_id  = 1 + and_index c
  in
  let args =
    Array.init
      (1 + nargs)
      (fun i -> if i < nargs then Variable (1+i) else Variable 0) in
  let replace t =
    Term.subst t (2 + nargs) args
  in
  let term_inner =
    List.fold_left
      (fun inner p -> Term.binary and_id inner (replace p))
      (replace (List.hd posts))
      (List.tl posts) in
  Term.some_quantified
    (Formals.make [|ST.symbol "x"|] [|result_type c|])
    term_inner



let uniqueness_condition (posts:term list) (c:t): term =
  (* Generate the uniqueness condition

         all(x,y:RT) e1_x ==> e1_y ==> ...
                     e2_x ==> e2_y ==> ... ==> x = y

     where [ei_x]/[ei_y] is the ith postcondition with the variable [Result]
     substituted by [x]/[y].
   *)
  assert (is_toplevel c);
  assert (has_result_variable c);
  assert (posts <> []);
  let nargs   = count_last_arguments c
  and nvars   = count_last_variables c
  and imp_id  = 2 + implication_index c
  and rt      = result_type c
  and tvs     = tvars c
  in
  let args_var xyvar  =
    assert (xyvar < 2);
    Array.init
      (1 + nargs)
      (fun i -> if i < nargs then Variable (2+i) else Variable xyvar) in
  let argsx = args_var 0
  and argsy = args_var 1 in
  let replace_by_args t args =
    Term.subst t (3 + nargs) args
  in
  let x_eq_y =
    let eq_id_0 = 2 + nvars + Constants.eq_index in
    let x_eq_y_0 = VAppl (eq_id_0, [|Variable 0; Variable 1|], [|Variable 0|],false)
    in
    Feature_table.substituted
      x_eq_y_0  0  (2+nvars)  0  [||]  0  [|rt|] tvs c.ft
  in
  let term_inner =
    List.fold_right
      (fun t inner ->
        let t1 = Term.binary imp_id (replace_by_args t argsy) inner in
        Term.binary imp_id (replace_by_args t argsx) t1
      )
      posts
      x_eq_y
  and nms = [|ST.symbol "x"; ST.symbol "y"|]
  and tps = [|rt; rt|]
  in
  Term.all_quantified (Formals.make nms tps) Formals.empty term_inner





let function_postconditions (idx:int) (posts:term list) (c:t): term list =
  (* Generate the postconditions

     [e1_f; e2_f; ...]

     where [ei_f] is [ei] with the variable [Result] substituted by [f(a,b,...)] and
     [idx] is the index of the function [f].
   *)
  assert (has_result_variable c);
  assert (posts <> []);
  let tvs   = Feature_table.tvars idx c.ft
  and nargs = count_last_arguments c
  in
  let fargs = Array.init nargs (fun i -> Variable i)
  and ags   = Array.init (Tvars.count_fgs tvs) (fun i -> Variable i)
  in
  let fterm = VAppl (1+nargs+idx, fargs, ags, false) in
  let args  =
    Array.init (1+nargs) (fun i -> if i < nargs then Variable i else fterm) in
  let replace t = Term.subst t (1+nargs) args in
  List.map replace posts


let get_type (tp:type_t withinfo) (c:t): type_term =
  let tvs = tvars c in
  Class_table.get_type tp false tvs (class_table c)


let downgrade_term (t:term) (nb:int) (c:t): term =
  Feature_table.downgrade_term
    t
    (nb + count_variables c)
    (count_all_type_variables c)
    c.ft


let arity_of_downgraded_type (tp:type_term) (c:t): int =
  let ntvs = Tvars.count_all c.entry.tvs in
  Class_table.arity_of_downgraded ntvs tp

let specialized (t:term) (c:t): term =
  let nvars = count_variables c
  and tvs   = tvars c in
  Feature_table.specialized t nvars tvs c.ft
