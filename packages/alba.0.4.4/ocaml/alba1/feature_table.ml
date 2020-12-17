(* Copyright (C) 2015 Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Container
open Support
open Term
open Signature
open Printf

module Option = Fmlib.Option

module IntArrayMap = Map.Make(struct
  let compare = Stdlib.compare
  type t = int array
end)


let false_constant (nb:int): term =
  let id = nb + Constants.false_index in
  VAppl (id,[||],[||],false)

let true_constant (nb:int): term =
  let id = nb + Constants.true_index in
  VAppl (id,[||],[||],false)


type definition = term

type formal     = int * term


class bdesc
        (is_exp:bool)
        (idx:int)
        (nfgs:int)
        (classes:int array)
        (spec:Feature.Spec.t)
  =
  object (self:'self)
    val mutable is_inh = false
    val mutable is_exp = is_exp
    val mutable spec = spec
    val mutable sd = idx                    (* each new feature is its own seed *)
    val mutable subst = standard_substitution nfgs
    val mutable vars = IntArrayMap.singleton classes idx  (* and own variant *)
    val mutable is_eq = (idx = Constants.eq_index)
    val mutable inv_ass = IntSet.empty

    method is_inherited:bool = is_inh
    method is_exported:bool = is_exp
    method seed:int = sd
    method ags:agens = subst
    method specification:Feature.Spec.t = spec
    method variant (classes:int array): int = IntArrayMap.find classes vars
    method has_variant (classes:int array): bool =
      IntArrayMap.mem classes vars
    method variants: int IntArrayMap.t = vars
    method is_equality: bool = is_eq
    method involved_assertions: IntSet.t = inv_ass
    method set_specification (sp:Feature.Spec.t): unit =
      spec <- sp
    method set_seed (s:int) (ags:agens):unit =
      sd <- s;
      subst <- ags;
      if sd = Constants.eq_index then
        is_eq <- true
    method add_variant (classes:int array) (idx:int): unit =
      assert (not (IntArrayMap.mem classes vars));
      vars <- IntArrayMap.add classes idx vars
    method set_inherited: unit =
      is_inh <- true
    method set_exported: unit =
      is_exp <- true
    method add_assertion (i:int): unit =
      inv_ass <- IntSet.add i inv_ass
  end

type descriptor = {
    mdl: Module.M.t;
    dominant_cls:int option;
    dominant_fg: int option;
    fname:       feature_name;
    impl:        Feature.implementation;
    tvs:         Tvars.t;         (* only formal generics *)
    argnames:    names;
    sign:        Sign.t;
    mutable tp:  type_term;
    bdesc:       bdesc;
    mutable recognizers: (term * term) list; (* reco, ghost_reco *)
    mutable projectors: int IntMap.t;
    mutable co_preconditions: term list;
    mutable is_constr: bool
  }

type t = {
    mutable map: Term_table.t ref Feature_map.t; (* search table *)
    seq:         descriptor seq;
    mutable base:int list ref IntMap.t; (* module name -> list of features *)
    ct:          Class_table.t;
    verbosity:   int
  }



let empty (comp:Module.Compile.t): t =
  let verbosity = Module.Compile.verbosity comp in
  {map  = Feature_map.empty;
   seq  = Seq.empty ();
   base = IntMap.empty;
   ct   = Class_table.base_table (comp);
   verbosity = verbosity}


let class_table (ft:t):  Class_table.t   = ft.ct
let compilation_context (ft:t): Module.Compile.t =
  Class_table.compilation_context ft.ct

let is_private (ft:t): bool = Class_table.is_private ft.ct
let is_public  (ft:t): bool = Class_table.is_public  ft.ct
let is_interface_check  (ft:t): bool = Class_table.is_interface_check ft.ct
let is_interface_use (ft:t): bool = Class_table.is_interface_use ft.ct
let is_interface_public_use (ft:t): bool =
  Class_table.is_interface_public_use ft.ct


let current_module (ft:t): Module.M.t =
  compilation_context ft |> Module.Compile.current

let is_current_module (m:Module.M.t) (ft:t): bool =
  Module.M.equal m (current_module ft)

let core_module (ft:t): Module.M.t =
  Class_table.core_module ft.ct

let count (ft:t): int =
  Seq.count ft.seq


let verbosity (ft:t): int = ft.verbosity

let descriptor (i:int) (ft:t): descriptor =
  assert (i < count ft);
  Seq.elem i ft.seq


let tvars (i:int) (ft:t): Tvars.t =
  (descriptor i ft).tvs

let signature0 (i:int) (ft:t): Tvars.t * Sign.t =
  let desc = descriptor i ft in
  desc.tvs, desc.sign


let argument_types (i:int) (ags:agens) (ntvs:int) (ft:t): types =
  let tvs,s = signature0 i ft in
  let tps = Sign.arguments s in
  Term.subst_array tps ntvs ags


let result_type (i:int) (ags:agens) (ntvs:int) (ft:t): type_term =
  let tvs,s = signature0 i ft in
  assert (Sign.has_result s);
  let rt = Sign.result s in
  Term.subst rt ntvs ags


let signature (i:int) (ags:agens) (ntvs:int) (ft:t): Sign.t =
  (* The signature of the feature [i] where the formal generics are substituted
     by the actual generics [ags] coming from a type environment with [ntvs]
     type variables. *)
  let argtps = argument_types i ags ntvs ft
  and rtp    = result_type i ags ntvs ft in
  Sign.make_func argtps rtp


let argument_names (i:int) (ft:t): int array =
  (descriptor i ft).argnames


let arity (i:int) (ft:t): int =
  Sign.arity (descriptor i ft).sign


let is_higher_order (i:int) (ft:t): bool =
  let desc = descriptor i ft in
  assert (Sign.has_result desc.sign);
  let ntvs = Tvars.count_all desc.tvs in
  let cls,_ = split_type (Sign.result desc.sign) in
  assert (ntvs <= cls);
  let cls = cls - ntvs in
  cls = Constants.predicate_class || cls = Constants.function_class


let tuple_arity (i:int) (ft:t): int =
  assert (is_higher_order i ft);
  let desc = descriptor i ft in
  assert (Sign.has_result desc.sign);
  let ntvs = Tvars.count_all desc.tvs in
  let _,args = split_type (Sign.result desc.sign) in
  assert (Array.length args = 1);
  let args = Class_table.extract_from_tuple_max ntvs args.(0) in
  Array.length args



let string_of_signature (i:int) (ft:t): string =
  let desc = descriptor i ft in
  (feature_name_to_string desc.fname) ^ " " ^
  (Class_table.string_of_complete_signature desc.sign desc.tvs ft.ct)



let is_feature_visible (i:int) (ft:t): bool =
  assert (i < count ft);
  not (is_interface_check ft) || (descriptor i ft).bdesc#is_exported



let feature_name (i:int) (ft:t): string =
  let desc = descriptor i ft in
  feature_name_to_string desc.fname


let base_descriptor (i:int) (ft:t): bdesc =
  assert (i < count ft);
  (descriptor i ft).bdesc



let has_owner (i:int) (ft:t): bool =
  assert (i < count ft);
  Option.has (descriptor i ft).dominant_cls


let owner (i:int) (ft:t): int =
  assert (i < count ft);
  Option.value (descriptor i ft).dominant_cls


let is_desc_deferred (desc:descriptor): bool =
  match desc.impl with
    Feature.Deferred -> true
  | _                -> false


let is_deferred (i:int) (ft:t): bool =
  assert (i < count ft);
  is_desc_deferred (descriptor i ft)



let dominant_formal_generic (i:int) (ft:t): int =
  assert (i < count ft);
  assert (is_deferred i ft);
  Option.value (descriptor i ft).dominant_fg



let is_ghost_function (i:int) (ft:t): bool =
  assert (i < count ft);
  let desc = descriptor i ft in
  Sign.is_ghost desc.sign


let is_constructor (i:int) (ft:t): bool =
  assert (i < count ft);
  match (descriptor i ft).dominant_cls with
  | Some cls when Class_table.can_match_pattern cls ft.ct ->
     IntSet.mem i (Class_table.constructors cls ft.ct)
  | _ ->
     false

let is_pseudo_constructor (i:int) (ft:t): bool =
  match (descriptor i ft).dominant_cls with
  | Some cls ->
     Class_table.is_pseudo_inductive cls ft.ct
     && Class_table.can_match_pattern cls ft.ct
     && IntSet.mem i (Class_table.constructors cls ft.ct)
  | _ ->
     false

let inductive_type (i:int) (ags:agens) (ntvs:int) (ft:t): type_term =
  assert (is_constructor i ft);
  let ctp, _ = Class_table.class_type (owner i ft) ft.ct in
  Term.subst ctp ntvs ags




let recursive_arguments (i:int) (ft:t): int list =
  (* List of recursive arguments *)
  let desc = descriptor i ft in
  let lst =
    interval_fold
      (fun lst i ->
        if Sign.arg_type i desc.sign = Sign.result desc.sign then
          i :: lst
        else
          lst
      )
      []
      0 (Sign.arity desc.sign) in
  List.rev lst


let feature_call(i:int) (nb:int) (args:arguments) (ags:agens) (ft:t): term =
  (* Construct the term [f(a,b,...)] for the feature [i] for an environment
     with [nb] variables.*)
  let len = Array.length args in
  assert (arity i ft = len);
  VAppl (i+nb, args, ags, false)


let constructor_rule (idx:int) (p:term) (ags:agens) (nb:int) (ft:t)
    : int * names * types * term list * term =
  (* Construct the rule for the constructor [idx] where the constructor [idx] has
     the form [c(a1,a2,...)] where [ar1,ar2,...] are recursive.

         all(args) p(ar1) ==> p(ar2) ==> ... ==> p(c(a1,a2,...))

     [p] is the predicate term which expects objects of the inductive type
     belonging to the constructor [i] and [ags] are the actual generics substituted
     for the formal generics of the inductive type.
   *)
  let desc = descriptor idx ft in
  let tps = Sign.arguments desc.sign
  in
  let n       = arity idx ft in
  let nms     = standard_argnames n in
  let p       = Term.up n p in
  let call    = VAppl (idx+n+nb, standard_substitution n, ags, false) in
  let tgt     = Application(p,[|call|],false) in
  let indargs = recursive_arguments idx ft in
  let ps_rev  =
    List.rev_map
      (fun iarg -> Application(p,[|Variable iarg|],false)) indargs in
  n,nms,tps,ps_rev,tgt



let induction_law (cls:int) (nb:int) (ft:t): term =
  (* Construct the induction law

     all(p,x) ind1 ==> ... ==> indn ==> p(x)
   *)
  assert (nb = 0); (* global only *)
  assert (Class_table.is_inductive cls ft.ct);
  let cons = Class_table.constructors cls ft.ct
  and tp,tvs = Class_table.class_type cls ft.ct
  and imp_id = nb + 2 + Constants.implication_index
  and p  = Variable 0
  and x  = Variable 1
  in
  let fgnms,fgcon = Tvars.fgnames tvs, Tvars.fgconcepts tvs
  and nms = [|ST.symbol "p"; ST.symbol "x"|]
  and tps = [|Class_table.predicate_type tp (Tvars.count_all tvs); tp|] in
  let lst = List.rev (IntSet.elements cons) in
  let t0 =
    List.fold_left
      (fun tgt idx ->
        let rule =
          let ags = Array.init (Array.length fgcon) (fun i -> Variable i) in
          let n,nms,tps,ps_rev,tgt = constructor_rule idx p ags (nb+2) ft in
          let chn  = Term.make_implication_chain ps_rev tgt (n+imp_id) in
          Term.all_quantified (Formals.make nms tps) Formals.empty chn in
        let rule = Term.prenex rule (2+nb) (Tvars.count_fgs tvs) imp_id in
        Term.binary imp_id rule tgt)
      (Application(p,[|x|],false))
      lst in
  Term.all_quantified (Formals.make nms tps) (Formals.make fgnms fgcon) t0



let is_term_visible (t:term) (nbenv:int) (ft:t): bool =
  let rec check_visi t nb =
    let check_visi_i i =
      let i = i - nb in
      assert (i < count ft);
      if not (is_feature_visible i ft) then
        raise Not_found
      else
        ()
    and check_args args nb = Array.iter (fun t -> check_visi t nb) args
    and check_lst  lst nb  = List.iter  (fun t -> check_visi t nb) lst
    in
    match t with
      Variable i when i < nb ->
        ()
    | Variable i ->
        check_visi_i i
    | VAppl(i,args,_,_) ->
        check_visi_i i;
        check_args args nb
    | Application(f,args,_) ->
        check_visi f nb;
        check_args args nb
    | Lam(tps,_,pres0,t0,_) ->
       let nb = nb + Formals.count tps in
       check_lst pres0 nb;
       check_visi t0 nb
    | QExp(tps,_,t0,_) ->
        check_visi t0 (Formals.count tps + nb)
    | Ifexp (cond,a,b) ->
       check_visi cond nb;
       check_visi a nb;
       check_visi b nb
    | Asexp (insp,tps,pat) ->
       check_visi insp nb;
       check_visi pat (Array.length tps)
    | Inspect(insp,cases) ->
       check_visi insp nb;
       Array.iter
         (fun (fs,pat,res) ->
           let nb = nb + Array2.count fs in
           check_visi pat nb;
           check_visi res nb
         )
         cases
    | Indset (nme,tp,rs) ->
        check_args rs (1+nb)
  in
  try
    check_visi t nbenv;
    true
  with Not_found ->
    false


let args_of_tuple (t:term) (nbenv:int) (ft:t): term array =
  (* The tuple (a,b,...) transformed into an argument array [a,b,...].
   *)
  let tuple_id  = nbenv + Constants.tuple_index in
  let rec collect t lst =
    match t with
      VAppl (i,args,_,_) when i = tuple_id ->
        assert (Array.length args = 2);
        let lst = args.(0) :: lst in
        collect args.(1) lst
    | _ ->
        t :: lst
  in
  let lst = collect t [] in
  Array.of_list (List.rev lst)


let ith_tuple_element
     (i:int) (n:int) (t:term) (tup_tp:type_term) (nvars:int) (ft:t)
    : term =
  (* The [i]th of [n] tuple elements of term [t] which has the tuple type
     [tup_tp] and comes from an enviroment with [nvars] variables.

     Example: Tuple elements of a 4-tuple:

          0: t.first
          1: t.second.first
          2: t.second.second.first
          3: t.second.second.second

          i + 1 < n:  i times second + first
          i + 1 = n:  i times second
   *)
  let first_id  = nvars + Constants.first_index
  and second_id = nvars + Constants.second_index
  in
  let elem (i:int) (tp:type_term): term =
    let split_tup (tp:type_term): agens =
      let _,ags = split_type tp in
      assert (Array.length ags = 2);
      ags
    in
    let rec seconds (m:int) (t:term) (tp:type_term): term * type_term =
      assert (0 <= m);
      if m = 0 then
        t, tp
      else
        let ags = split_tup tp in
        let t = VAppl (second_id, [|t|], ags, true) in
        seconds (m - 1) t ags.(1)
    in
    if i + 1 < n then
      let t, tp = seconds i t tup_tp in
      let ags = split_tup tp in
      VAppl (first_id, [|t|], ags, true)
    else if i + 1 = n then
      let t,_ = seconds i t tup_tp in
      t
    else
      assert false (* cannot happen *)
  in
  elem i tup_tp


let args_of_tuple_ext
    (t:term) (tp:type_term) (nvars:int) (nargs:int) (ft:t)
    : term array =
  (* The tuple [t = (a,b,...,z)] with type [tp] transformed into an argument
     array [a,b,...].

     If the tuple [t] is only an n-tuple and [nargs]
     arguments are needed with [n < nargs] the last tuple element [z] is used
     to generated the missing elements as

         z.first,  z.second.first, z.second...first, z.second...second
   *)

  assert (0 < nargs);
  let tuple_id  = nvars + Constants.tuple_index
  in
  let rec untup
      (t:term) (tp:type_term) (n:int) (lst:term list)
      : int * term list * type_term =
    assert (n < nargs);
    if n + 1 = nargs then
      n + 1, t::lst, tp
    else
      match t with
        VAppl(i,args,ags,_) when i = tuple_id ->
          assert (Array.length args = 2);
          assert (Array.length ags  = 2);
          untup args.(1) ags.(1) (n+1) (args.(0)::lst)
      | _ ->
          n + 1, t::lst, tp
  in
  let n, lst_rev, tp_last = untup t tp 0 []
  in
  let lst_rev =
    if n < nargs then begin
      match lst_rev with
        [] ->
          assert false
      | last::tail ->
          let delta = nargs - n + 1 in
          let rec add_args_from i lst_rev =
            if i = delta then
              lst_rev
            else
              add_args_from
                (i + 1)
                ((ith_tuple_element i delta last tp_last nvars ft) :: lst_rev)
          in
          add_args_from 0 tail
    end else
      lst_rev
  in
  assert (List.length lst_rev = nargs);
  Array.of_list (List.rev lst_rev)



let tuple_of_args
    (args:arguments) (tup_tp:type_term) (nb:int) (ft:t)
    : term =
  (* The arguments [a,b,...] transformed to a tuple (a,b,...) of the type [tup_tp].
   *)
  let nargs = Array.length args
  and tup_id = nb + Constants.tuple_index
  in
  assert (0 < nargs);
  let rec tup_from (i:int) (tup_tp:type_term): term =
    if i = nargs - 1 then
      args.(i)
    else begin
      assert (i + 2 <= nargs);
      let _,ags = split_type tup_tp in
      assert (Array.length ags = 2);
      let b = tup_from (i + 1) ags.(1) in
      VAppl (tup_id, [| args.(i); b |], ags, false)
    end
  in
  tup_from 0 tup_tp





let make_lambda
      (tps:formals) (fgs:formals) (ps: term list) (t:term)
      (rt:type_term option)
      (nvars:int) (ntvs:int)
      (ft:t)
    : term =
  assert (Formals.count tps > 0);
  Lam(tps,fgs,ps,t,rt)



let make_application
    (f:term) (args:term array) (tup:type_term) (nbenv:int) (ft:t)
    : term =
  assert (Array.length args > 0);
  let args =
    if Array.length args = 1 then
      args
    else
      [|tuple_of_args args tup nbenv ft|]
  in
  Application (f, args, false)



let beta_reduce
      (n:int) (tlam: term) (tup_tp:type_term) (args:term array)
      (nbenv:int) (ntvs:int) (ft:t)
    : term =
  (* Beta reduce the expression ((x,y,...)->tlam)(args). The expression comes from
     an environment with [nbenv] variables. The expression is normalized i.e. fully
     tupelized i.e. it has one argument which is an [n] tuple.

     [tp] is the type of the lambda term.
   *)
  let len = Array.length args in
  assert (0 < len);
  assert (len <= n);
  let args =
    if len < n then
      let tps = Class_table.extract_from_tuple n ntvs tup_tp in
      let tup_tp2 = Class_table.to_tuple ntvs (len-1) tps in
      let args2 = args_of_tuple_ext args.(len-1) tup_tp2 nbenv (n-len+1) ft
      in
      Array.append (Array.sub args 0 (len-1)) args2
    else
      args
  in
  assert (Array.length args = n);
  Term.apply tlam args




let fake_tuple_type (n:int): type_term =
  assert (1 <= n);
  let rec tup i tp =
    if i = 0 then
      tp
    else
      let tp = make_type (-1) [|empty_term; tp|] in
      tup (i - 1) tp
  in
  tup (n - 1) empty_term




let term_to_string
    (t:term)
    (norm:bool)
    (long:bool)
    (nanon: int)
    (names: int array)
    (tvs: Tvars.t)
    (ft:t)
    : string =
  (* Convert the term [t] in an environment with the named variables [names]
      to a string.
   *)
  let anon_symbol (i:int) (nanon:int): int =
      ST.symbol ("$" ^ (string_of_int (nanon+i)))
  in
  let rec to_string
      (t:term)
      (names: int array)
      (nanonused: int)
      (tvs:Tvars.t)
      (is_fun: bool)
      (outop: (operator*bool) option)
      : string =
    (* nanonused: number of already used anonymous variables
       is_fun: the term is used in a function position
       outop: the optional outer operator and a flag if the current term
              is the left operand of the outer operator
     *)
    let nnames = Array.length names
    and anon2sym (i:int): int = anon_symbol i nanonused
    in
    let find_op_int (i:int): operator * int=
      if nnames <= i then
        let idx = i - nnames in
        match (Seq.elem idx ft.seq).fname with
          FNoperator op -> op, idx
        | _ -> raise Not_found
      else
        raise Not_found
    in
    let var2str (i:int): string =
      if i < nnames then
        ST.string names.(i)
      else
        feature_name_to_string
          (Seq.elem (i-nnames) ft.seq).fname
    and find_op (f:term): operator * int =
      match f with
        Variable i -> find_op_int i
      | _ -> raise Not_found
    and args2str (n:int) (nms:int array): string =
      let nnms  = Array.length nms in
      assert (nnms = n);
      let argsstr = Array.init n (fun i -> ST.string nms.(i)) in
      String.concat "," (Array.to_list argsstr)
    in
    let local_names (n:int) (nms:int array): int * int array =
      let nnms  = Array.length nms in
      if n = nnms then
        nanonused, nms
      else
        nanonused+n, Array.init n anon2sym
    in
    let lam_strs (n:int) (nms:int array) (ps:term list) (t:term)
        : string * string *string =
      let nanonused, nms = local_names n nms in
      let names = Term.prepend_names nms names in
      args2str n nms,
      String.concat ";"
        (List.map (fun t -> to_string t names nanonused tvs false None) ps),
      to_string t names nanonused tvs false None
    in
    let argsstr (args: term array): string =
      String.concat
        ","
        (List.map
           (fun t -> to_string t names nanonused tvs false None)
           (Array.to_list args))
    in
    let fapp2str
        (f:term) (args:term array) (inop:bool): operator option * string =
      let argsstr = argsstr args in
      if inop then
        let argstr =
          to_string args.(0) names nanonused tvs false (Some (Inop,true))
        and fstr =
          to_string f names nanonused tvs false (Some (Inop,false))
        in
        Some Inop,
        argstr ^ " in " ^ fstr
      else
        None,
        (to_string f names nanonused tvs true (Some (Parenop,true))) ^
        "(" ^ argsstr ^ ")"
    in
    let ooapp2str (i:int) (args:term array): operator option * string =
      assert (nnames <= i);
      let fstr =
        match (descriptor (i-nnames) ft).fname with
          FNname fsym -> ST.string fsym
        | _ -> assert false
      in
      let len = Array.length args in
      assert (0 < len);
      let tgtstr =
        to_string args.(0) names nanonused tvs false (Some (Dotop,true))
      in
      Some Dotop,
      tgtstr ^ "." ^ fstr ^
      (if len = 1 then
        ""
      else
        let args = Array.sub args 1  (len - 1) in
        "(" ^ (argsstr args) ^ ")")
    in
    let funiapp2str (i:int) (args:term array): operator option * string =
      assert (nnames <= i);
      let fn = (descriptor (i-nnames) ft).fname in
      match fn with
        FNname fsym ->
          if fsym = (ST.symbol "singleton") then
            None, "{" ^ argsstr args ^ "}"
          else if fsym = ST.tuple then
            Some Commaop, (argsstr args)
          else
            None, ST.string fsym ^ "(" ^ argsstr args ^ ")"
      | _ ->
          assert false;
    in
    let op2str (op:operator) (fidx:int) (args: term array): string =
      match op with
        Allop | Someop | Asop -> assert false (* cannot happen *)
      | Bracketop when arity fidx ft > 1 ->
          let nargs = Array.length args in
          assert (nargs = arity fidx ft);
          let tgt = args.(0)
          and args = Array.sub args 1 (nargs - 1) in
          let tgtstr = to_string tgt names nanonused tvs false None in
          tgtstr ^ "[" ^ (argsstr args) ^ "]"
      | _ ->
          let nargs = Array.length args in
          if nargs = 1 && arity fidx ft = 0 then begin
            assert (is_higher_order fidx ft);
            let nargs_tup = tuple_arity fidx ft in
            assert (1 <= nargs_tup);
            if nargs_tup = 1 then
              "(" ^ (operator_to_rawstring op) ^ ")"
              ^ (to_string args.(0) names nanonused tvs false (Some (op,false)))
            else
              let tup_tp = fake_tuple_type nnames in
              let args = args_of_tuple_ext args.(0) tup_tp nnames 2 ft in
              (to_string args.(0) names nanonused tvs false (Some (op,true)))
              ^ " " ^ (operator_to_rawstring op) ^ " "
              ^ (to_string args.(1) names nanonused tvs false (Some (op,false)))
          end else if nargs = 1 then
            (operator_to_rawstring op) ^ " "
            ^ (to_string args.(0) names nanonused tvs false (Some (op,false)))
          else begin
            assert (nargs=2); (* only unary and binary operators *)
            (to_string args.(0) names nanonused tvs false (Some (op,true)))
            ^ " " ^ (operator_to_rawstring op) ^ " "
            ^ (to_string args.(1) names nanonused tvs false (Some (op,false)))
          end
    and lam2str (n:int) (nms: int array) (pres:term list) (t:term) (pr:bool)
        : string =
      let argsstr, presstr, tstr = lam_strs n nms pres t in
      if pr then
        "{" ^ argsstr ^ ": " ^ tstr ^ "}"
      else
        match pres with
          [] -> "((" ^ argsstr ^ ") -> " ^ tstr ^ ")"
        | _ -> "(agent (" ^ argsstr ^ ") require " ^
            presstr ^
            " ensure -> " ^ tstr ^ " end)"
    and if2str (cond:term) (a:term) (b:term): string =
      let to_str t = to_string t names nanonused tvs false None in
      "(if " ^ to_str cond ^ " then " ^ to_str a ^ " else " ^ to_str b ^ ")"
    and insp2str insp cases =
      "(inspect "
      ^ to_string insp names nanonused tvs false None
      ^ Array.fold_left
          (fun str (fs,pat,res) ->
            let nms1 = Term.prepend_names (Array2.first fs) names in
            let to_str t = to_string t nms1 nanonused tvs false None in
            str ^ " case " ^ to_str pat ^  " then " ^ to_str res
          )
          ""
          cases
      ^ ")"
    and as2str insp tps pat =
      to_string insp names nanonused tvs false (Some (Asop,true))
      ^ " as "
      ^ let n = Array.length tps in
        let nms = anon_argnames n in
        let names1 = Array.append nms names in
        to_string pat names1 nanonused tvs false (Some (Asop,false))
    in
    let inop, str =
      match t with
        Variable i ->
          None, var2str i
      | VAppl (i,args,_,oo) ->
          if Array.length args = 0 then
            None, var2str i
          else
            begin try
              let op,fidx = find_op_int i in
              Some op, op2str op fidx args
            with Not_found ->
              if oo then
                ooapp2str i args
              else
                funiapp2str i args
            end
      | Application (f,args,inop) ->
          begin
            try
              let op,fidx = find_op f in
              Some op, op2str op fidx args
            with Not_found ->
              fapp2str f args inop
          end
      | Lam (tps,fgs,pres,t,rt) ->
         assert (Formals.count fgs = 0);
         let n = Formals.count tps
         and nms = Formals.names tps
         and pr = (rt = None)
         in
         None, lam2str n nms pres t pr
      | QExp (tps,fgs,t,is_all) ->
         let n = Formals.count tps
         and nms = Formals.names tps
         and tps = Formals.types tps
         and fgnms = Formals.names fgs
         and fgcon = Formals.types fgs in
          let op, opstr  = if is_all then Allop, "all"  else Someop, "some"
          in
          if not long && nanonused + Array.length names <> 0 then
            let argsstr, _, tstr = lam_strs n nms [] t in
            Some op, opstr ^ "(" ^ argsstr ^ ") " ^ tstr
          else
            begin
              assert (Tvars.has_no_variables tvs || Array.length fgcon = 0);
              let names = Term.prepend_names nms names
              and nfgs  = Array.length fgnms
              and tvs1 =
                assert (Tvars.count tvs = 0 || Array.length fgcon = 0);
                if Tvars.count tvs = 0 then
                  Tvars.push_fgs (Formals.make fgnms fgcon) tvs
                else
                  tvs
              in
              let fgsstr  = Class_table.string_of_inner_fgs nfgs tvs1 ft.ct
              and argsstr = Class_table.arguments_string2 tvs1 nms tps ft.ct
              and tstr = to_string t names nanonused tvs1 false None
              in
              Some op, opstr ^ fgsstr ^ argsstr ^ " " ^ tstr
            end
      | Ifexp (cond,a,b) ->
         None, if2str cond a b
      | Asexp (insp,tps,pat) ->
         Some(Asop), as2str insp tps pat
      | Inspect (insp,cases) ->
         None, insp2str insp cases
      | Indset (nme,tp,rs) ->
          let n,nms = 1, [|nme|] in
          let argsstr = args2str n nms in
          let nanonused, nms = local_names n nms in
          let names = Term.prepend_names nms names in
          let rsstrs =
            List.map (fun t -> to_string t names nanonused tvs false None)
              (Array.to_list rs) in
          let rsstr = String.concat "," rsstrs in
          let str = "{(" ^ argsstr ^ "):"
            ^  rsstr ^ "}" in
          None, str
    in
    match inop, outop with
      Some iop, Some (oop,is_left) ->
        let _,iprec,iassoc = operator_data iop
        and _,oprec,oassoc = operator_data oop
        in
        let paren1 = iprec < oprec
        and paren2 = (iop = oop) &&
          match oassoc with
            Left     -> not is_left
          | Right    -> is_left
          | Nonassoc -> true
        and paren3 = (iprec = oprec) && (iop <> oop)
        in
        if  paren1 || paren2 || paren3 then
          "(" ^ str ^ ")"
        else
          str
    | _ -> str
  in
  let names =
    Term.prepend_names
      (anon_argnames nanon)
      names
  (*let nnames = Array.length names in
  let names = Array.init (nnames + nanon)
      (fun i ->
        if i<nanon then anon_symbol i 0
        else names.(i-nanon))*)
  in
  to_string t names nanon tvs false None


let string_of_term_anon (t:term) (nb:int) (ft:t): string =
  term_to_string t true false nb [||] (Tvars.empty) ft

let string_long_of_term_anon (t:term) (nb:int) (tvs:Tvars.t) (ft:t): string =
  term_to_string t true true nb [||] tvs ft




let count_fgs (i:int) (ft:t): int =
  assert (i < count ft);
  Tvars.count_fgs (descriptor i ft).tvs


let seed (i:int) (ft:t): int =
  assert (i < count ft);
  (base_descriptor i ft)#seed






let variant (i:int) (classes:int array) (ft:t): int =
  (* The variant of the feature [i] in the class [cls] *)
  assert (i < count ft);
  let bdesc = base_descriptor i ft in
  let seed_bdesc = base_descriptor bdesc#seed ft
  in
  seed_bdesc#variant classes




let variant_generics
    (idx_var:int) (idx:int) (ags:agens) (tvs:Tvars.t) (ft:t): agens =
  (* [idx_var] is a variant of the feature [idx]. We consider a feature call
     of the form [VAppl(idx,_,ags)] where [ags] are from the type environment
     [tvs]. The routine calculates the actual generics of the call of the
     variant feature [VAppl(idx_var,_,ags_var)].
   *)
  let desc     = descriptor idx ft
  and desc_var = descriptor idx_var ft
  in
  let nfgs_var = Tvars.count_fgs desc_var.tvs in
  assert (Tvars.has_no_variables desc_var.tvs);
  assert (Tvars.has_no_variables desc.tvs);
  assert (Tvars.has_no_variables tvs);
  if nfgs_var = 0 then
    [||]
  else
    let idx_tp_substituted = Term.subst desc.tp (Tvars.count_fgs tvs) ags in
    try
      Type_substitution.make_equal
        desc_var.tp desc_var.tvs
        idx_tp_substituted tvs
        ft.ct
    with Not_found ->
      assert false (* must be unifiable *)
      (*
    begin
      let idx_tp_substituted = Term.subst desc.tp (Tvars.count_fgs tvs) ags in
      let open Type_substitution in
      let sub = make nfgs_var desc_var.tvs tvs ft.ct in
      begin
        try
          unify desc_var.tp idx_tp_substituted sub
        with Reject ->
          assert false (* must be unifiable *)
      end;
      array nfgs_var sub
    end*)


let variant_feature
    (i:int) (nb:int) (ags:type_term array) (tvs:Tvars.t) (ft:t): int*agens =
  (* The variant of the feature [i] in a context with [nb] variables where the
     formal generics are substituted by the actual generics [ags] which come from
     the type environment [tvs].

     Return the feature index of the variant and the actual generics which
     substitute the formal generics of the variant feature.
   *)
  assert (i < nb + count ft);
  assert (Tvars.has_no_variables tvs);
  let idx = i - nb in
  if Array.length ags <> count_fgs idx ft then begin
    printf "variant_feature %d %s\n" idx (string_of_signature idx ft);
    printf "#ags %d, count_fgs %d\n" (Array.length ags) (count_fgs idx ft);
  end;
  assert (Array.length ags = count_fgs idx ft);
  let nfgs = Array.length ags in
  if nfgs = 0 then
    i,ags
  else begin (* variant has to be found via the common seed *)
    let bdesc = base_descriptor idx ft in
    let sd,ags_sd = bdesc#seed, bdesc#ags
    and nall = Tvars.count_all tvs in
    let ags1 = (* ags transformed into actual generics for the seed *)
      if sd = i then
        ags
      else
        Term.subst_array ags_sd nall ags
    in
    let classes = Array.map (fun tp -> Tvars.principal_class tp tvs) ags1 in
    try
      let idx_var = variant sd classes ft in
      let ags_var = variant_generics idx_var idx ags tvs ft in
      if not (Array.for_all (fun tp -> tp <> empty_term) ags_var)
      then
        begin
          printf "base    %d %s\n" idx (string_of_signature idx ft);
          printf "variant %d %s\n" idx_var (string_of_signature idx_var ft);
          printf "tvs %s, ags %s\n"
                 (Class_table.string_of_tvs tvs ft.ct)
                 (Class_table.string_of_type_arr ags tvs ft.ct)
        end;
      assert (Array.for_all (fun tp -> tp <> empty_term) ags_var);
      nb + idx_var, ags_var
    with Not_found ->
      i,ags
  end


let substituted
    (t:term) (nargs:int) (nbenv:int) (ntvs:int)
    (args:arguments) (d:int)
    (ags:agens) (tvs:Tvars.t)
    (ft:t)
    : term =
  (* The term [t] has [nargs] arguments below [nbenv] variables. Furthermore
     there are formal generics or [ntvs] type variables (locals+generics!).

     The first arguments will be substituted by [args] coming from an
     enviroment with [d] variables more than [t].

     The formal generics will be substituted by [ags] coming from the type
     environment [tvs]. All feature calls will be specialized according to the
     actual generics.

     Note: All formal generics will be substituted.

     Note: The presence of type variables and formal generics is mutually
           exclusive.
   *)
  assert (Tvars.has_no_variables tvs); (* not usable during typing *)
  let len    = Array.length args
  and is_gen = (0 < Array.length ags)
  and nall   = Tvars.count_all tvs in
  assert (not is_gen || ntvs = 0);       (* mutually exclusive *)
  let subtp tp = Term.subst tp (nall-ntvs) ags in
  assert (len <= nargs);
  let rec spec nb t =
    let spec_args nb args = Array.map (fun t -> spec nb t) args
    and spec_lst  nb lst  = List.map  (fun t -> spec nb t) lst
    in
    match t with
      Variable i when i < nb ->
        t
    | Variable i when i < nb + len ->
        Term.up (nb+nargs-len) args.(i-nb)
    | Variable i when i < nb + nargs ->
        Variable (i - len)
    | Variable i ->
        Variable (i - len + d)
    | VAppl(i0,args,ags0,oo) ->
       assert (Array.for_all (fun tp -> tp <> empty_term) ags0);
        let ags0 = Array.map subtp ags0
        and args   = spec_args nb args in
       assert (Array.for_all (fun tp -> tp <> empty_term) ags0);
        let i,ags0 =
          if is_gen then
            variant_feature i0 (nb+nargs+nbenv) ags0 tvs ft
          else
            i0,ags0
        in
        assert (Array.for_all (fun tp -> tp <> empty_term) ags0);
        let i = i - len + d in
        VAppl (i,args,ags0,oo)
    | Application (f,args,inop) ->
        let f = spec nb f
        and args = spec_args nb args in
        Application (f,args,inop)
    | Lam (tps,fgs,ps,t0,rt) ->
       assert (Formals.count fgs = 0);
       let nb = Formals.count tps + nb in
       Lam(Formals.map subtp tps,
           Formals.map subtp fgs,
           spec_lst nb ps,
           spec nb t0,
           Option.map subtp rt)
    | QExp (tps,fgs,t0,is_all) ->
       assert (fgs = Formals.empty);
       let n = Formals.count tps in
       let nb = n + nb in
       let t0 = spec nb t0 in
        QExp (Formals.map subtp tps, fgs, t0, is_all)
    | Ifexp (cond, a, b) ->
       Ifexp(spec nb cond, spec nb a, spec nb b)
    | Asexp (insp, tps, pat) ->
       Asexp(spec nb insp,
              Array.map subtp tps,
              let nb = nb + Array.length tps in
              spec nb pat)
    | Inspect(insp, cases) ->
       Inspect(spec nb insp,
               Array.map
                 (fun (fs,pat,res) ->
                   let nb = nb + Array2.count fs in
                   (Array2.make
                      (Array2.first fs)
                      (Array.map subtp (Array2.second fs))),
                   spec nb pat,
                   spec nb res)
                 cases)
    | Indset (nme,tp,rs) ->
        let nb = 1 + nb in
        let rs = spec_args nb rs
        and tp = subtp tp in
        Indset (nme,tp,rs)
  in
  spec 0 t



let specialized (t:term) (nb:int) (tvs:Tvars.t) (ft:t)
    : term =
  let rec spec nb t =
    let spec_args nb args = Array.map (fun t -> spec nb t) args
    and spec_lst  nb lst  = List.map  (fun t -> spec nb t) lst
    in
    match t with
      Variable _ -> t
    | VAppl(i0,args,ags,oo) ->
        let args  = spec_args nb args
        and i,ags = variant_feature i0 nb ags tvs ft in
        VAppl (i,args,ags,oo)
    | Application (f,args,inop) ->
        let f = spec nb f
        and args = spec_args nb args in
        Application (f,args,inop)
    | Lam (tps,fgs,ps,t0,rt) ->
        let nb = Formals.count tps + nb in
        let ps = spec_lst nb ps
        and t0 = spec nb t0 in
        Lam (tps,fgs,ps,t0,rt)
    | QExp (tps,fgs,t0,is_all) ->
       assert (fgs = Formals.empty);
       let nb = Formals.count tps + nb in
       let t0 = spec nb t0 in
       QExp (tps,fgs,t0,is_all)
    | Ifexp(cond, a, b) ->
       Ifexp(spec nb cond, spec nb a, spec nb b)
    | Asexp(insp, tps, pat) ->
       Asexp(spec nb insp,
             tps,
             spec (nb + Array.length tps) pat)
    | Inspect(insp,cases) ->
       Inspect(spec nb insp,
               Array.map
                 (fun (fs,pat,res) ->
                   let nb = nb + Array2.count fs in
                   fs, spec nb pat, spec nb res)
                 cases)
    | Indset (nme,tp,rs) ->
        let nb = 1 + nb in
        let rs = spec_args nb rs in
        Indset (nme,tp,rs)
  in
  spec nb t


let equality_term
    (a:term) (b:term) (nb:int) (tp:type_term) (tvs:Tvars.t) (ft:t)
    : term =
  (* The term [a = b] where [a] and [b] have the type [tp] in the type environment
     [tvs] and are valid in an environment with [nb] variables.

     Note: '=' in the class ANY has the signature [A:ANY](x,y:A):BOOLEAN
   *)
  let args0 = standard_substitution 2
  and ags0  = standard_substitution 1 in
  let eq0 = VAppl(2 + Constants.eq_index, args0, ags0,false) in
  substituted eq0 2 0 0 [|a;b|] nb [|tp|] tvs ft


let evaluated_as_expression
    (t:term)
    (nb:int)
    (tvs:Tvars.t)
    (ft:t)
    : term =
  (* The as expression [t as cons(...)] has the evaluation

         some(a,b,c,...) t = cons(...)
   *)
  let eq_term c n t0 args ags =
    let ntvs = Tvars.count_all tvs in
    let tp = inductive_type (c-nb-n) ags ntvs ft in
    let t0 = Term.up n t0 in
    equality_term t0 (VAppl(c,args,ags,false)) (nb+n) tp tvs ft
  in
  match t with
  | Asexp (insp, tps, VAppl(co,args,ags,_)) ->
     let n = Array.length tps in
     let eq = eq_term co n insp args ags in
     let nms = standard_argnames n in
     Term.some_quantified (Formals.make nms tps) eq
  | _ ->
     assert false (* not an as expression *)




let implication (a:term) (b:term) (nb:int): term =
  (* The implication [a ==> b] where [a] and [b] are valid in an environment
     with [nb] variables.
   *)
  VAppl(nb+Constants.implication_index, [|a;b|], [||],false)



let raw_definition_term (i:int) (ft:t): term =
  assert (i < count ft);
  Feature.Spec.definition_term (base_descriptor i ft)#specification


let definition_term (i:int) (nb:int) (ags:agens) (tvs:Tvars.t) (ft:t)
    : int * int array * term =
  (* The definition term of the feature [i-nb] transformed into an environment with
     [nb] variables, the formal generics substituted by [ags] which come from the
     type context [tvs] and the inner functions properly specialized *)
  assert (nb <= i);
  assert (i  <= nb + count ft);
  let idx = i - nb in
  let t0 = raw_definition_term idx ft in
  let desc = descriptor idx ft in
  let nargs = arity idx ft in
  let t = substituted t0 nargs 0 0 [||] nb ags tvs ft in
  nargs, desc.argnames, t


let has_no_definition (i:int) (ft:t): bool =
  Feature.Spec.has_no_definition (base_descriptor i ft)#specification


(* Really necessary?? *)
let expanded_definition
    (i:int) (nb:int) (args:arguments) (ags:agens) (tvs:Tvars.t) (ft:t)
    : term =
  (* The definition of the feature [i-nb] expanded with the arguments [args]
     and the actual generics [ags] which come from the type context [tvs].

     The term to be expanded is [VAppl(i, args, ags)].
   *)
  assert (nb <= i);
  assert (i  <= nb + count ft);
  let idx = i - nb in
  let t0 = raw_definition_term idx ft in
  let nargs = Array.length args in
  assert (nargs = arity idx ft);
  substituted t0 nargs 0 0 args nb ags tvs ft



let has_definition_term (i:int) (ft:t): bool =
  try
    ignore (raw_definition_term i ft); true
  with Not_found ->
    false



let is_inductive_set (i:int) (nb:int) (ft:t): bool =
  (* Does the feature [i] in an environment with [nb] bound variables represent
     an inductively defined set`*)
  assert (nb <= i);
  try
    let t = raw_definition_term (i-nb) ft in
    begin
      match t with
        Indset _ -> true
      | _        -> false
    end
  with Not_found ->
    false


let inductive_set (i:int) (args:term array) (ags:agens) (nb:int) (tvs:Tvars.t) (ft:t)
    : term =
  if is_inductive_set i nb ft then
    expanded_definition i nb args ags tvs ft
  else
    raise Not_found


let preconditions (i:int) (nb:int) (ft:t): int * int array * term list =
  (* The preconditions (if there are some) of the feature [i] as optional number of
     arguments and a list of expressions transformed into an environment with [nb]
     bound variables.*)
  assert (nb <= i);
  let i = i - nb in
  let desc = descriptor i ft in
  let spec = (base_descriptor i ft)#specification in
  let n = arity i ft in
  if Feature.Spec.has_preconditions spec then
    let lst = Feature.Spec.preconditions spec in
    let lst = List.map (fun t -> Term.up_from nb n t) lst in
    n, desc.argnames, lst
  else
    n, desc.argnames, []



let postconditions (i:int) (nb:int) (ft:t): int * int array * term list =
  assert (nb <= i);
  let i = i - nb in
  let desc = descriptor i ft in
  let spec = (base_descriptor i ft)#specification in
  let n = arity i ft in
  let lst = Feature.Spec.postconditions spec in
  let lst = List.map (fun t -> Term.up_from nb n t) lst in
  n, desc.argnames, lst



let count_postconditions (i:int) (ft:t): int =
  let bdesc = base_descriptor i ft in
  Feature.Spec.count_postconditions bdesc#specification

let function_property_assertions (idx:int) (ft:t): term list =
  (* Get the list of assertions

        all[fgs](tps) r1 ==> r2 ==> ... ==> ei

     of a function with the specification

        f (a:A,b:B,...): RT
            require
                r1; r2; ...
            ensure
                e1(Result)
                e2(Result)
                ...
            end

     where [Result] has been replaced by [f(a,b,...)].
   *)
  let desc  = descriptor idx ft in
  let spec  = desc.bdesc#specification in
  let pres  = List.rev (Feature.Spec.preconditions spec)
  and posts = Feature.Spec.postconditions spec
  and nargs = Sign.arity desc.sign
  and tps   = Sign.arguments desc.sign
  and fgnms = Tvars.fgnames desc.tvs
  and fgcon = Tvars.fgconcepts desc.tvs
  in
  List.map
    (fun e ->
      let chn =
        Term.make_implication_chain pres e (nargs + Constants.implication_index)
      in
      let t =
        Term.all_quantified
          (Formals.make desc.argnames tps) (Formals.make fgnms fgcon)
          chn
      in
      Term.prenex t 0 0 Constants.implication_index
    )
    posts



let function_property
    (i:int) (fidx:int) (nb:int) (args:term array) (ft:t): term =
  assert (nb <= fidx);
  assert false (* nyi *)
  (*let fidx = fidx - nb in
  let spec = (base_descriptor fidx ft).spec in
  let n = arity fidx ft in
  if n <> Array.length args then invalid_arg "wrong size of arguments";
  if i < 0 || count_postconditions fidx ft <= i then
    invalid_arg "postcondition does not exist";
  let pres  = Feature.Spec.preconditions spec
  and post  = Feature.Spec.postcondition i spec in
  let chn =
    Term.make_implication_chain (List.rev pres) post (implication_index + n) in
  Term.sub chn args nb*)




let domain_of_feature (i:int) (nb:int) (ags:agens) (tvs:Tvars.t) (ft:t): term =
  (* The domain of the feature [i] in a context with [nb] variables and the
     formal generics of the feature [i] substituted by the actual generics [ags]
     coming from the type environment [tvs].
   *)
  assert (nb <= i);
  assert (arity (i-nb) ft > 0);
  let n,nms,pres = preconditions i nb ft
  and nall = Tvars.count_all tvs
  in
  let subst t =
    substituted t 0 (n+nb) 0 [||] 0 ags tvs ft
  in
  let s = signature (i-nb) ags nall ft in
  let tps = Formals.make nms (Sign.arguments s) in
  let t =
    match pres with
      [] ->
        true_constant (n+nb)
    | hd::tl ->
        let and_id = n + nb + Constants.and_index in
        List.fold_left
          (fun t1 t2 -> Term.binary and_id (subst t1) (subst t2))
          hd
          tl
  in
  make_lambda tps Formals.empty [] t None nb nall ft




let body (i:int) (ft:t): Feature.body =
  assert (i < count ft);
  let desc = descriptor i ft
  and bdesc = base_descriptor i ft in
  bdesc#specification, desc.impl


let is_ghost_term (t:term) (nargs:int) (ft:t): bool =
  let rec is_ghost (t:term) (nb:int): bool =
    let rec ghost_args (args:term array) (i:int) (n:int): bool =
      if i = n then false
      else
        let ghost = is_ghost args.(i) nb in
        ghost || ghost_args args (i+1) n
    in
    match t with
      Variable i when i < nb+nargs -> false
    | Variable i ->
        is_ghost_function (i-nb-nargs) ft
    | Lam (tps,_,_,t,_) ->
        is_ghost t (Formals.count tps + nb)
    | QExp _ ->
        true
    | Ifexp(cond, a, b) ->
       is_ghost cond nb || is_ghost a nb || is_ghost b nb
    | Asexp (insp,tps,pat) ->
       is_ghost insp nb
    | Inspect(insp,cases) ->
       is_ghost insp nb
       || Array.exists
            (fun (fs,pat,res) ->
              is_ghost res (nb + Array2.count fs)
            )
            cases
    | Indset _ ->
        true
    | VAppl (i,args,_,_) ->
        let ghost = is_ghost_function (i-nb-nargs) ft in
        ghost || ghost_args args 0 (Array.length args)
    | Application (f,args,_) ->
        let fghost = is_ghost f nb in
        fghost || ghost_args args 0 (Array.length args)
  in
  is_ghost t 0


let is_ghost_specification (spec:Feature.Spec.t) (ft:t): bool =
  Feature.Spec.has_postconditions spec ||
  (Feature.Spec.has_definition_term spec &&
   let nargs = Feature.Spec.count_arguments spec in
   is_ghost_term (Feature.Spec.definition_term spec) nargs ft)


let is_total (i:int) (ft:t): bool =
  assert (i < count ft);
  true  (* nyi: features with preconditions *)

let is_predicate (i:int) (ft:t): bool =
  let desc = descriptor i ft in
  let sign = desc.sign
  and tvs  = desc.tvs in
  let nfgs = Tvars.count_all tvs in
  0 < Sign.arity sign &&
  is_total i ft &&
  Sign.has_result sign &&
  let res = Sign.result sign in
  match res with
    Variable i when nfgs <= i ->
      i - nfgs = Constants.boolean_class
  | _ ->
      false



let seed_function (ft:t): int -> int =
  fun i -> seed i ft




let rec complexity (t:term) (nbenv:int) (tvs:Tvars.t) (ft:t): int =
  (* The complexity (i.e. node count) of the term [t] with all functions fully
     expanded. The term [t] comes from an environment with
     [nb] variables and its types are valid in the type environment [tvs].
   *)
  complexity_base t 0 [||] nbenv tvs ft

and complexity_base
    (t:term) (nb:int) (cargs:int array) (nbenv:int) (tvs:Tvars.t) (ft:t)
    : int =
  (* The term [t] has [nb] bound variables below [#cargs] arguments below
     [nbenv] variables coming from the type environment [tvs].

     Compute the complexity (i.e. node count) of the term [t] with all
     functions fully expanded and all arguments substituted by term with a
     node count corresponding to the array [cargs].
   *)

  let compl t = complexity_base t nb cargs nbenv tvs ft in
  let nargs = Array.length cargs in
  match t with
    Variable i when nb <= i && i < nb + nargs ->
      cargs.(i - nb)
  | Variable i ->
      assert (i < nb || nb + nargs <= i);
      assert (i < nb + nargs + nbenv);
      1
  | VAppl (i,args,ags,_) ->
      assert (nb + nargs + nbenv <= i);
      let cargs = Array.map compl args in
      feature_complexity (i - nb - nargs - nbenv) cargs ags tvs ft
  | Application (f,args,_) ->
      let cf = compl f
      and cargs = Array.map compl args in
      Myarray.sum cf cargs
  | Lam (tps,_,ps,t0,_) ->
      1 + complexity_base t0 (Formals.count tps + nb) cargs nbenv tvs ft
  | QExp (tps,fgs,t0,is_all) ->
      1 + complexity_base t0 (Formals.count tps + nb) cargs nbenv tvs ft
  | Ifexp _ | Asexp _ | Inspect _ | Indset _ ->
      Term.nodes0 t nb cargs

and feature_complexity
    (i:int) (cargs:int array) (ags:agens) (tvs:Tvars.t) (ft:t)
    : int =
  (* The complexity of the feature call [VAppl(i,args,ags)] where [cargs]
     contains the complexity of the arguments. The formal generics are substituted
     by the actual generics coming from the type environment [tvs].
   *)
  let desc = descriptor i ft in
  try
    let t0 = Feature.Spec.definition_term desc.bdesc#specification in
    let nargs = Array.length cargs
    and ari   = Sign.arity desc.sign in
    let t = substituted t0  0 ari 0 [||] 0 ags tvs ft in
    if nargs = ari then
      complexity_base t 0 cargs 0 tvs ft
    else begin
      assert (nargs = 0);
      complexity t ari tvs ft
    end
  with Not_found ->
    Myarray.sum 1 cargs




let equality_index (cls:int) (ft:t): int =
  variant Constants.eq_index [|cls|] ft


let equality_index_of_type (tp:term) (tvs:Tvars.t) (ft:t): int =
  let cls = Tvars.principal_class tp tvs in
  equality_index cls ft



let definition_equality (i:int) (ft:t): term =
  assert (i < count ft);
  assert (has_definition_term i ft);
  let desc  = descriptor i ft
  and bdesc = base_descriptor i ft in
  assert (Sign.has_result desc.sign);
  let nargs = Sign.arity desc.sign
  and nfgs  = Tvars.count_all desc.tvs
  and r_tp  = Sign.result desc.sign
  in
  let f_id  = nargs + i
  in
  let t = Option.value (Feature.Spec.definition_term_opt bdesc#specification) in
  let f =
    if nargs = 0 then
      Variable f_id
    else
      let args = standard_substitution nargs
      and ags  = standard_substitution nfgs in
      VAppl (f_id, args, ags, false)
  in
  let eq_id = nargs + Constants.eq_index in
  VAppl (eq_id, [|f;t|], [|r_tp|], false)




let transformed_specifications (i:int) (ivar:int) (ags:agens) (ft:t): term list =
  (* The specification assertions of the feature [i] in the environment of the
     variant feature [ivar] with the actual generics valid in the variant. *)
  let posts =
    if has_definition_term i ft then
      [definition_equality i ft]
    else
      let _,_,posts = postconditions i 0 ft in
      posts
  and n,nms,pres  = preconditions  i 0 ft in
  let pres_rev = List.rev pres in
  let imp_id = n + Constants.implication_index
  and desc_var = descriptor ivar ft in
  let tps = Sign.arguments desc_var.sign
  and fgnms = Tvars.fgnames desc_var.tvs
  and fgcon = Tvars.fgconcepts desc_var.tvs
  in
  List.map
    (fun t ->
      let t1 = Term.make_implication_chain pres_rev t imp_id
      and args = standard_substitution n in
      let t2 = substituted t1 n 0 0 args n ags desc_var.tvs ft in
      QExp((Formals.make nms tps),(Formals.make fgnms fgcon),t2,true)
    )
    posts





let names_of_formals (farr: formal array): int array =
  Array.map (fun (name,_) -> name) farr

let terms_of_formals (farr: formal array): term array =
  Array.map (fun (_,t) -> t) farr



let find_seed_with_signature
    (fn:feature_name withinfo) (idx:int) (tvs: Tvars.t) (sign:Sign.t) (ft:t)
    : int*agens =
  (* Find the seed of the feature with the name [fn] and signature [tvs/sign]
     which is different from the the feature [idx].

     Raise [Not_found] if there is no seed.

     Report an error if there are multiple seeds.
   *)
  let ntvs = Tvars.count_all tvs in
  let tp   = Class_table.to_dummy ntvs sign in
  let tab = Feature_map.find fn.v ft.map in
  let lst  = Term_table.unify0 tp ntvs !tab in
  let lst =
    List.fold_left
      (fun lst (sd,sub) ->
        if sd = idx then
          lst
        else begin
          let desc = descriptor sd ft
          and ags = Term_sub.arguments (Term_sub.count sub) sub in
          let len = Array.length ags in
          assert (Tvars.count_fgs desc.tvs = len);
          assert (Tvars.count     desc.tvs = 0);
          let ok =
            interval_for_all
              (fun i ->
                Class_table.satisfies ags.(i) tvs (Variable i) desc.tvs ft.ct
              )
              0 len
          in
          if ok then
            (sd,ags) :: lst
          else
            lst
        end
      )
      []
      lst
  in
  match lst with
    [] ->
      raise Not_found
  | [idx,ags] ->
      idx, ags
  | _ :: _ ->
      let str =
        "Feature \"" ^ (feature_name_to_string fn.v) ^
        "\" cannot have multiple seeds\nseeds:\n\t"
      in
      let strlst =
        List.map (fun (idx,_) -> string_of_signature idx ft) lst in
      let str = str ^ String.concat "\n\t" strlst in
      error_info fn.i str



let find_with_signature
    (fn:feature_name withinfo) (tvs: Tvars.t) (sign:Sign.t) (ft:t)
    : int =
  (* Find the feature with the name [fn] and the signature [tvs/sign].  *)
  let i,ags_sd = find_seed_with_signature fn (-1) tvs sign ft
  in
  let ivar,ags_var = variant_feature i 0 ags_sd tvs ft
  in
  let desc = descriptor ivar ft in
  if Tvars.is_equivalent tvs desc.tvs && sign = desc.sign then
    ivar
  else
    raise Not_found



let has_with_signature
    (fn:feature_name withinfo) (tvs: Tvars.t) (sign:Sign.t) (ft:t): bool =
  try
    let _ = find_with_signature fn tvs sign ft in true
  with Not_found -> false



let find_proper_seed (info:info) (idx:int) (ft:t): int*agens =
  let desc = descriptor idx ft in
  let fn = withinfo info desc.fname in
  find_seed_with_signature fn idx desc.tvs desc.sign ft




let add_class_feature (i:int) (ft:t)
    : unit =
  (* Add the feature [i] as a class feature to the corresponding owner
     class and to the anchor class. *)
  assert (i < count ft);
  let desc  = descriptor i ft in
  Class_table.add_generics i false desc.tvs ft.ct


let add_key (i:int) (ft:t): unit =
  (* Add the key of the feature [i] to the key table. *)
  assert (i < count ft);
  let desc  = descriptor i ft in
  let ntvs  = Tvars.count_all desc.tvs
  in
  desc.tp <- Class_table.to_dummy ntvs desc.sign;
  let tab =
    try Feature_map.find desc.fname ft.map
    with Not_found ->
      let tab = ref Term_table.empty in
      ft.map <- Feature_map.add desc.fname tab ft.map;
      tab
  in
  tab := Term_table.add0 desc.tp ntvs 0 i !tab



let remove_key (i:int) (ft:t): unit =
  (* Remove the key of the feature [i] from the key table. *)
  let tab =
    try
      Feature_map.find (descriptor i ft).fname ft.map
    with Not_found ->
      assert false (* cannot happen *)
  in
  tab := Term_table.remove i !tab




let add_keys (ft:t): unit =
  for i = 0 to (count ft)-1 do
    add_key i ft
  done


let add_feature
    (fn:       feature_name withinfo)
    (tvs:      Tvars.t)
    (argnames: int array)
    (sign:     Sign.t)
    (impl:     Feature.implementation)
    (ft:       t): unit =
  (* Add a new feature to the feature table with an empty specification *)
  assert (not (has_with_signature fn tvs sign ft));
  let cnt     = Seq.count ft.seq
  and spec    = Feature.Spec.make_empty argnames
  and nfgs    = Tvars.count_all tvs
  in
  assert (not (Feature.Spec.has_definition_term spec));
  assert (Tvars.count tvs = 0);
  let dominant_cls = Class_table.dominant_class tvs sign ft.ct in
  let dominant_fg  = Class_table.dominant_formal_generic tvs dominant_cls ft.ct
  in
  let classes =
    Array.map
      (fun tp -> Tvars.principal_class tp tvs)
      (Tvars.fgconcepts tvs)
  in
  if impl = Feature.Deferred then
    Class_table.check_deferred dominant_cls dominant_fg fn.i ft.ct;
  let bdesc = new  bdesc (is_interface_public_use ft) cnt nfgs classes spec
  and nfgs = Tvars.count_all tvs
  in
  let desc =
    {mdl      = current_module ft;
     dominant_cls;
     dominant_fg;
     fname    = fn.v;
     impl;
     tvs;
     argnames;
     sign;
     tp       = Class_table.to_dummy nfgs sign;
     bdesc;
     recognizers = [];
     projectors  = IntMap.empty;
     co_preconditions = [];
     is_constr   = false}
  in
  Seq.push desc ft.seq;
  add_key cnt ft;
  add_class_feature cnt ft;
  if ft.verbosity > 1 then
    printf "  new feature %d %s\n" cnt (string_of_signature cnt ft)



let equality_signature (cls:int) (ft:t): Tvars.t * Sign.t =
  let tp, tvs =  Class_table.class_type cls ft.ct
  in
  let sign =
    Sign.make_func
      [|tp;tp|]
      (Class_table.boolean_type (Tvars.count_all tvs)) in
  tvs, sign



let add_equality (cls:int) (ft:t): unit =
  (* Add an undefined equality function to the class [cls] *)
  let tvs,sign = equality_signature cls ft
  and m       =  Class_table.module_of_class cls ft.ct
  in
  assert (Module.M.equal m (current_module ft));
  let impl =
    if Class_table.is_deferred cls ft.ct then
      Feature.Deferred
    else
      Feature.Builtin
  in
  add_feature
    (withinfo UNKNOWN (FNoperator Eqop))
    tvs
    (standard_argnames 2)
    sign
    impl
    ft



let update_specification (i:int) (spec:Feature.Spec.t) (ft:t): unit =
  assert (i < count ft);
  let bdesc = base_descriptor i ft in
  assert (Feature.Spec.is_empty bdesc#specification);
  assert begin
    not (is_interface_check ft) ||
    Feature.Spec.is_consistent
      (base_descriptor i ft)#specification
      spec
  end;
  bdesc#set_specification spec


let hide_definition (i:int) (ft:t): unit =
  let bdesc = base_descriptor i ft in
  bdesc#set_specification (Feature.Spec.without_definition bdesc#specification)



let export_feature (i:int) (ft:t): unit =
  (* Export the feature [i] unless it is already publicly available. *)
  assert (i < count ft);
  if ft.verbosity > 1 then
    printf "  export feature %d %s\n" i (string_of_signature i ft);
  (descriptor i ft).bdesc#set_exported


let export_equality (cls:int) (ft:t): unit =
  let tvs,sign = equality_signature cls ft
  and fn = withinfo UNKNOWN (FNoperator Eqop) in
  let i = find_with_signature fn tvs sign ft in
  export_feature i ft


let add_base
    (mdl_nme: string)
    (cls: int)
    (fn:feature_name)
    (concepts: type_term array)
    (argtypes: type_term array)
    (res:  type_term)
    (defer: bool)
    (ghost: bool)
    (spec: Feature.Spec.t)
    (ft:t)
    : unit =
  assert (not defer || not (Feature.Spec.has_definition_term spec));
  let mdl_nme            = ST.symbol mdl_nme
  in
  let sign =
    if ghost then
      Sign.make_ghost argtypes res
    else
      Sign.make_func argtypes res
  and ntvs = Array.length concepts
  and cnt  = count ft
  and nargs = Array.length argtypes
  and is_exported = false
  in
  let bdesc = new bdesc is_exported cnt ntvs [|cls|] spec
  in
  let tvs = Tvars.make_fgs (standard_fgnames ntvs) concepts in
  let dominant_cls = Class_table.dominant_class tvs sign ft.ct in
  let dominant_fg  = Class_table.dominant_formal_generic tvs dominant_cls ft.ct
  in
  let lst =
    try IntMap.find mdl_nme ft.base
    with Not_found ->
      let lst = ref [] in
      ft.base <- IntMap.add mdl_nme lst ft.base;
      lst
  and desc = {
    mdl = core_module ft;
    fname    = fn;
    dominant_cls;
    dominant_fg;
    impl     =
    if Feature.Spec.has_definition_term spec then Feature.Empty
    else if defer then Feature.Deferred
    else Feature.Builtin;
    tvs;
    argnames = standard_argnames nargs;
    sign;
    tp       = Class_table.to_dummy ntvs sign;
    bdesc;
    recognizers = [];
    projectors  = IntMap.empty;
    co_preconditions = [];
    is_constr   = false
  }
  in
  Seq.push desc ft.seq;
  lst := cnt :: !lst



let base_table (comp:Module.Compile.t) : t =
  (* Construct a basic table which contains at least implication.  *)
  let bool    = Class_table.boolean_type 0 in
  let ft      = empty comp
  in
  let any1  = Variable (Constants.any_class+1)
  and any2  = Variable (Constants.any_class+2)
  and bool1 = Variable (Constants.boolean_class+1)
  and g_tp  = Variable 0
  and a_tp  = Variable 0
  and b_tp  = Variable 1 in
  let p_tp1 = make_type (Constants.predicate_class+1) [|g_tp|]
  and p_tp2 = make_type (Constants.predicate_class+2) [|a_tp|]
  and f_tp  = make_type (Constants.function_class+2)  [|a_tp;b_tp|]
  and tup_tp= make_type (Constants.tuple_class+2) [|a_tp;b_tp|]
  and spec_none n = Feature.Spec.make_func_def (standard_argnames n) None []
  and spec_term n t = Feature.Spec.make_func_def (standard_argnames n) (Some t) []
  in
  add_base (* ==> *)
    "core" Constants.boolean_class (FNoperator DArrowop)
    [||] [|bool;bool|] bool false false (spec_none 2) ft;

  add_base (* false *)
    "core" Constants.boolean_class FNfalse
    [||] [||] bool false false (spec_none 0) ft;

  let imp_id1   = 1 + Constants.implication_index
  and imp_id2   = 2 + Constants.implication_index
  and not_id2   = 2 + Constants.not_index
  in
  let not_term = Term.binary imp_id1 (Variable 0) (false_constant 1)
  and or_term  =  Term.binary imp_id2 (Term.unary not_id2 (Variable 0)) (Variable 1)
  and and_term =
    Term.unary  not_id2
      (Term.binary imp_id2
         (Variable 0)
         (Term.binary imp_id2 (Variable 1) (false_constant 2)))
  and true_term =
    Term.binary Constants.implication_index (false_constant 0) (false_constant 0)
  in
  add_base (* true *)
    "core" Constants.boolean_class FNtrue
    [||] [||] bool false false (spec_term 0 true_term) ft;

  add_base (* not *)
    "core" Constants.boolean_class (FNoperator Notop)
    [||] [|bool|] bool false false (spec_term 1 not_term) ft;

  add_base (* and *)
    "core" Constants.boolean_class (FNoperator Andop)
    [||] [|bool;bool|] bool false false (spec_term 2 and_term) ft;

  add_base (* or *)
    "core" Constants.boolean_class (FNoperator Orop)
    [||] [|bool;bool|] bool false false (spec_term 2 or_term) ft;

  add_base (* any equality *)
    "core" Constants.any_class (FNoperator Eqop)
    [|any1|] [|g_tp;g_tp|] bool1 true false (spec_none 2) ft;

  add_base (* in *)
    "core" Constants.predicate_class (FNoperator Inop)
    [|any1|] [|g_tp;p_tp1|] bool1 false false (spec_none 2) ft;

  add_base (* domain *)
    "core" Constants.function_class (FNname ST.domain)
    [|any2;any2|] [|f_tp|] p_tp2 false true (spec_none 1) ft;

  add_base (* tuple *)
    "core" Constants.tuple_index (FNname ST.tuple)
    [|any2;any2|] [|a_tp;b_tp|] tup_tp false false (spec_none 2) ft;

  let first_second_term i =
    assert (i < 2);
    let args = standard_substitution 2
    and ags  = standard_substitution 2
    and nms  = standard_argnames 2 in
    let fs = Array2.make nms ags in
    let tup = VAppl(Constants.tuple_index+3, args, ags, false) in
    Inspect(Variable 0, [|fs,tup,Variable i|])
  in
  add_base (* first *)
    "core" Constants.tuple_class (FNname ST.first)
    [|any2;any2|] [|tup_tp|] a_tp false false (spec_term 1 (first_second_term 0)) ft;

  add_base (* second *)
    "core" Constants.tuple_class (FNname ST.second)
    [|any2;any2|] [|tup_tp|] b_tp false false (spec_term 1 (first_second_term 1)) ft;

  assert ((descriptor Constants.implication_index ft).fname = FNoperator DArrowop);
  assert ((descriptor Constants.false_index ft).fname   = FNfalse);
  assert ((descriptor Constants.not_index ft).fname     = FNoperator Notop);
  assert ((descriptor Constants.and_index ft).fname     = FNoperator Andop);
  assert ((descriptor Constants.or_index ft).fname      = FNoperator Orop);
  assert ((descriptor Constants.eq_index ft).fname      = FNoperator Eqop);
  assert ((descriptor Constants.domain_index ft).fname  = FNname ST.domain);
  assert ((descriptor Constants.tuple_index ft).fname   = FNname ST.tuple);
  assert ((descriptor Constants.first_index ft).fname   = FNname ST.first);
  assert ((descriptor Constants.second_index ft).fname  = FNname ST.second);
  ft



let find_features (fn:feature_name) (nvars:int) (ft:t): int list =
  try
    List.fold_left
      (fun lst (i,_,_,_) ->
        if is_feature_visible i ft then
          (i + nvars) :: lst
        else
          lst
      )
      []
      (Term_table.terms !(Feature_map.find fn ft.map))
  with Not_found ->
    []


let find_funcs
    (fn:feature_name)
    (nargs:int) (ft:t)
    : (int * Tvars.t * Sign.t) list =
  let tab = Feature_map.find fn ft.map in
  let lst =
    List.fold_left
      (fun lst (i,_,_,_) ->
        let desc = descriptor i ft in
        let sign = desc.sign in
        let arity = Sign.arity sign
        and tvs   = Tvars.fgs_to_global desc.tvs
        in
        if arity <= nargs || nargs = 0 then
          (i,tvs,sign) :: lst
        else
          lst
      )
      []
      (Term_table.terms !tab)
  in
  if lst = [] then raise Not_found
  else lst


let variant_data (i:int) (k:int) (ft:t): agens =
  (* Tell if [i] is a base feature of [k] i.e. [k] is a variant of [i].
     If no then raise [Not_found].
     If yes then return the actual generics to substitute the formal generics of [i]
     to make the signatures identical.
   *)
  let desc_i = descriptor i ft
  and desc_k = descriptor k ft
  in
  let nfgs_i = Tvars.count_fgs desc_i.tvs
  in
  let open Type_substitution in
  try
    let sub = make nfgs_i desc_i.tvs desc_k.tvs ft.ct in
    unify desc_i.tp desc_k.tp sub;
    array nfgs_i sub
  with Reject ->
    raise Not_found



let find_minimal_variants (i:int) (cls:int) (ft:t): (int*agens) list =
  (* Find the minimal variants of the feature [i] in the variant table of
     [i] or the seed of [i] with at least one class above [cls].

     Return a list [ivar,ags] of the minimal variants so that the signature of
     [i] substituted by the actual generics [ags] yields the signature of
     [ivar]
   *)
  let desc_i = descriptor i ft in
  let sd     = desc_i.bdesc#seed in
  let desc_sd = descriptor sd ft in
  IntArrayMap.fold
    (fun _ ivar lst ->
      if i = ivar then
        lst
      else
        try
          let ags = variant_data i ivar ft in
          let desc_ivar = descriptor ivar ft in
          let above =
            interval_exist
              (fun k ->
                let pcls = Tvars.principal_class ags.(k) desc_ivar.tvs in
                Class_table.has_ancestor pcls cls ft.ct
              )
              0 (Array.length ags) in
          if not above then
            raise Not_found;
          (* [ivar] is a variant of [i], but is it minimal? *)
          let lst,is_min =
            List.fold_left
              (fun (lst,is_min) (ivar0,ags0) ->
                if not is_min then
                  (ivar0,ags0) :: lst, false
                else
                  try
                    ignore(variant_data ivar ivar0 ft);
                    lst, true
                  with Not_found ->
                    (ivar0,ags0) :: lst, false
              )
              ([],true)
              lst
          in
          if is_min then
            (ivar,ags) :: lst
          else
            lst
        with Not_found ->
          lst
    )
    desc_sd.bdesc#variants
    []




let find_unifiables (fn:feature_name) (tp:type_term) (ntvs:int) (ft:t)
    : (int*Term_sub.t) list =
  try
    let tab = Feature_map.find fn ft.map in
    Term_table.unify0_with tp ntvs 0 !tab
  with Not_found ->
    []



let find_new_variants (i:int) (ft:t): (int*agens) list =
  (* Find new variants of the feature [i] in the search tables.

     A new variant has the same name and there is a valid substitution of
     the formal generics of [i] so that the signature of [i] becomes equal to
     the signature of the variant candidate.

     Return a list [ivar,ags] of the new variants so that the signature of [i]
     substituted by the actual generics [ags] yields the signature of [ivar]
   *)
  let desc = descriptor i ft in
  let nfgs = Tvars.count_all desc.tvs in
  List.fold_left
    (fun lst (idx,sub) ->
      assert (nfgs = Term_sub.count sub);
      if i = idx then
        lst
      else
        let desc_idx = descriptor idx ft in
        let valid =
          Term_sub.for_all
            (fun k tp ->
              Class_table.satisfies tp desc_idx.tvs (Variable k) desc.tvs ft.ct
            )
            sub in
        if valid then
          (idx,Term_sub.arguments nfgs sub)::lst
        else
          lst
    )
    []
    (find_unifiables desc.fname desc.tp nfgs ft)


let get_variant_seed (i:int) (ivar:int) (ags:agens) (ft:t): int*agens =
  (* Get the seed of the feature [i] which has variant [ivar] and the actual
     generics [ags] which transform the signature of [i] into the signature of
     [ivar] ([ags] are valid in the environment of [ivar].

     Return the seed of [i] together with the actual generics which transform the
     signature of the seed into the signature of [ivar] *)
  let desc_i = descriptor i ft
  and desc_ivar = descriptor ivar ft in
  let sd, ags0 = desc_i.bdesc#seed, desc_i.bdesc#ags in
  let d = Tvars.count_all desc_ivar.tvs in
  let agssd = Term.subst_array ags0 d ags in
  sd,agssd



let add_variant (info:info) (sd:int) (ivar:int) (ags:agens) (ft:t): unit =
  (* Add to the seed [sd] the variant [ivar] where [ags] are the actual generics
     which substitute the formal generics of [sd] to get the same signature.

     Furthermore remove the variant [ivar] from the key table! Reason: A variant
     shall be found only via its seed.
   *)
  let desc_ivar = descriptor ivar ft in
  let classes = Array.map (fun tp -> Tvars.principal_class tp desc_ivar.tvs) ags
  and bdesc = base_descriptor sd ft
  in
  if bdesc#has_variant classes then begin
    let var_str = string_of_signature ivar ft
    and seed_str = string_of_signature sd ft
    and old_ivar = bdesc#variant classes in
    let old_var_str = string_of_signature old_ivar ft
    in
    let err_str =
      "Illegal variant\n" ^
      "The feature\n\n\t" ^ var_str ^
      "\n\ncannot be a variant of\n\n\t" ^ seed_str ^
      (if old_ivar <> sd then
        "\n\nbecause there is already the variant\n\n\t" ^ old_var_str
      else "")
    in
    error_info info err_str
  end;
  assert(not (bdesc#has_variant classes));
  bdesc#add_variant classes ivar;
  remove_key ivar ft



let set_seed (sd:int) (ivar:int) (ags:agens) (ft:t): unit =
  (* Add the seed [sd] to the variant [ivar] where [ags] are the actual generics
     which substitute the formal generics of [sd] the get the same signature.
   *)
  (base_descriptor ivar ft)#set_seed sd ags


let has_recognizer (exp:term) (gh_reco:term) (idx:int) (ft:t): bool =
  try
    let eq t1 t2 = Term.equivalent t1 t2 in
    ignore(List.find
             (fun (e,gr) -> eq e exp && eq  gr gh_reco)
             (descriptor idx ft).recognizers);
    true
  with Not_found ->
    false



let recognizers (idx:int) (ft:t): term list =
  List.map fst (descriptor idx ft).recognizers


let recognizer (idx:int) (ft:t): term =
  (* Return the first recognizer or raise Not_found if there are no recognizers

     A recognizer has only one free variable which can be substituted by the
     expression to be recognized for the constructor [idx]. If the constructor
     [idx] has formal generics these have to be substituted by the actual
     generics.  *)
  match (descriptor idx ft).recognizers with
  | [] ->
     raise Not_found
  | (reco,_) :: _ ->
     reco


let constructor_preconditions (idx:int) (ft:t): term list =
  (descriptor idx ft).co_preconditions


let add_constructor_preconditions (pres:term list) (idx:int) (ft:t): unit =
  (descriptor idx ft).co_preconditions <- pres


let add_recognizer
      (exp:term) (ghost_reco:term) (idx:int) (ft:t)
    : unit =
  if has_recognizer exp ghost_reco idx ft then
    ()
  else
    begin
      let desc = descriptor idx ft in
      desc.recognizers <- (exp,ghost_reco) :: desc.recognizers
    end


let filter_recognizers (ghost_reco:term) (co:int) (ft:t): unit =
  (* Filter out the recognizers which have the ghost recognizer [ghost_reco] of
     constructor [co].

     A ghost recognizer has the form

        some(cargs) cond and x = c(cargs)

     which comes from the corresponding induction law.
   *)
  let desc = descriptor co ft in
  desc.recognizers <-
    List.filter
      (fun (exp,ghost_reco0) -> Term.equivalent ghost_reco ghost_reco0)
      desc.recognizers




let set_projector (proj:int) (ivar:int) (idx:int) (ft:t): unit =
  assert (ivar < arity idx ft);
  let desc = descriptor idx ft in
  if IntMap.mem ivar desc.projectors then
    ()
  else
    desc.projectors <- IntMap.add ivar proj desc.projectors


let has_all_projectors (idx:int) (ft:t): bool =
  IntMap.cardinal (descriptor idx ft).projectors
  = arity idx ft


let projectors (idx:int) (ft:t): int array =
  let map = (descriptor idx ft).projectors in
  Array.init
    (arity idx ft)
    (fun i ->
      try
        IntMap.find i map
      with Not_found ->
        assert false (* Illegal call, not all projectors available *)
    )


let is_equality_index (idx:int) (ft:t): bool =
  (base_descriptor idx ft)#is_equality



let split_equality (t:term) (nbenv:int) (ft:t): int * int * term * term =
  (* Return [nargs, eq_id, left, right] if the term is an equality. *)
  let nargs, t =
    try
      let tps,_,t0 = Term.all_quantifier_split t in
      let n = Formals.count tps in
      n, t0
    with Not_found ->
      0, t
  in
  let nbenv = nbenv + nargs in
  let i,a,b = Term.binary_split_0 t in
  let i = i - nbenv in
  assert (i < count ft);
  if (base_descriptor i ft)#is_equality then begin
    nargs, i, a, b
  end else
    raise Not_found


let is_equality (t:term) (nbenv:int) (ft:t): bool =
  try
    let _ = split_equality t nbenv ft in true
  with Not_found -> false



let add_base_features (mdl_name:int) (ft:t): unit =
  try
    let lst = IntMap.find mdl_name ft.base in
    List.iter
      (fun idx ->
        let desc = descriptor idx ft in
        if is_interface_use ft then
          desc.bdesc#set_exported;
        add_key idx ft;
        add_class_feature idx ft)
      !lst
  with Not_found ->
    ()




let add_used_module (m:Module.M.t) (ft:t): unit =
  let name = Module.M.base_name m in
  Class_table.add_used_module m ft.ct;
  add_base_features name ft




let add_current_module (m:Module.M.t) (ft:t): unit =
  let name = Module.M.base_name m in
  Class_table.add_current_module m ft.ct;
  add_base_features name ft;
  if name <> ST.symbol "core" then begin
    let or_desc  = descriptor Constants.or_index ft
    and and_desc = descriptor Constants.and_index ft in
    or_desc.bdesc#set_specification
      (Feature.Spec.make_func_def or_desc.argnames  None []);
    and_desc.bdesc#set_specification
      (Feature.Spec.make_func_def and_desc.argnames None [])
  end



let set_interface_check (ft:t): unit =
  Class_table.set_interface_check ft.ct



let check_interface (ft:t): unit =
  assert (is_interface_check ft);
  for i = 0 to count ft - 1 do
    let desc = descriptor i ft in
    if is_current_module desc.mdl ft
       && is_desc_deferred desc
       && not desc.bdesc#is_exported
       && Class_table.is_class_public (owner i ft) ft.ct
    then
      begin
        let open Module in
        assert (M.has_interface desc.mdl);
        error_info (FINFO (1,0,Src.path (M.interface desc.mdl)))
                   ("deferred feature \"" ^ (string_of_signature i ft) ^
                      "\" is not exported")
      end
  done




let pattern_subterms (n:int) (pat:term) (nb:int) (ft:t): (int*term*int) list =
  (* Return a list of all subterms of the pattern [n,pat] with their level.
   *)
  let rec subterms t level lst =
    match t with
      Variable i ->
        assert (i < n || n + nb <= i);
        assert (i < n || is_constructor (i-n-nb) ft);
        (n+nb,t,level)::lst
    | VAppl(i,args,ags,_) ->
        assert (n + nb <= i);
        assert (is_constructor (i-n-nb) ft);
        let lst = (n+nb,t,level)::lst
        and level = level + 1 in
        Array.fold_left
          (fun lst arg -> subterms arg level lst)
          lst
          args
    | _ ->
        assert false (* cannot happen in pattern *)
  in
  subterms pat 0 []




let peer_constructors (i:int) (ft:t): IntSet.t =
  assert (i < count ft);
  assert (is_constructor i ft);
  let cls = owner i ft in
  assert (cls <> -1);
  let set = Class_table.constructors cls ft.ct in
  assert (IntSet.mem i set);
  IntSet.remove i set



type tplst = type_term list

let peer_matches
    (i:int) (ags:agens) (nb:int) (ntvs:int) (ft:t): (int*tplst*term) list =
  (* Match expressions for the peer constructors on the constructor [i] with
     actual generics [ags] coming from an environment with [nb] variables and
     ntvs type variables. *)
  assert (i < count ft);
  assert (is_constructor i ft);
  let set = peer_constructors i ft in
  IntSet.fold
    (fun i lst ->
      assert (is_constructor i ft);
      let tvs,s = signature0 i ft in
      assert (Tvars.count_all tvs = Array.length ags);
      let n = Sign.arity s
      and tps = Array.to_list (Sign.arguments s) in
      let tps = List.map (fun tp -> Term.subst tp ntvs ags) tps in
      let t =
        let args = Array.init n (fun i -> Variable i) in
        VAppl (i+nb+n,args,ags,false) in
      (n,tps,t)::lst)
    set
    []





(*  Complementary Pattern
    =====================

    We have a pattern [pat] and want to find the minimal set of complementary
    pattern so that one pattern of the complete set always matches.

    (a) The pattern is a variable: The complementary set is empty (catch all)

    (b) The pattern has the form [f(a,b,...)] (Note: [f] has to be a constructor).

        The first part of the complementary set consists of all trivial
        pattern of the peer constructors of [f].

        Each argument has a complementary set. We have to make a combination
        of the complementary sets of the arguments. We compute a list of
        argument pattern lists and start with the list of arguments and an
        empty list of argument pattern lists:

        (a1) The argument list is empty: No pattern is added to the list of
             argument pattern lists.

        (a2) The argument list is 'rest+a':

             cset: The complementary pattern set of 'a'
             lst:  The list of argument pattern lists of the rest

             for each pattern in {a}+cset
                 for each argument pattern list in 'lst':
                     append a to the argument pattern list
             concatenate all list of argument pattern lists

             for each pattern p in cset
                     add [v0,v1,...,p] to the resulting argument pattern list ????

        (a3) At the end we have a complete list of argument pattern list

             The complementary set of 'f(a,b,...)' is

                   fcset + {f(a0,b0,...), f(a1,b1,...), ... }

             where [ai,bi,...] is an entry in the list of argument pattern lists.

    Boundary cases:

        f(a,b,...) is f(v0,v1,...) where vi is a variable:

        The complementary set of each vi is empty. The list of argument
        pattern lists before is empty before and remains empty during
        (a2). Therefore the complementary pattern set of f(v) consist of all
        primitive pattern of the peer constructors of f.

    Implementation hints:

        (a) A pattern set has the form

                 {(p0,n0), (p1,n1), ... }

            where p is the pattern term and n is the number of variables in
            the pattern.

        (b) A list of argument pattern lists has the form

                 [ ([p0a,p0b,...],n0), ([p1a,p1b,...],n1), ... ]

            where [pia,pib,...] is the ith argument pattern sequence and [ni]
            is the number of used variables in the pattern sequence.

            Appending an argument pattern (p,n) to the sequence ([pia,pib,...],ni)
            results in ([pia,pib,...p'],n+ni) where p' is p shifted up by n.


 *)

let complementary_pattern
    (n:int) (tps:types) (p:term) (nb:int) (ntvs:int) (ft:t)
    : (int*tplst*term) list =
  let rec compl_pat (p:term): (int*tplst*term) list =
    let rec args_pat_lst (argsrev:term list) (nargs:int)
        (aplst:(term list*int*tplst) list)
        : (term list*int*tplst) * (term list*int*tplst) list =
      assert (List.length argsrev = nargs);
      match argsrev with
        [] ->
          ([],0,[]), aplst
      | a::tl ->
          let cset  = compl_pat a
          and (argsrevtl,nargsrevtl,tpsrevtl), aplst =
            args_pat_lst tl (nargs-1) aplst in
          let a,na,tpsa =
            let abnd  = Term.bound_variables a n
            and perm  = Array.make n (Variable (-1)) in
            let na,tpsa = IntSet.fold
                (fun i (na,tpsa) ->
                  perm.(i) <- Variable na; na+1, tps.(i)::tpsa)
                abnd (0,[]) in
            let a = Term.subst a na perm in
            a,na,tpsa
          in
          let push_arg n tps p nargsrev argsrev tpsrev =
            let argsrev =
              List.map (fun t -> Term.up_from n nargsrev t) argsrev in
            let argsrev = (Term.up nargsrev p)::argsrev
            and tps = List.rev_append tps tpsrev in
            argsrev,(n+nargsrev),tps in
          let prepend_pattern np tps p aplst =
            List.rev_map
              (fun (argsrev,n,tpsrev) -> push_arg np tps p n argsrev tpsrev)
              aplst in
          let aplst =
            let cset = (na,tpsa,a)::cset in
            let lstlst =
              List.rev_map (fun (n,tps,p) -> prepend_pattern n tps p aplst) cset in
            List.concat lstlst in
          let push_arg0 n tps p = push_arg n tps p nargsrevtl argsrevtl tpsrevtl in
          let aplst =
            List.fold_left
              (fun aplst (n,tps,p) ->
                (push_arg0 n tps p)::aplst)
              aplst
              cset in
          push_arg0 na tpsa a, aplst
    in
    match p with
      Variable i when i < n ->
        []
    | Variable i ->
        assert (n + nb <= i);
        assert false (* there are no global variables *)
    | VAppl (i,args,ags,_) ->
        assert (n + nb <= i);
        let idx = i-n-nb in
        let fcset = peer_matches idx ags nb ntvs ft in
        let nargs    = Array.length args
        and args_rev = List.rev (Array.to_list args) in
        let _,apl = args_pat_lst args_rev nargs [] in
        List.fold_left
          (fun cset (args_rev,n,tps) ->
            let args = Array.of_list (List.rev args_rev)
            and tps  = List.rev tps in
            assert (Array.length args = nargs);
            let p  = VAppl (idx+n+nb,args,ags,false) in
            (n,tps,p) :: cset)
          fcset
          apl
    | _ ->
        assert false (* cannot happen in pattern *)
  in
  compl_pat p




let is_pattern (n:int) (t:term) (nb:int) (ft:t): bool =
  (* Is the term [t] with [n] variables a pattern i.e. does it contain only variables
     or constructors?

     All variables below [n] must occur only once in a pattern.
   *)
  let is_constr i = (n+nb) <= i && is_constructor (i-n-nb) ft
  in
  let free = Term.free_variables     t n
  and bnd  = Term.bound_variables    t n
  and bnd_lst = Term.used_variables t n in
  let nbnd = IntSet.cardinal bnd
  in
  IntSet.for_all is_constr free &&
  n = nbnd &&
  nbnd = List.length bnd_lst









(*  Unmatched cases
    ===============


    Example:

        is_prefix (a,b:[G]): BOOLEAN
            -> inspect a, b
               case nil, _   then true
               case _  , nil then false
               case x^a, y^b then x = y and a.is_prefix(b)
               end

    The first case leaves us the unmatched cases:

         (0^1, 2)

    The second case has the pattern (0, nil) and should leave us with

         (0^1, 2^3)

    which is matched by the third case.

    The peer pattern of the second case:

        (0, 1^2)

    Check the second case:
        compare unmatched    (0^1,   2)
        with pattern         (0  , nil)

    An unmatched pattern has to be either
        removed                           the pattern is more general
        left in unchanged                 unmatched and pattern do not match
        modified and/or splitted up       the pattern matches partially

    Partial match: There is a substitution for the unmatched and for the pattern.
                   When both substitutions are applied the terms are equal

       (0^1,2)   sub [0~>0,1~>1,2~>nil]    res (0^1,nil)
       (0,nil)   sub [0~>0^1]              res (0^1,nil)

    The pattern is more general: Special case of a partial match where the
                                 substitution of the unmatched is empty

    No match: There is no substitution pair.


    Unification of two pattern:
    ===========================

    Example 1:
    (0^1,2) and (0,nil)

    With unique variables (0^1,2) and (3,nil)

    General substitution: [0~>0,1~>1,2~>nil,  3~>0^1]
    makes both to (0^1,nil)

    Example 2:
    (0^1,2) and (0,1)

    With unique variables (0^1,2) and (3,4)

    General substitution: [0~>0,1~>1,2~>2,  3~>0^1,4~>2]
    makes both to (0^1,2)

    If one pattern is more special than the other it gets the identity substitution.
    I.e. substitution happens only on the more general pattern.

    Computing unmatched pattern
    ===========================

    For each case clause we have a set of unmatched pattern of the previous clauses.

    For each pattern in the set of unmatched pattern the current pattern is either

    - unrelated:     The unmatched pattern remains
    - more general:  The unmatched pattern is resolved
    - partial match: The unmatched pattern has to be splitted into unmatched and
                     resolved subpattern. Only the unmatched subpattern remain.

    Splitting partially resolved pattern
    ====================================

    We have an unmatched pattern upat and a pattern pat. We unify the two pattern
    and get a substitution which is not the identity in the part of the variables
    of upat. The variables with a nontrivial substitution are only partially
    resolved.

    1. Do the substitution on upat to get upatsub
         The partially resolved variables disappear and some variables of pat
         might be introduced

    2. Treat upatsub as a pattern and calculate all peer pattern

    3. Filter the peer pattern so that only pattern more special than upat remain

    The unmatched pattern upat has to be splitted up into the filtered set of peer
    pattern of upatsub.

 *)
let unmatched_and_splitted
    (n:int) (tps:tplst) (pat:term)   (* Pattern to be analyzed *)
    (unmatched:(int*tplst*term)list) (* Complementary pattern of the previous cases *)
    (nb:int) (ntvs:int) (ft:t)
    : (int*tplst*term) list * (int * tplst * term * term array option) list =
  (* Calculate the remaining unmatched pattern and a split list. The unmatched
     pattern are all the pattern which are left over with the pattern [n,pat]
     working of the unmatched cases in [unmatched]. The split list consist of one
     or more pattern into which [n,pat] has to be splitted. The pattern has to be
     splitted if it is more general than some pattern in [unmatched].
   *)
  let is_trivial arr n =
    assert (n <= Array.length arr);
    interval_for_all
      (fun i ->
        match arr.(i) with
          Variable k -> k < n
        | _ -> false)
      0 n
  and unmatched_partial
      (n:int) (tps:tplst) (pat:term)
      (arr: term array) (tps2:tplst)
      : int * tplst * term =
    assert (n <= Array.length arr);
    let len = Array.length arr in
    let n2  = len - n in
    let tps = Array.append (Array.of_list tps) (Array.of_list tps2) in
    assert (len = Array.length tps);
    let upat  = Term.up_from n2 n pat in
    let upat  = Term.subst upat len arr in
    let used  = List.rev (Term.used_variables upat len) in
    let n_new = List.length used in
    let arr2  = Array.make len empty_term
    and tps_new = Array.make n_new empty_term in
    List.iteri (fun pos i ->
      arr2.(i) <- Variable pos;
      tps_new.(pos) <- tps.(i)) used;
    let upat  = Term.subst upat n_new arr2
    and tps_new = Array.to_list tps_new in
    n_new, tps_new, upat in
  let add_filtered_peers
      (peers:(int*tplst*term) list)
      (npat:int) (tps:tplst) (pat:term)
      (lst:(int*tplst*term) list)
      : (int*tplst*term) list =
    List.fold_left
      (fun lst (n,tps,t) ->
        try
          let subarr = Term_algo.unify_pattern n t npat pat in
          if is_trivial subarr n then (n,tps,t) :: lst else lst
        with Not_found ->
          lst)
      lst
      peers
  in
  let unmatched,splitted = List.fold_left
      (fun (unmatched,splitted) (npat0,tps0,pat0) ->
        try
          let subarr = Term_algo.unify_pattern npat0 pat0 n pat in
          assert (Array.length subarr = npat0 + n);
          if is_trivial subarr npat0 then begin
            (* pat is more general, i.e. pat0 no longer needed as unmatched *)
            let newsplit =
              if npat0 = n && pat = pat0 then (n,tps,pat,None)
              else (npat0,tps0,pat0,Some subarr) in
            unmatched,
            newsplit::splitted
          end else begin
            (* pat resolves pat0 only partial, splitting of pat necessary *)
            let n2, tps2, upat2 = unmatched_partial npat0 tps0 pat0 subarr tps in
            assert (n2 = List.length tps2);
            let tps2arr = Array.of_list tps2 in
            let peers = complementary_pattern n2 tps2arr upat2 nb ntvs ft
            and subarr =
              try Term_algo.unify_pattern n2 upat2 n pat
              with Not_found -> assert false
            in
            add_filtered_peers peers npat0 tps0 pat0 unmatched,
            (n2,tps2,upat2,Some subarr)::splitted
          end
        with Not_found -> (* pat and pat_i cannot be unified *)
          (npat0,tps0,pat0) :: unmatched, splitted)
      ([],[])
      unmatched
  in
  unmatched, splitted



let unmatched_inspect_cases
      (cases: (formals*term*term) array)
      (nb:int)
      (ntvs:int)
      (ft:t)
    : (int * tplst* term) list =
  (* The unmatched cases of the inspect expression [Flow(Inspect,args)]
   *)
  assert (Array.length cases > 0);
  Array.fold_left
    (fun lst (fs,pat,res) ->
      let n = Array2.count fs in
      let tpslst = Array.to_list (Array2.second fs) in
      let unmatched,_ =
        unmatched_and_splitted n tpslst pat lst nb ntvs ft
      in
      unmatched
    )
    [1, [empty_term], Variable 0] (* empty_term shall not be used!! *)
    cases





let downgrade_term (t:term) (nb:int) (ntvs:int) (ft:t): term =
  (* Downgrade all calls of the form

         [ Application (VAppl (i, [||], ags), args, pr)]
     to

         [VAppl(i,args')] if [i] is not a constant.
   *)
  let rec down t nb =
    let down_args args nb = Array.map (fun t -> down t nb) args
    and down_list lst nb  = List.map  (fun t -> down t nb) lst in
    match t with
      Variable _ ->
        t
    | VAppl (i,args,ags,oo) ->
        VAppl(i, down_args args nb,ags,oo)
    | Application(VAppl (i,[||],ags,oo), args, _) when nb <= i ->
        assert (Array.length args = 1);
        let nargs = arity (i - nb) ft in
        let args = down_args args nb in
        if nargs = 0 then
          t
        else
          let s = signature i ags ntvs ft in
          let tup_tp = Class_table.to_tuple ntvs 0 (Sign.arguments s) in
          let args = args_of_tuple_ext args.(0) tup_tp nb nargs ft in
          VAppl(i,args,ags,oo)
    | Application(f,args,inop) ->
        Application (down f nb, down_args args nb, inop)
    | Lam(tps,fgs,pres,t0,rt) ->
       let nb = nb + Formals.count tps in
        Lam (tps,fgs, down_list pres nb, down t0 nb, rt)
    | QExp (tps,fgs,t0,is_all) ->
        QExp (tps,fgs, down t0 (Formals.count tps + nb), is_all)
    | Ifexp(cond,a,b) ->
       Ifexp (down cond nb, down a nb, down b nb)
    | Asexp(insp,tps,pat) ->
       Asexp(down insp nb, tps, pat)
    | Inspect(insp, cases) ->
       Inspect(down insp nb,
               Array.map
                 (fun (fs,pat,res) ->
                   let nb = nb + Array2.count fs in
                   fs, pat, down res nb
                 )
                 cases)
    | Indset (nme,tp,rs) ->
        Indset (nme, tp, down_args rs (1+nb))
  in
  down t nb


let collect_called (t:term) (nb:int) (ft:t): IntSet.t =
  let rec collect (t:term) (nb:int) (set:IntSet.t): IntSet.t =
    let collect_args args nb set =
      Array.fold_left
        (fun set arg -> collect arg nb set)
        set
        args
    in
    match t with
      Variable _ ->
        set
    | VAppl(i,args,ags,_) ->
        assert (nb <= i);
        let set = IntSet.add (i-nb) set in
        collect_args args nb set
    | Application (f,args,_) ->
        let set = collect f nb set in
        collect_args args nb set
    | Lam (tps, _, pres, t0, _) ->
        collect t0 (Formals.count tps + nb) set
    | QExp (args, fgs, t0, is_all) ->
        collect t0 (Formals.count args + nb) set
    | Ifexp(cond,a,b) ->
       collect cond nb set |> collect a nb |> collect b nb
    | Asexp (insp,tps,pat) ->
       collect insp nb set |> collect pat (nb+Array.length tps)
    | Inspect(insp,cases) ->
       Array.fold_left
         (fun set (fs,pat,res) ->
           let nb = nb + Array2.count fs in
           collect pat nb set |> collect res nb
         )
         (collect insp nb set)
         cases
    | Indset (nme,tp,rs) ->
        collect_args rs (1+nb) set
  in
  collect t nb IntSet.empty


let involved_assertions (fidx:int) (ft:t): IntSet.t =
  (base_descriptor fidx ft)#involved_assertions


let add_involved_assertion (ia:int) (t:term) (ft:t): unit =
  (* Add [ia] as an involved assertion to all global functions of the term [t].
   *)
  IntSet.iter
    (fun fidx -> (base_descriptor fidx ft)#add_assertion ia)
    (collect_called t 0 ft)



let equal_symmetry_term (): term =
  (* Construct all(a,b) a = b ==> b = a *)
  let eq_id  = 2  + Constants.eq_index
  and imp_id = 2  + Constants.implication_index
  and a = Variable 0
  and b = Variable 1
  and ag = Variable 0
  and any_tp = Variable (1 + Constants.any_class)
  in
  let eq a b = VAppl (eq_id, [|a;b|], [|ag|], false) in
  let imp = Term.binary imp_id (eq a b) (eq b a) in
  Term.all_quantified
    (Formals.make (standard_argnames 2) [|ag;ag|])
    (Formals.make (standard_fgnames 1)  [|any_tp|])
    imp


let leibniz_term (): term =
  (* Construct all(a,b,p) a = b ==> p(a) ==> p(b) *)
  let eq_id  = 3 + Constants.eq_index
  and imp_id = 3 + Constants.implication_index
  and a = Variable 0
  and b = Variable 1
  and p = Variable 2
  and ag = Variable 0
  and any_tp = Variable (1 + Constants.any_class)
  in
  let pred = Class_table.predicate_type ag 1 in
  let eqab = VAppl (eq_id, [|a;b|], [|ag|],false)
  and p x  = Application (p, [|x|], false) in
  let imp = Term.binary imp_id eqab (Term.binary imp_id (p a) (p b)) in
  Term.all_quantified
    (Formals.make (standard_argnames 3) [|ag;ag;pred|])
    (Formals.make (standard_fgnames 1)  [|any_tp|])
    imp
