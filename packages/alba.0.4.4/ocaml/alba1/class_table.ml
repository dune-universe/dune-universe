(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
 *)

open Support
open Term
open Signature
open Container
open Printf

module Option = Fmlib.Option

type formal = int * type_term



type parent_descriptor = {
    is_ghost: bool;
    actual_generics: type_term array
  }

type base_descriptor = { hmark:    header_mark;
                         mutable cvar: int;  (* class variable,
                                                for deferred classes *)
                         mutable tvs: Tvars.t;
                         mutable generics: (bool*int) list; (* features and
                                                               assertions *)
                         mutable constructors: IntSet.t;
                         mutable base_constructors: IntSet.t;
                         mutable descendants:  IntSet.t;
                         mutable ancestors: parent_descriptor IntMap.t}


type descriptor      = {
    mdl:  Module.M.t;
    ident: int;
    name: int;
    bdesc: base_descriptor;
    mutable indlaws: (int * (term * int * term list) array * IntSet.t) list;
                   (* law idx, constructors, constructor set
                      array: each constructor with a ghost recognizer and
                      preconditions *)
    mutable rec_pairs: (term * term) list;
    mutable can_match_pattern: bool;
    mutable wflaws: (int * int) list; (* law idx, relation *)
    mutable is_exp: bool
  }


type t = {mutable map:   int list IntMap.t;
          seq:           descriptor seq;
          mutable base:  int list IntMap.t; (* module name -> class indices *)
          mutable fgens: type_term IntMap.t;
          mutable comp:  Module.Compile.t}




let compilation_context (ct:t): Module.Compile.t = ct.comp

let current_module (ct:t): Module.M.t =
  compilation_context ct |> Module.Compile.current

let is_current_module (m:Module.M.t) (ct:t): bool =
  Module.M.equal m (current_module ct)

let current_package (ct:t): library_name =
  let open Module in
  current_module ct |> M.package_name

let is_private (ct:t): bool =
  Module.Compile.is_verifying ct.comp

let is_public (ct:t):  bool =
  not (Module.Compile.is_verifying ct.comp)

let is_interface_check  (ct:t): bool =
  Module.Compile.is_interface_check ct.comp

let is_interface_use (ct:t): bool =
  Module.Compile.is_interface_use ct.comp

let is_interface_public_use (ct:t): bool =
  Module.Compile.is_interface_public_use ct.comp


let count (ct:t):int =
  Seq.count ct.seq

let standard_bdesc (hm:header_mark) (cvar:int) (tvs:Tvars.t) (idx:int)
    : base_descriptor =
  let args =
    Array.init
      (Tvars.count_fgs tvs)
      (fun i -> Variable i)
  in
  let anc  = IntMap.singleton idx
                              {is_ghost = false; actual_generics = args} in
  {hmark = hm;
   cvar;
   tvs;
   generics = [];
   base_constructors = IntSet.empty;
   constructors = IntSet.empty;
   descendants  = IntSet.empty;
   ancestors=anc}


let descriptor (idx:int) (ct:t): descriptor =
  assert (idx < count ct);
  Seq.elem idx ct.seq


let base_descriptor (idx:int) (ct:t): base_descriptor =
  assert (0 <= idx);
  assert (idx < count ct);
  (descriptor idx ct).bdesc



let is_deferred (cls:int) (ct:t): bool =
  let bdesc = base_descriptor cls ct in
  match bdesc.hmark with
    Deferred_hmark -> true
  | _ -> false



let class_symbol (i:int) (ct:t): int =
  assert (i<count ct);
  (descriptor i ct).name


let module_of_class (cls:int) (ct:t): Module.M.t =
  assert (cls < count ct);
  (descriptor cls ct).mdl

let core_module (ct:t): Module.M.t =
  let comp = compilation_context ct in
  let open Module in
  assert (MSet.has_id 0 (Compile.set comp));
  MSet.module_of_id 0 (Compile.set comp)

let is_exported (c:int) (ct:t): bool =
  (descriptor c ct).is_exp

let is_visible (cidx:int) (ct:t): bool =
  (* Is the class visible i.e. being in interface check mode implies that the
     class is either defined in a publicly used module or it is defined in the
     current module and exported.
   *)
  assert (cidx < count ct);
  let open Module in
  let curr_mdl = current_module ct
  and cls_mdl  = module_of_class cidx ct
  in
  if is_private ct then
    true
  else if is_interface_use ct then
    M.uses_public curr_mdl cls_mdl
  else
    (* interface check mode *)
    (M.equal curr_mdl cls_mdl && is_exported cidx ct)
    || M.uses_public curr_mdl cls_mdl


let is_class_public (cidx:int) (ct:t): bool = is_visible cidx ct



exception Ambiguous of int list

let find_unqualified
      (nme: int) (ct:t): int =
  (* Find a class by its unqualified name.

     The class can be identifed  uniquely in the following cases:

     - The classname is unique in the system

     - A class with this name exists in the current module

     - Only one class of this name exists in the current package.

     If no class of [nme] is found then [Not_found] is raised. If the
        class cannot be uniquely identified then [Ambiguous lst] is
        raised. *)
  let m = current_module ct in
  let lst0 = IntMap.find nme ct.map in
  let lst0 = List.filter (fun c -> is_visible c ct) lst0 in
  match lst0 with
  | [] ->
     raise Not_found
  | [cls] ->
     cls
  | _ ->
     let lst1 =
       List.filter
         (fun cls -> Module.M.equal m (module_of_class cls ct))
         lst0
     in
     match lst1 with
     | [c] ->
        c
     | _ ->
        assert (lst1 = []); (* There cannot be multiple classes with the
                               same name in the same module. *)
        let lst2 =
          List.filter
            (fun c -> Module.M.same_package m (module_of_class c ct))
                lst0
        in
        match lst2 with
        | [c] ->
           c
        | _ ->
           raise (Ambiguous lst0)


let qualified_class_name (i:int) (ct:t): string =
  let open Module in
  let desc = descriptor i ct in
  let m = desc.mdl in
  let str = ST.string desc.name
  in
  let curr = current_module ct
  in
  if M.equal curr m then
    str
  else if M.same_package m curr then
    ST.string (M.base_name m) ^ "." ^ str
  else
    M.string_of_name m ^ "." ^ str


let class_name (i:int) (ct:t): string =
  let desc = descriptor i ct in
  try
    let i2 = find_unqualified desc.name ct in
    if i = i2 then
      ST.string desc.name
    else
      qualified_class_name i ct
  with Not_found ->
       ST.string desc.name
     | Ambiguous _ ->
        qualified_class_name i ct





let add_to_map (cls:int) (ct:t): unit =
  (* Add the class [cls] to the map in order to be able to find it.
   *)
  assert (cls < count ct);
  let desc = descriptor cls ct in
  try
    let lst = IntMap.find desc.name ct.map in
    assert (not (List.mem cls lst));
    ct.map <- IntMap.add desc.name (cls::lst) ct.map
  with Not_found ->
    ct.map <- IntMap.add desc.name [cls] ct.map



let add_base_classes (mdl_nme:int) (ct:t): unit =
  try
    let clslst = IntMap.find mdl_nme ct.base in
    List.iter
      (fun cls -> add_to_map cls ct)
      clslst
  with Not_found ->
    ()



let add_used_module (m:Module.M.t) (ct:t): unit =
  ct.fgens <- IntMap.empty;
  ct.comp  <- Module.Compile.set_current m ct.comp;
  let name = Module.M.name m in
  add_base_classes (fst name) ct




let add_current_module (m:Module.M.t) (ct:t): unit =
  let name = Module.M.base_name m in
  ct.fgens <- IntMap.empty;
  ct.comp  <- Module.Compile.set_current m ct.comp;
  add_base_classes name ct



let set_interface_check (ct:t): unit =
  ct.fgens <- IntMap.empty;
  ct.comp  <- Module.Compile.set_interface_check ct.comp


let descendants (i:int) (ct:t): IntSet.t =
  assert (i < count ct);
  (base_descriptor i ct).descendants


let class_type (i:int) (ct:t): type_term * Tvars.t =
  (* A type term and a type environment which represents the class. For
     inheritable classes (deferred classes) a type variable (formal generic)
     is returned whose concept is the class.  *)
  assert (i < count ct);
  let bdesc = base_descriptor i ct in
  let nfgs  = Tvars.count_fgs bdesc.tvs in
  if is_deferred i ct then
    begin
      let tvs = Tvars.augment_fgs [|bdesc.cvar|] [|Variable (i+1)|] bdesc.tvs in
      let tp =
        if nfgs = 0 then
          Variable 0
        else
          let ags = Array.init nfgs (fun i -> Variable (i+1)) in
          make_type 0 ags
      in
      tp, tvs
    end
  else
    let tp =
      if nfgs = 0 then
        Variable i
      else
        make_type (i+nfgs) (Array.init nfgs (fun i -> Variable i))
    in
    tp, bdesc.tvs


let domain_type (tp:type_term): type_term =
  (* [tp] is either a function type [A->B] or a predicate type {A}. The domain
     type is in both cases A.
   *)
  let _,ags = split_type tp in
  assert (0 < Array.length ags);
  ags.(0)


let to_tuple (ntvs:int) (start:int) (args:type_term array): type_term =
  let n = Array.length args in
  assert (n > 0);
  let rec tuple (i:int) (tp:type_term): type_term =
    assert (0 <= i);
    if i = start then
      tp
    else
      let i = i - 1
      and tup_id = ntvs + Constants.tuple_class in
      let tp = make_type tup_id [|args.(i);tp|] in
      tuple i tp
  in
  tuple (n-1) args.(n-1)




let boolean_type (ntvs:int)  = Variable (Constants.boolean_class+ntvs)
let any (ntvs:int)           = Variable (Constants.any_class+ntvs)
let func nb dom ran =
  make_type (nb+Constants.function_class) [|dom;ran|]


let predicate_type (tp:type_term) (ntvs:int): type_term =
  make_type (ntvs+Constants.predicate_class) [|tp|]

let function_type (tp_a:type_term) (tp_b:type_term) (ntvs:int): type_term =
  make_type(ntvs+Constants.function_class) [|tp_a;tp_b|]

let to_dummy (ntvs:int) (s:Sign.t): type_term =
  (* Convert the callable signature [0,1,...]:RT to the dummy signature
     @DUMMY[(0,(1,...)),RT].  *)
  assert (Sign.has_result s);
  if Sign.arity s = 0 then
    Sign.result s
  else
    let tup = to_tuple ntvs 0 (Sign.arguments s) in
    make_type (ntvs+Constants.dummy_class) [|tup;Sign.result s|]


let to_function (ntvs:int) (s:Sign.t): type_term =
  (* Convert the callable signature [0,1,...]:RT to the function signature
     (0,(1,...)) ->RT  *)
  assert (Sign.has_result s);
  if Sign.arity s = 0 then
    Sign.result s
  else
    let tup = to_tuple ntvs 0 (Sign.arguments s)
    and fid = ntvs + Constants.function_class in
    make_type fid [|tup;Sign.result s|]


let upgrade_signature (ntvs:int) (is_pred:bool) (s:Sign.t): type_term =
  (* Convert the callable signature [0,1,...]:RT to a predicate (0,1,...)? or to a
     function signature (0,(1,...)) -> RT.  *)
  assert (Sign.has_result s);
  assert (Sign.arity s > 0);
  let tup = to_tuple ntvs 0 (Sign.arguments s)
  in
  let idx, args =
    if is_pred then
      Constants.predicate_class,  [|tup|]
    else
      Constants.function_class, [|tup;Sign.result s|]
  in
  let idx = idx + ntvs in
  make_type idx args



let result_type_of_compound (tp:type_term) (ntvs:int): type_term =
  let cls_idx,args = split_type tp in
  if cls_idx = ntvs + Constants.predicate_class then begin
      assert (Array.length args = 1);
      boolean_type ntvs
    end else if cls_idx = ntvs + Constants.function_class ||
                  cls_idx = ntvs + Constants.dummy_class
  then begin
      assert (Array.length args = 2);
      args.(1)
    end else
    raise Not_found



let is_boolean_binary (s:Sign.t) (ntvs:int): bool =
  (Sign.is_binary s)
  && (Sign.arg_type 0 s) = (boolean_type ntvs)
  && (Sign.arg_type 1 s) = (boolean_type ntvs)
  && (Sign.result s)     = (boolean_type ntvs)

let is_boolean_unary (s:Sign.t) (ntvs:int): bool =
  (Sign.is_unary s)
  && (Sign.arg_type 0 s) = (boolean_type ntvs)
  && (Sign.result s)     = (boolean_type ntvs)



let type2string (t:term) (nb:int) (fgnames: int array) (ct:t): string =
  (** Convert the type term [t] in an environment with [nb] type variables
      and the formal generics [fgnames] to a string.
   *)
  let nfgs = Array.length fgnames
  in
  let rec to_string(t:term) (nb:int) (prec:int): string =
    let args_to_string (tarr:term array) (nb:int): string =
      "["
      ^ (String.concat
           ","
           (Array.to_list (Array.map (fun t -> to_string t nb 1) tarr)))
      ^ "]"
    in
    let inner_prec, str =
      match t with
      | Variable j ->
         2,
         if j<nb then
           string_of_int j
         else if j < nb+nfgs then
           ST.string fgnames.(j-nb)
         else
           class_name (j-nb-nfgs) ct
      | Application(Variable j, tarr, _ ) ->
         let j1 = j-nb-nfgs
         and tarrlen = Array.length tarr in
         if j1 = Constants.predicate_class then begin
             assert (tarrlen=1);
             1, ("{" ^ (to_string tarr.(0) nb 1) ^ "}")
           end else if j1 = Constants.list_class then begin
             assert (tarrlen=1);
             1, ("[" ^ (to_string tarr.(0) nb 1) ^ "]")
           end else if j1 = Constants.function_class then begin
             assert (tarrlen=2);
             1, ((to_string tarr.(0) nb 2) ^ "->" ^ (to_string tarr.(1) nb 1))
           end else if j1 = Constants.tuple_class then begin
             assert (tarrlen=2);
             0, ((to_string tarr.(0) nb 1) ^ "," ^ (to_string tarr.(1) nb 0))
           end else begin
             2,
             (to_string (Variable j) nb 1) ^ (args_to_string tarr nb)
           end
      | VAppl (j,tarr,_,_) ->
         printf "type2string VAppl cannot happen %s\n" (Term.to_string t);
         0, to_string (make_type j tarr) nb 1
      | _ ->
         assert false (* cannot happen with types *)
    in
    if inner_prec < prec then "(" ^ str ^ ")" else str
  in
  to_string t nb 1


let string_of_type (tp:type_term) (tvs:Tvars.t) (ct:t): string =
  type2string tp (Tvars.count tvs) (Tvars.fgnames tvs) ct


let string_of_type_arr (ags: agens) (tvs:Tvars.t) (ct:t): string =
  String.concat
    ","
    (List.map (fun tp -> string_of_type tp tvs ct) (Array.to_list ags))



let string_of_concepts (tvs:Tvars.t) (ct:t): string =
  string_of_type_arr (Tvars.concepts tvs) tvs ct



let string_of_fgconcepts (tvs:Tvars.t) (ct:t): string =
  let cptsarr = Myarray.combine (Tvars.fgnames tvs) (Tvars.fgconcepts tvs) in
  let lst = Array.to_list cptsarr in
  String.concat
    ","
    (List.map (fun (n,tp) ->
         (ST.string n) ^ ":" ^ (string_of_type tp tvs ct)) lst)


let string_of_tvs (tvs:Tvars.t) (ct:t): string =
  let str1 =
    if Tvars.count_local tvs = 0 then
      ""
    else
      "(" ^ string_of_int (Tvars.count_local tvs) ^ ")"
  and str2 = string_of_concepts tvs ct
  and str3 = string_of_fgconcepts tvs ct in
  let strcpts =
    if str2 = "" && str3 = "" then
      ""
    else if str3="" then
      "[" ^ str2 ^ "]"
    else if str2="" then
      "[" ^ str3 ^ "]"
    else
      "[" ^ str2 ^ "," ^ str3 ^ "]"
  in
  str1 ^ strcpts


let string_of_inner_fgs (nfgs:int) (tvs:Tvars.t) (ct:t): string =
  assert (nfgs <= Tvars.count_fgs tvs);
  if nfgs = 0 then
    ""
  else
    "[" ^
      String.concat
        ","
        (Array.to_list
           (Array.init
              nfgs
              (fun i ->
                ST.string (Tvars.fgnames tvs).(i) ^ ":" ^
                  string_of_type (Tvars.fgconcepts tvs).(i) tvs ct
              )
           )
        )
      ^ "]"



let string_of_sub (sub:Term_sub.t) (tvs:Tvars.t) (ct:t): string =
  let lst = Term_sub.to_list sub in
  let str =
    String.concat
      ","
      (List.map
         (fun (i,t) ->
           (string_of_int i) ^ ":=" ^ (string_of_type t tvs ct))
         lst)
  in
  "[" ^ str ^ "]"



let arguments_string
      (tvs:Tvars.t) (args:formal array) (ct:t)
    : string =
  (* The string "(a:A, b1,b2:B, ... )" of the arguments [args] within the
     type environment [tvs].

     In case that there are no arguments the empty string is returned and
     not "()".
   *)
  let nargs = Array.length args in
  if nargs = 0 then
    ""
  else
    let fargs = Array.to_list args
    in
    let llst = List.fold_left
                 (fun ll (n,tp) -> match ll with
                                     [] -> [[n],tp]
                                   | (ns,tp1)::tl ->
                                      if tp=tp1 then (n::ns,tp)::tl
                                      else           ([n],tp)::ll )
                 []
                 fargs
    in
    "("
    ^  String.concat
         ","
         (List.rev_map
            (fun (ns,tp) ->
              let ntvs = Tvars.count tvs in
              (String.concat "," (List.rev_map (fun n -> ST.string n) ns))
              ^ ":"
              ^ (type2string tp ntvs (Tvars.fgnames tvs) ct))
            llst)
    ^ ")"




let arguments_string2
      (tvs:Tvars.t) (nms:names) (tps:types) (ct:t)
    : string =
  (* The string "(a:A, b1,b2:B, ... )" of the arguments [args] within the
     type environment [tvs] prefixed of a potential list of formal generics.

     In case that there are no arguments the empty string is returned and
     not "()".
   *)
  let nargs = Array.length nms in
  assert (nargs = Array.length tps);
  let args = Myarray.combine nms tps in
  arguments_string tvs args ct



let find_for_declaration (cn:int) (ct:t): int =
  (* A class declaration declares a new class unless the class has already
     been declared in the same module.
   *)
  List.find
    (fun cls -> is_current_module (module_of_class cls ct) ct)
    (IntMap.find cn ct.map)



let extract_from_tuple
      (nargs:int) (ntvs:int) (tp:type_term): type_term array =
  assert (0 < nargs);
  let tup_idx = ntvs + Constants.tuple_class in
  let rec extract
            (n:int) (tp:type_term) (lst:type_term list): type_term list =
    assert (0 < n);
    if n = 1 then
      tp :: lst
    else
      let cls_idx, args = split_type tp in
      if cls_idx = tup_idx then
        extract (n-1) args.(1) (args.(0)::lst)
      else
        raise Not_found
  in
  let lst = extract nargs tp [] in
  Array.of_list (List.rev lst)





let extract_from_tuple_max (ntvs:int) (tp:type_term): type_term array =
  let tup_idx = ntvs + Constants.tuple_class in
  let rec extract (tp:type_term) (lst:type_term list): type_term list =
    let cls_idx, args = split_type tp in
    if cls_idx = tup_idx then begin
        extract args.(1) (args.(0)::lst)
      end else
      tp :: lst
  in
  let lst = extract tp [] in
  Array.of_list (List.rev lst)



let arity_of_downgraded (ntvs:int) (tp:type_term): int =
  let pred_idx = Constants.predicate_class + ntvs
  and func_idx = Constants.function_class  + ntvs
  and dum_idx  = Constants.dummy_class     + ntvs
  in
  let cls,args = split_type tp in
  if cls = pred_idx || cls = func_idx || cls = dum_idx then begin
      assert (0 < Array.length args);
      let args = extract_from_tuple_max ntvs args.(0) in
      Array.length args
    end else
    0


let downgrade_signature
      (ntvs:int) (sign:Sign.t) (nargs:int): Sign.t =
  assert (Sign.arity sign < nargs);
  if not (Sign.is_constant sign || Sign.arity sign = 1) then
    raise Not_found;
  let pred_idx = Constants.predicate_class + ntvs
  and func_idx = Constants.function_class  + ntvs
  and dum_idx  = Constants.dummy_class     + ntvs
  in
  if Sign.is_constant sign then
    let tp = Sign.result sign in
    let cls_idx,args = split_type tp in
    if cls_idx < ntvs then
      raise Not_found
    else if cls_idx = pred_idx then
      begin
        assert (Array.length args = 1);
        Sign.make_func
          (extract_from_tuple nargs ntvs args.(0))
          (boolean_type ntvs)
      end
    else if cls_idx = func_idx || cls_idx = dum_idx then
      begin
        assert (Array.length args = 2);
        Sign.make_func
          (extract_from_tuple nargs ntvs args.(0))
          args.(1)
      end
    else
      raise Not_found
  else
    begin
      let args, rt = Sign.arguments sign, Sign.result_type sign in
      assert (Array.length args = 1);
      let args = extract_from_tuple nargs ntvs args.(0) in
      Sign.make args rt
    end




let primary_induction_law (cls:int) (ct:t)
    : int * (term * int * term list) array * IntSet.t =
  assert (cls < count ct);
  match (descriptor cls ct).indlaws with
  | law :: _ ->
     law
  | _ ->
     raise Not_found


let add_induction_law
      (idx:int) (cs:(term * int * term list) array) (cls:int) (ct:t)
    : unit =
  (* Add the induction law [idx] with its constructors [cs] to the class
     [cls]. *)
  assert (cls < count ct);
  let cset =
    Array.fold_left
      (fun cset (_,co,_) -> IntSet.add co cset) IntSet.empty cs
  in
  let desc = descriptor cls ct in
  desc.indlaws <- desc.indlaws @ [idx,cs,cset]



let is_inductive (cls:int) (ct:t): bool =
  assert (cls < count ct);
  (base_descriptor cls ct).constructors <> IntSet.empty


let is_pseudo_inductive (cls:int) (ct:t): bool =
  (descriptor cls ct).indlaws <> []
  && not (is_inductive cls ct)


let constructors (cls:int) (ct:t): IntSet.t =
  assert (cls < count ct);
  if is_inductive cls ct then
    let bdesc = base_descriptor cls ct in
    bdesc.constructors
  else if is_pseudo_inductive cls ct then
    let _,_,cset = primary_induction_law cls ct in
    cset
  else
    IntSet.empty


let base_constructors (cls:int) (ct:t): IntSet.t =
  assert (cls < count ct);
  let bdesc = base_descriptor cls ct in
  bdesc.base_constructors


let can_match_pattern (cls:int) (ct:t): bool =
  (descriptor cls ct).can_match_pattern

let set_pattern_match (cls:int) (ct:t): unit =
  assert (is_pseudo_inductive cls ct);
  (descriptor cls ct).can_match_pattern <- true


let set_constructors
      (base_set:IntSet.t) (set:IntSet.t) (cls:int) (ct:t): unit =
  assert (cls < count ct);
  assert (not (is_interface_check ct));
  assert (not (is_inductive cls ct));
  assert (not (is_pseudo_inductive cls ct));
  let desc = descriptor cls ct in
  assert (desc.bdesc.constructors = IntSet.empty);
  desc.bdesc.base_constructors <- base_set;
  desc.bdesc.constructors <- set;
  desc.can_match_pattern <- true



let recognizer_pairs (cls:int) (ct:t): (term*term) list =
  (descriptor cls ct).rec_pairs


let add_recognizer_pair (t1:term) (t2:term) (cls:int) (ct:t): unit =
  let desc = descriptor cls ct in
  desc.rec_pairs <- (t1,t2) :: desc.rec_pairs


let primary_wellfounded_relation (cls:int) (ct:t): int =
  assert (cls < count ct);
  match (descriptor cls ct).wflaws with
  | (law,rel) :: _ ->
     rel
  | _ ->
     raise Not_found


let add_wellfounded_induction_law
      (idx:int) (rel_idx:int) (cls:int) (ct:t)
    : unit =
  (* Add the wellfounded induction law [idx] with its relation [rel_idx] to
     the class [cls]. *)
  assert (cls < count ct);
  let desc = descriptor cls ct in
  desc.wflaws <- desc.wflaws @ [idx,rel_idx]


let export
      (idx:   int)
      (hm:    header_mark withinfo)
      (tvs:   Tvars.t)
      (ct:    t)
    : unit =
  let desc = Seq.elem idx ct.seq in
  let hm1, hm2 = desc.bdesc.hmark, hm.v in
  if hm1 <> hm2 then begin
      let hstr = hmark2string hm1 in
      let hstr = if hstr = "" then "" else " \"" ^ hstr ^ "\"" in
      error_info
        hm.i
        ("Header mark is not consistent with previous header mark" ^
           hstr)
    end;
  if not (Tvars.is_equivalent desc.bdesc.tvs tvs) then begin
      let str =
        "The formal generics are not consistent with previous declaration\n" ^
          "   previous declaration \"" ^ (string_of_tvs desc.bdesc.tvs ct)
          ^ "\""
      in error_info hm.i str
    end;
  desc.bdesc.tvs <- tvs;
  desc.is_exp    <- true





let generics (cidx:int) (ct:t): (bool*int) list =
  (* The list of all generic features/assertions in the order of insertion. *)
  assert (cidx < count ct);
  List.rev (base_descriptor cidx ct).generics



let add_generic (idx:int) (is_ass:bool) (cls:int) (ct:t): unit =
  assert (cls < count ct);
  let bdesc = base_descriptor cls ct in
  assert (not (List.mem (is_ass,idx) bdesc.generics));
  bdesc.generics <- (is_ass,idx)::bdesc.generics



let add_generics (idx:int) (is_ass:bool) (tvs:Tvars.t) (ct:t): unit =
  assert (Tvars.count_all tvs = Tvars.count_fgs tvs);
  let set = Array.fold_left
              (fun set tp -> IntSet.add (Tvars.principal_class tp tvs) set)
              IntSet.empty
              (Tvars.fgconcepts tvs) in
  IntSet.iter
    (fun cls -> add_generic idx is_ass cls ct)
    set



let can_see (c1:int) (c2:int) (ct:t): bool =
  (* Can class [c1] see [c2] i.e. has [c2] been defined before in the same
     module or i   n a module which is used by the current module? *)
  let m1 = module_of_class c1 ct
  and m2 = module_of_class c2 ct in
  if Module.M.equal m1 m2 then
    c1 >= c2
  else if is_public ct then
    Module.M.uses_public m1 m2
  else
    Module.M.uses m1 m2



let can_see_all (c:int) (cs:IntSet.t) (ct:t): bool =
  IntSet.for_all (fun c2 -> can_see c c2 ct) cs



let dominant_class (tvs:Tvars.t) (s:Sign.t) (ct:t): int option =
  let classes = Sign.involved_classes tvs s in
  let owners =
    IntSet.fold
      (fun c owners ->
        if can_see_all c classes ct then
          IntSet.add c owners
        else
          owners
      )
      classes
      IntSet.empty
  in
  if IntSet.cardinal owners = 1 then
    Some (IntSet.choose owners)
  else
    None



let dominant_formal_generic (tvs:Tvars.t) (owner:int option) (ct:t): int option =
  match owner with
  | None ->
     None
  | Some owner ->
     let fgtps = Tvars.fgconcepts tvs
     and nfgs  = Tvars.count_fgs tvs in
     let fgs =
       interval_fold
         (fun fgs i ->
           match fgtps.(i) with
           | Variable j when nfgs <= j ->
              if j = owner + nfgs then
                IntSet.add i fgs
              else
                fgs
           | _ ->
              assert false (* Cannot happen with concepts *)
         )
         IntSet.empty
         0 nfgs
     in
     if IntSet.cardinal fgs = 1 then
       Some (IntSet.choose fgs)
     else
       None



let check_deferred  (owner:int option) (fg:int option) (info:info) (ct:t): unit =
  match owner with
  | None ->
     ()
  | Some owner ->
     let desc  = descriptor owner ct
     and bdesc = base_descriptor owner ct in
     (match bdesc.hmark with
        Deferred_hmark -> ()
      | _ ->
         error_info
           info
           ("The owner class " ^ (class_name owner ct) ^ " is not deferred")
     );
     let m = desc.mdl in
     if not (is_current_module m ct) then
       error_info
         info
         ("Can be defined only in the module \""
          ^ Module.M.string_of_name m
          ^ "\" of the owner class "
          ^ class_name owner ct)
     else if not (is_interface_check ct || IntSet.is_empty bdesc.descendants)
     then
       error_info
         info
            ("Owner class " ^ (class_name owner ct) ^" has already descendants")
     else if fg = None then
       error_info
         info
         ("There must be a unique formal generic anchored to the owner class " ^
            (class_name owner ct))





let ancestor (cls:int) (anc:int) (ct:t): parent_descriptor =
  let bdesc = base_descriptor cls ct in
  IntMap.find anc bdesc.ancestors


let is_ghost_ancestor (cls:int) (anc:int) (ct:t): bool =
  try
    (ancestor cls anc ct).is_ghost
  with Not_found ->
    assert false


let has_ancestor (cls:int) (anc:int) (ct:t): bool =
  (** Does the class [cls] have [anc] as an ancestor ? *)
  cls = anc ||
  try let _ = ancestor cls anc ct in true
  with Not_found -> false




let ancestor_type (tp:type_term) (anc_cls:int) (ntvs:int) (ct:t): type_term =
  (* The ancestor type of type [tp] with the ancestor class [anc_cls] in an
     environment with [ntvs] type variables *)
   assert (ntvs <= anc_cls);
   assert (anc_cls-ntvs < count ct);
   let cls,args = split_type tp in
   assert (ntvs <= cls);
   assert (cls-ntvs < count ct);
   let pargs = (ancestor (cls-ntvs) (anc_cls-ntvs) ct).actual_generics in
   if Array.length pargs = 0 then
     Variable anc_cls
   else
     let pargs = Array.map (fun tp -> Term.subst tp ntvs args) pargs in
     make_type anc_cls pargs



let satisfies
    (tp1:type_term) (tvs1:Tvars.t) (tp2:type_term) (tvs2:Tvars.t) (ct:t)
    : bool =
  (* Does [tp1] satisfy the concept [tp2] or the concept of the class variable
     in [tp2]? *)
  let nlocs1 = Tvars.count_local tvs1
  and nall1  = Tvars.count_all   tvs1
  and nlocs2 = Tvars.count_local tvs2
  and nall2  = Tvars.count_all   tvs2
  in
  let sat_tp1 c2 =
    (* Does [tp1] inherit class [c2]? *)
    let sat c1 = has_ancestor c1 c2 ct in
    match tp1 with
    | Variable i1 when i1 < nlocs1 ->
       false
    | Variable i1 when i1 < nall1 ->
       sat (Tvars.concept_class i1 tvs1)
    | Variable i1 ->
       sat (i1 - nall1)
    | Application (Variable i1, ags, _) when i1 < nall1 ->
       assert (nlocs1 <= i1);
       sat (Tvars.concept_class i1 tvs1)
    | Application (Variable i1, ags, _) ->
       sat (i1 - nall1)
    | _ ->
       assert false (* Not possible with types *)
  in
  match tp2 with
  | Variable i2 when i2 < nlocs2 ->
     true
  | Variable i2 when i2 < nall2 -> (* A class variable *)
     sat_tp1 (Tvars.concept_class i2 tvs2)
  | Variable i2 -> (* A concept *)
     sat_tp1 (i2 - nall2)
  | _ ->
     assert false (* Must be some type/class variable or a concept *)





let valid_type
    (cls_idx:int)
    (args: type_term array)
    (info: info)
    (tvs:  Tvars.t)
    (ct:t): type_term =
  (* The valid type term [cls_idx[args.(0),args.(1),...] in a type environment
     [tvs].

     If the type term is not valid then [Not_found] is raised.

     To check: Do all actual generics [args] satisfy their corresponding
     concepts? *)
  let ntvs  = Tvars.count tvs
  and nall  = Tvars.count_all tvs
  and nargs = Array.length args in
  assert (ntvs <= cls_idx);
  let cls_idx_0 =
    if cls_idx < nall then
      match Tvars.concept cls_idx tvs with
      | Variable i ->
         assert (nall <= i);
         i - nall
      | _ ->
         assert false (* Concept has to be a class *)
    else
      cls_idx - nall
  in
  let bdesc = base_descriptor cls_idx_0 ct in
  let fgconcepts = Tvars.fgconcepts bdesc.tvs in
  if nargs <> Array.length fgconcepts then
    error_info info "number of actual generics wrong";
  for i = 0 to nargs-1 do
    if satisfies args.(i) tvs fgconcepts.(i) bdesc.tvs ct then
      ()
    else
      error_info info ("actual generic #" ^ (string_of_int i)
                       ^ " " ^ (string_of_type args.(i) tvs ct)
                       ^ " of class " ^ (class_name cls_idx_0 ct)
                       ^ " does not satisfy the required concept "
                       ^ (string_of_type fgconcepts.(i) bdesc.tvs ct))
  done;
  if nargs = 0 then
    Variable cls_idx
  else
    make_type cls_idx args


let find_qualified (mnme:int) (pckg:library_name) (nme:int) (ct:t): int =
  let lst = IntMap.find nme ct.map in
  let lst =
    List.filter
      (fun c ->
        let open Module in
        let mc = module_of_class c ct in
        mnme = M.base_name mc
        && match pckg with
           | [] ->
              M.same_package mc (current_module ct)
           | _ ->
              pckg = M.package_name mc
      )
      lst
  in
  match lst with
  | [] ->
     raise Not_found
  | [c] ->
     c
  | _ ->
     assert false (* Qualified cannot be ambiguous *)

let class_index (path:int list) (name:int) (tvs:Tvars.t) (info:info) (ct:t): int =
  (* Find the class index/type variable index of [path.name] in the type
     environment [tvs].

     If the name is unqualified then classes of the same module or the same
     package have precedence.

     Multiple classes with the same name in the same module cannot exist. But
     a package can have multiple classes with the same name. I.e. different
     modules can declare different classes with the same name.

   *)
  let ntvs    = Tvars.count tvs
  and fgnames = Tvars.fgnames tvs
  and nall    = Tvars.count_all tvs
  in
  try (* If there is no path then first try to find the name in the
         formal generics *)
    if path = [] then
      ntvs + Search.array_find_min (fun n -> n=name) fgnames
    else
      raise Not_found
  with Not_found ->
    (* [name] is not a formal generic in tvs *)
    try
      match path with
      | [] ->
         begin
           try
             nall + find_unqualified name ct
           with Ambiguous lst ->
             let open Format in
             eprintf "@[<v>%s Ambiguous classname %s@,@,"
                     (info_string info)
                     (ST.string name);
             eprintf "The class name %s is ambiguous, it could be one of@,"
                     (ST.string name);
             List.iter
               (fun c ->
                 eprintf "@,  %s" (qualified_class_name c ct))
               lst;
             eprintf "@]@.";
             exit 1
         end
      | m::p ->
         nall + find_qualified m p name ct
    with Not_found ->
      error_info info ("Class " ^ (string_of_classname path name)
                       ^ " does not exist in this context")




let tuple_name     = ST.symbol "TUPLE"
let predicate_name = ST.symbol "PREDICATE"
let function_name  = ST.symbol "FUNCTION"
let sequence_name  = ST.symbol "SEQUENCE"
let list_name      = ST.symbol "LIST"


let get_type
    (tp:type_t withinfo)
    (deferred_allowed: bool)
    (tvs: Tvars.t)
    (ct:t)
    : term =
  (* Convert the syntactic type [tp] in an environment with the [tvs] type
     variables and the formal generics [fgnames,concepts] into a type term.

     [deferred_allowed] specifies wheather a deferred class is allowed at
     the top.

     Only visible classes can be used legally in a type!
   *)
  let class_index0 path (nme: int): int = class_index path nme tvs tp.i ct
  in
  let info = tp.i in
  let rec get_tp (top:bool) (tp:type_t): type_term =
    let valid_tp (idx:int) (args:type_term array): type_term =
        valid_type idx args info tvs ct
    in
    let rec tuple (tp_list: type_t list): type_term =
      let ta, tb =
        match tp_list with
          [tpa;tpb] ->
            get_tp false tpa, get_tp false tpb
        | tpa::tail ->
            get_tp false tpa, tuple tail
        | _ ->
            assert false (* tuple type must have at least two types *)
      in
      valid_tp (class_index0 [] tuple_name) [|ta;tb|]
    in
    match tp with
      Normal_type (path,name,actuals) ->
        let args = List.map (get_tp false) actuals in
        let args = Array.of_list args in
        let cls = class_index0 path name in
        let cls0 = cls - Tvars.count_all tvs in
        assert (cls0 < count ct);
        if not (deferred_allowed && top) && 0 <= cls0 && is_deferred cls0 ct then
          error_info
            info
            ("Deferred class " ^ class_name cls0 ct ^ " must not be used directly");
        valid_tp cls args
    | Paren_type tp ->
        get_tp false tp
    | Brace_type tp ->
        let t = get_tp false tp in
        valid_tp (class_index0 [] predicate_name) [|t|]
    | Star_type tp ->
        let t = get_tp false tp in
        valid_tp (class_index0 [] sequence_name) [|t|]
    | List_type tp ->
        let t = get_tp false tp in
        valid_tp (class_index0 [] list_name) [|t|]
    | Arrow_type (tpa,tpb) ->
        let ta = get_tp false tpa
        and tb = get_tp false tpb in
        valid_tp (class_index0 [] function_name) [|ta;tb|]
    | Tuple_type tp_lst ->
        tuple tp_lst
  in
  get_tp true tp.v





let inherits_any (cls:int) (ct:t): bool =
  cls <> Constants.any_class &&
  has_ancestor cls Constants.any_class ct


let descends_from_any (cls:int) (ct:t): bool =
  has_ancestor cls Constants.any_class ct


let type_descends_from_any (tp:term) (tvs:Tvars.t) (ct:t): bool =
  let cls = Tvars.principal_class tp tvs in
  descends_from_any cls ct


let parent_type (cls:int) (tvs:Tvars.t) (tp:type_t withinfo) (ct:t)
    : int * type_term array =
  assert (cls < count ct);
  let tp_term = get_type tp true tvs ct
  and n = Tvars.count_all tvs
  in
  let i, args = split_type tp_term
  in
  if i < n then
    error_info tp.i "Formal generic not allowed as parent class";
  i-n, args





let one_inherit
    (cls_idx: int)
    (cls_bdesc:base_descriptor)
    (anc_idx: int)
    (anc: parent_descriptor)
    (anc_bdesc:base_descriptor)
    : unit =
  cls_bdesc.ancestors <-
    IntMap.add anc_idx anc cls_bdesc.ancestors;
  assert (not (IntSet.mem cls_idx anc_bdesc.descendants));
  anc_bdesc.descendants <- IntSet.add cls_idx anc_bdesc.descendants



let rec inherit_parent
    (cls:int) (par:int) (args:type_term array) (ghost:bool)
    (info:info) (ct:t): unit =
  (* Inherit the parent [par,args] in the class [cls] and in the descendants of
     [cls]. *)
  let par_bdesc      = base_descriptor par ct
  and cls_bdesc      = base_descriptor cls ct in
  let cls_nfgs  = Tvars.count_fgs cls_bdesc.tvs in
  let inherit_ancestor anc anc_args is_ghost anc_bdesc cls_bdesc =
    try
      let pdesc = IntMap.find anc cls_bdesc.ancestors in
      if anc_args <> pdesc.actual_generics then
        error_info info ("Cannot inherit " ^ (class_name anc ct) ^
                         " in " ^ (class_name cls ct) ^
                         " with different actual generics")
      else if ghost <> pdesc.is_ghost then
        error_info info ("Cannot change ghost status of " ^ (class_name anc ct) ^
                         " in " ^ (class_name cls ct))
      else
        () (* ancestor already consistently available *)
    with Not_found ->
      let adesc =
        {is_ghost = ghost; actual_generics = anc_args} in
      one_inherit cls cls_bdesc anc adesc anc_bdesc
  in
  IntMap.iter
    (fun anc adesc ->
      let anc_args = Array.map
          (fun t -> Term.subst t cls_nfgs args)
          adesc.actual_generics in
      let anc_bdesc = base_descriptor anc ct in
      inherit_ancestor anc anc_args adesc.is_ghost anc_bdesc cls_bdesc)
    par_bdesc.ancestors;
  IntSet.iter
    (fun desc ->
      let adesc = ancestor desc cls ct in
      inherit_parent desc cls adesc.actual_generics adesc.is_ghost info ct)
    cls_bdesc.descendants





let put_formal (name: int withinfo) (cidx:int) (ct:t): unit =
  (** Add the formal generic with [name] based on the class [cidx] to the
      formal generics of the class table [ct] *)
  assert (cidx < count ct);
  if IntMap.mem name.v ct.fgens then
    error_info
      name.i
      ("formal generic " ^ (ST.string name.v) ^ " already defined")
  else
    ct.fgens <- IntMap.add name.v (Variable cidx) ct.fgens



let tvs_of_class_for_parent (cls:int) (ct:t): Tvars.t =
  (base_descriptor cls ct).tvs

let check_base_descriptor
    (hm:    header_mark withinfo)
    (cv:    int withinfo option)
    (tvs:   Tvars.t)
    (desc:  base_descriptor)
    (cidx:  int)
    (update: bool)
    (ct:    t)
    : unit =
  if hm.v <> desc.hmark then
    (let str =
      "Header mark should be \""
      ^ (hmark2string desc.hmark)
      ^ "\"\n"
    in
    error_info hm.i str);
  assert (not (Option.has cv) || hm.v = Deferred_hmark);
  if not (Tvars.is_equivalent desc.tvs tvs) then
    begin
      let str =
        "The formal generics are not consistent with previous declaration\n" ^
          "   previous declaration \"" ^ (string_of_tvs desc.tvs ct)
          ^ "\""
      in error_info hm.i str;
    end;
  if update then
    begin
      desc.tvs <- tvs;
      match cv with
      | None ->
         ()
      | Some cv ->
         put_formal cv cidx ct;
         desc.cvar <- cv.v
    end




let check_class
    (idx:   int)
    (hm:    header_mark withinfo)
    (cv:    int withinfo option)
    (tvs:   Tvars.t)
    (ct:    t)
    : unit =
  check_base_descriptor hm cv tvs (base_descriptor idx ct) idx false ct


let update
    (idx:   int)
    (info:info)
    (hm:    header_mark withinfo)
    (cv:    int withinfo option)
    (tvs:   Tvars.t)
    (ct:    t)
    : unit =
  let desc = Seq.elem idx ct.seq
  and mdl  = current_module ct
  in
  if Module.M.equal desc.mdl mdl then
    begin
      check_base_descriptor hm cv tvs desc.bdesc idx true ct;
      if not desc.is_exp then
        export idx hm tvs ct
    end
  else
    begin
      (* This error handling shall never happen *)
      let open Format in
      eprintf
        "%s %s %s %s \"%s\" %s@."
        (info_string info)
        "The class"
        (class_name idx ct)
        "belongs to module"
        (Module.M.string_of_name desc.mdl)
        "You cannot redeclare it in another module";
      exit 1
    end







let add
    (hm:    header_mark withinfo)
    (cv:    int withinfo option)
    (cn:    int)
    (tvs:   Tvars.t)
    (ct:    t)
    : unit =
  let idx  = count ct in
  let cvar =
    match cv with
    | None ->
       begin
         match hm.v with
         | Deferred_hmark ->
            ST.symbol "CV"
         | _ ->
            -1
       end
    | Some cv ->
       assert (hm.v = Deferred_hmark);
       cv.v
  in
  let bdesc = standard_bdesc hm.v cvar tvs idx
  and is_exp = is_interface_public_use ct
  in
  Seq.push
    {mdl  = current_module ct;
     name = cn;
     ident = idx;
     is_exp = is_exp;
     indlaws = [];
     wflaws  = [];
     rec_pairs = [];
     can_match_pattern = false;
     bdesc = bdesc}
    ct.seq;
  add_to_map idx ct;
  begin
    match cv with
    | None -> ()
    | Some cv ->
       put_formal cv  idx ct
  end



let add_fg
    (name:int)
    (path: int list)
    (fgs: formal list)
    (tvs:Tvars.t)
    (ct:t)
    : formal list =
  (* Check if [name] is a new formal generic. If yes prepend it to [fgs].

     Note: [fgs] is reversed *)
  if path = [] &&
    not (List.exists (fun (nme,_)-> nme=name) fgs) &&
    not (Tvars.has_fg name tvs)
  then
    try
      let cpt = IntMap.find name ct.fgens in
      (name,cpt) :: fgs
    with Not_found ->
      fgs
  else
    fgs


let collect_fgs
    (tp:type_t)
    (fgs:formal list)
    (tvs:Tvars.t)
    (ct:t)
    : formal list =
  (* Collect the formal generics of the type [tp] and prepend them to [fgs] if
     they are new.

     Note: [fgs] is reversed *)
  let rec collect (tp:type_t) (fgs:formal list): formal list =
    match tp with
      Normal_type (path,name,args) ->
        let fgs = List.fold_left
            (fun lst tp -> collect tp lst)
            fgs
            args
        in
        add_fg name path fgs tvs ct
    | Arrow_type (tpa,tpb) ->
        collect tpb (collect tpa fgs)
    | Brace_type tp | Star_type tp | Paren_type tp | List_type tp ->
        collect tp fgs
    | Tuple_type lst ->
        List.fold_left (fun fgs tp -> collect tp fgs) fgs lst
  in
  collect tp fgs



let formal_generics
    (entlst:   entities list withinfo)
    (rt:       return_type)
    (is_func:  bool)
    (tvs:      Tvars.t)
    (ct:       t)
    : Tvars.t =
  let ntvs_new,fgs_new =
    List.fold_left
      (fun (ntvs,fgs) ent ->
        match ent with
          Untyped_entities vars ->
            ntvs + List.length vars, fgs
        | Typed_entities (_,tp) ->
            ntvs, collect_fgs tp.v fgs tvs ct)
      (0,[])
      entlst.v
  in
  let ntvs_new, fgs_new =
    match rt with
      None when is_func -> ntvs_new + 1, fgs_new
    | None -> ntvs_new, fgs_new
    | Some tp ->
        let t,_,_ = tp.v in
        ntvs_new, collect_fgs t fgs_new tvs ct
  in
  let fgs_new = Array.of_list (List.rev fgs_new) in
  let fgnames,fgconcepts = Myarray.split fgs_new in
  let nfgs_new = Array.length fgconcepts in
  let fgconcepts = Array.map (fun tp -> Term.up nfgs_new tp) fgconcepts in
  let tvs = Tvars.add_local ntvs_new tvs in
  Tvars.augment_fgs fgnames fgconcepts tvs




let class_formal_generics (fgens: formal_generics) (ct:t): formal array =
  Array.of_list
    (List.map
       (fun nme ->
         try
           nme, IntMap.find nme ct.fgens
         with Not_found ->
           let str = "Unknown formal generic " ^ (ST.string nme) in
           error_info fgens.i str)
       fgens.v)


let class_tvs
      (fgens:formal_generics) (ct:t): Tvars.t =
  let fgs = class_formal_generics fgens ct in
  let nfgs = Array.length fgs in
  let fgnms, fgcon = Myarray.split fgs in
  let fgcon = Array.map (Term.up nfgs) fgcon in
  Tvars.make_fgs fgnms fgcon




let formal_arguments
    (entlst: entities list withinfo)
    (tvs:Tvars.t)
    (ct:t)
    : formal list * int =
  (* The formal arguments of the entity list [entlst] in the type environment [tvs]
   *)
  let rec check_duplicates arglst =
    match arglst with
      [] -> ()
    | (nme,_)::tail ->
        if List.exists (fun (n,_) -> n = nme) tail then
          error_info entlst.i ("Duplicate formal argument " ^ (ST.string nme))
        else
          check_duplicates tail
  in
  let n_untyped = ref 0 in
  let fargs (es: entities): formal list =
    match es with
      Untyped_entities lst ->
        if Tvars.count_local tvs < List.length lst then
          error_info entlst.i "Untyped arguments not allowed here";
        assert (List.length lst <= Tvars.count_local tvs);
        assert (!n_untyped = 0);
        n_untyped := List.length lst;
        List.mapi (fun i name -> name, Variable i) lst
    | Typed_entities (lst,tp) ->
        let t = get_type tp false tvs ct in
        List.map (fun name -> name,t) lst
  in
  let arglst = List.concat (List.map fargs entlst.v) in
  check_duplicates arglst;
  arglst, !n_untyped



let result_type
    (rt:return_type)
    (is_pred: bool)
    (is_func: bool)
    (n_untyped: int)
    (tvs:Tvars.t)
    (ct:t)
    : Result_type.t =
  (** The result type which corresponds to the return type [rt] in an
      environment with the formal generics [fgnames,concepts] and [ntvs] type
      variables *)
  match rt with
    None when is_pred ->
      let t = boolean_type (Tvars.count_all tvs) in
      Result_type.make t false false
  | None when is_func ->
      assert (n_untyped < Tvars.count_local tvs);
      Result_type.make (Variable n_untyped) false false
  | None ->
      Result_type.empty
  | Some tpinf ->
      let tp,proc,ghost = tpinf.v in
      let t =
        get_type (withinfo tpinf.i tp) false tvs ct
      in
      Result_type.make t proc ghost


let analyze_signature
    (entlst: entities list withinfo)
    (rt:return_type)
    (is_pred: bool)
    (is_func: bool)
    (rvar: bool)
    (tvs:Tvars.t)
    (ct:t): formal array * Result_type.t  =
  (*  The variable with names and types and the result type of [entlst,rt]
   *)
  let arglst, n_untyped = formal_arguments entlst tvs ct in
  let rt = result_type rt is_pred is_func n_untyped tvs ct in
  let arglst =
    if rvar then begin
      assert (Result_type.has_result rt);
      arglst @ [ST.symbol "Result", Result_type.result rt]
    end else
      arglst
  in
  Array.of_list arglst, rt



let empty_table (comp:Module.Compile.t): t =
  let cc = Seq.empty ()
  in
  {map   = IntMap.empty;
   seq   = cc;
   fgens = IntMap.empty;
   base  = IntMap.empty;
   comp = comp}


let tvars_with_class_variable
      (hm:header_mark) (cv:int option ) (tvs:Tvars.t) (cls:int)
    : Tvars.t =
  match hm with
  | Deferred_hmark ->
     let nme =
       match cv with
       | None ->
          ST.symbol "CV"
       | Some cv ->
          cv
     in
     Tvars.augment_fgs [|nme|] [|Variable cls|] tvs
  | _ ->
     tvs


let add_base_class
    (mdl_name: string)
    (name:string)
    (hm:  header_mark)
    (fgs: formal array)
    (ct:t)
    : unit =
  let idx  = count ct
  and nfgs = Array.length fgs
  and nme  = ST.symbol name
  and fgnames,concepts = Myarray.split fgs
  in
  let concepts = Array.map (fun tp -> Term.up nfgs tp) concepts in
  let tvs  = Tvars.make_fgs fgnames concepts in
  let cvar =
    match hm with
    | Deferred_hmark ->
       ST.symbol "CV"
    | _ ->
       -1
  in
  let bdesc = standard_bdesc hm cvar tvs idx
  in
  Seq.push
    {mdl = core_module ct;
     name = nme;
     ident = idx;
     is_exp = (name = "@DUMMY");
     indlaws = [];
     wflaws  = [];
     rec_pairs = [];
     can_match_pattern = false;
     bdesc = bdesc}
    ct.seq;
  let mdl_nme = ST.symbol mdl_name in
  begin
    try
      let lst = IntMap.find mdl_nme ct.base in
      assert (not (List.mem idx lst));
      ct.base <- IntMap.add mdl_nme (idx::lst) ct.base
    with Not_found ->
      ct.base <- IntMap.add mdl_nme [idx] ct.base
  end




let check_base_classes (ct:t): unit =
  assert (Constants.tuple_class < (count ct));
  assert ((class_name Constants.boolean_class   ct) = "BOOLEAN");
  assert ((class_name Constants.any_class       ct) = "ANY");
  assert ((class_name Constants.dummy_class     ct) = "@DUMMY");
  assert ((class_name Constants.predicate_class ct) = "PREDICATE");
  assert ((class_name Constants.function_class  ct) = "FUNCTION");
  assert ((class_name Constants.tuple_class     ct) = "TUPLE");
  assert ((class_name Constants.sequence_class  ct) = "SEQUENCE");
  assert ((class_name Constants.list_class      ct) = "LIST");
  ()




let base_table (comp:Module.Compile.t): t =
  let fgg = ST.symbol "G"
  and fga = ST.symbol "A"
  and fgb = ST.symbol "B"
  and anycon = Variable Constants.any_class
  and ct = empty_table (comp)   in
  add_base_class "core"      "BOOLEAN"   No_hmark        [||] ct;
  add_base_class "core"      "ANY"       Deferred_hmark  [||] ct;
  add_base_class "@dummy"    "@DUMMY"    No_hmark        [||] ct;
  add_base_class "core"      "PREDICATE" No_hmark        [|fgg,anycon|] ct;
  add_base_class "core"      "FUNCTION"  No_hmark
                 [|(fga,anycon);(fgb,anycon)|] ct;
  add_base_class "core"      "TUPLE"     No_hmark
                 [|(fga,anycon);(fgb,anycon)|] ct;
  add_base_class "core"      "SEQUENCE"  No_hmark        [|fga,anycon|] ct;
  add_base_class "core"      "LIST"      No_hmark        [|fga,anycon|] ct;
  check_base_classes ct;
  ct





let string_of_signature
    (s:Sign.t)
    (tvs:Tvars.t)
    (ct:t)
    : string =
  let has_args = (Sign.arity s) <> 0
  and has_res  = Sign.has_result s
  in
  let argsstr =
    if not has_args then ""
    else
      "("
      ^ (String.concat
           ","
           (List.map
              (fun tp -> string_of_type tp tvs ct)
              (Array.to_list (Sign.arguments s))))
      ^ ")"
  and retstr =
    if has_res then
      string_of_type (Sign.result s) tvs ct
    else ""
  and colon = if has_args && has_res then ":" else ""
  in
  argsstr ^ colon ^ retstr


let string_of_complete_signature
    (s:Sign.t)
    (tvs:Tvars.t)
    (ct:t)
    : string =
  (string_of_tvs tvs ct) ^ (string_of_signature s tvs ct)



let string_of_detailed_tvs (tvs:Tvars.t) (ct:t): string =
  let nall = Tvars.count_all tvs in
  if nall = 0 then
    ""
  else
    let nlocs  = Tvars.count_local tvs
    and nglobs = Tvars.count_global tvs in
    let arr = Array.init nall
        (fun i ->
          if i < nlocs then
            (string_of_int i) ^ ":_"
          else if i < nlocs + nglobs then
            (string_of_int i) ^ ":" ^ (string_of_type (Tvars.concept i tvs) tvs ct)
          else
            (ST.string (Tvars.name i tvs)) ^ ":" ^
            (string_of_type (Tvars.concept i tvs) tvs ct)) in
    "[" ^
    (String.concat "," (Array.to_list arr)) ^
    "] "



let transformed (nall:int) (tvset:IntSet.t) (down_from: int->int->'a->'a) (x:'a)
    : 'a =
  let i0,n,x =
    IntSet.fold
      (fun i (i0,n,x) ->
        if i = i0 + n then
          i0+1, n, x
        else begin
          assert (i0 + n < i);
          let ndelta = i - i0 - n in
          let nnew   = n + ndelta in
          i0+1, nnew, down_from ndelta i0 x
        end)
      tvset
      (0,0,x) in
  down_from (nall-i0-n) i0 x


let string_of_reduced_complete_signature
    (s:Sign.t)
    (tvs: Tvars.t)
    (ct:t): string =
  let nall   = Tvars.count_all tvs
  and nlocs  = Tvars.count_local tvs
  and nglobs = Tvars.count_global tvs
  in
  let foldfun set tp = IntSet.union set (Term.bound_variables tp nall) in
  let tvset = Sign.fold foldfun IntSet.empty s in
  let tvset =
    IntSet.fold
      (fun i set ->
        if i < nlocs then set
        else IntSet.union set (Term.bound_variables (Tvars.concept i tvs) nall))
      tvset
      tvset in
  let _,tvmap =
    IntSet.fold
      (fun i (n,set) -> n+1, IntMap.add n i set)
      tvset
      (0,IntMap.empty) in
  let nlocs0,nglobs0,nfgs0 =
    IntSet.fold
      (fun i (nlocs0,nglobs0,nfgs0) ->
        if i < nlocs then
          nlocs0+1, nglobs0, nfgs0
        else if i < nlocs+nglobs then
          nlocs0, nglobs0+1, nfgs0
        else
          nlocs0, nglobs0, nfgs0+1)
      tvset
      (0,0,0)
  in
  let s0 = transformed nall tvset Sign.down_from s
  in
  let concepts = Array.init nglobs0
      (fun i ->
        let idx = IntMap.find (i+nlocs0) tvmap in
        let tp = Tvars.concept idx tvs in
        transformed nall tvset Term.down_from tp)
  and fgconcepts = Array.init nfgs0
      (fun i ->
        let idx = IntMap.find (i+nlocs0+nglobs0) tvmap in
        let tp = Tvars.concept idx tvs in
        transformed nall tvset Term.down_from tp)
  and fgnames = Array.init nfgs0
      (fun i ->
        let idx = IntMap.find (i+nlocs0+nglobs0) tvmap in
        Tvars.name idx tvs) in
  let tvs0 = Tvars.make nlocs0 concepts (Formals.make fgnames fgconcepts) in
  (string_of_detailed_tvs tvs0 ct) ^
  (string_of_signature s0 tvs0 ct)


let verify_substitution
    (tps1:types) (tvs1:Tvars.t)
    (tps2:types) (tvs2:Tvars.t): unit =
  (* Verify that the [tps1] from the environment [tvs1] can be substituted by
     the types [tps2] from the environment [tvs2].

     Both environments must have the same formal generics, must not have global
     type variables and might differ in the number of local type variables.
   *)
  let ntps = Array.length tps1 in
  assert (ntps = Array.length tps2);
  assert (Tvars.count_fgs tvs1 = Tvars.count_fgs tvs2);
  assert (Tvars.count_global tvs1 = 0);
  assert (Tvars.count_global tvs2 = 0);
  let nlocs1 = Tvars.count_local tvs1
  and nlocs2 = Tvars.count_local tvs2
  in
  let maxlocs = max nlocs1 nlocs2 in
  let up1 = maxlocs - nlocs1
  and up2 = maxlocs - nlocs2 in
  let ok =
    interval_for_all (* For higher performance consider unification *)
      (fun i -> Term.up up1 tps1.(i) = Term.up up2 tps2.(i))
      0 ntps
  in
  if not ok then
    raise Not_found
  else
    ()
