(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Container
open Term

type t = {
    nlocal:int;                  (* local type variables without concept *)
    concepts: type_term array;   (* global type variables with concept
                                    coming from used functions           *)
    fgs: Formals.t               (* Formal generics *)
  }


let empty: t =
  {nlocal=0;concepts=[||];fgs = Formals.empty}

let make
    (nlocs:int)
    (concepts:type_term array)
    (fgs: Formals.t)
    : t =
  {nlocal = nlocs;
   concepts;
   fgs}


let copy (tvs:t): t =
  {nlocal   = tvs.nlocal;
   concepts = Array.copy tvs.concepts;
   fgs      = Formals.copy tvs.fgs}

let count_fgs (tvs:t): int = Formals.count tvs.fgs


let fgconcepts (tvs:t): type_term array = Formals.types tvs.fgs


let fgnames (tvs:t): int array = Formals.names tvs.fgs


let has_fg (name:int) (tvs:t): bool =
  try
    let _ = Search.array_find_min (fun n -> n=name) (fgnames tvs) in
    true
  with Not_found ->
    false


let make_fgs (nms: int array) (cpts:type_term array): t =
  {nlocal=0;concepts=[||];fgs = Formals.make nms cpts}


let count_local (tvs:t): int = tvs.nlocal

let count_global (tvs:t): int = Array.length tvs.concepts

let count (tvs:t): int    = tvs.nlocal + count_global tvs

let count_all(tvs:t): int = tvs.nlocal + count_global tvs + count_fgs tvs

let is_empty (tvs:t): bool =
  count_all tvs = 0

let has_no_variables (tvs:t): bool =
  count tvs = 0

let has_variables (tvs:t): bool =
  count tvs > 0

let has_no_formal_generics (tvs:t): bool =
  count_fgs tvs = 0

let has_formal_generics (tvs:t): bool =
  count_fgs tvs > 0



let concept (i:int) (tvs:t): type_term =
  assert (count_local tvs <= i);
  assert (i < count_all tvs);
  if i < count tvs then
    tvs.concepts.(i - count_local tvs)
  else
    Formals.typ (i - count tvs) tvs.fgs


let concept_class (i:int) (tvs:t): int =
  let nall = count_all tvs in
  assert (count_local tvs <= i);
  assert (i < nall);
  match concept i tvs with
  | Variable j ->
     assert (nall <= j);
     j - nall
  | _ ->
     assert false (* Concept must be a class *)



let name (i:int) (tvs:t): int =
  assert (count tvs <= i);
  Formals.name (i - count tvs) tvs.fgs

let concepts (tvs:t): type_term array = tvs.concepts


let is_equivalent (tvs1:t) (tvs2:t): bool =
  tvs1.nlocal =  tvs2.nlocal
  && Term.equivalent_array tvs1.concepts tvs2.concepts
  && Formals.is_equivalent tvs1.fgs tvs2.fgs


let is_equal (tp1:type_term) (tvs1:t) (tp2:type_term) (tvs2:t): bool =
  (* Are the types [tp1,tvs1] and [tp2,tvs2] equal in the sense that they have
     identical structures and each absolute used type is identical in both and
     if formal generics occur then their concepts are equal? *)
  let nall1 = count_all tvs1
  and nall2 = count_all tvs2
  and nloc1 = count_local tvs1
  and nloc2 = count_local tvs2
  in
  let rec is_eq (tp1:type_term) (tp2:type_term) (nmax:int): bool =
    (* nmax: infinite recursion protection *)
    match tp1, tp2 with
      Variable i, Variable j when i < nloc1 || j < nloc2 ->
        false
    | Variable i, Variable j when i < nall1 && j < nall2 ->
        assert (0 < nmax);
        is_eq (concept i tvs1) (concept j tvs2) (nmax-1)
    | Variable i, Variable j when nall1 <= i && nall2 <= j ->
        (i - count_all tvs1) = (j - count_all tvs2)
    | Application (Variable i,args1,_), Application(Variable j,args2,_) ->
        let n1 = Array.length args1
        and n2 = Array.length args2 in
        let res = ref (n1 = n2 && i = j) in
        begin
          try
            for k = 0 to n1-1 do
              res := !res && is_eq args1.(k) args2.(k) nmax;
            if not !res then raise Not_found
            done;
            true
          with Not_found -> false
        end
    | _ -> false
  in
  is_eq
    tp1
    tp2
    (let n1,n2 = nall1-nloc1,nall2-nloc2 in
    if n1<=n2 then n2 else n1)



let is_equal_or_fg (tp1:type_term) (tvs1:t) (tp2:type_term) (tvs2:t): bool =
  (* Is the type [tp1,tvs1] equal to [tp2,tvs2] or is tp1 a formal generic and
     its concept is equal to [tp2,tvs2]? *)
  match tp1 with
    Variable i when count_local tvs1 <= i && i < count_all tvs1 ->
      is_equal (concept i tvs1) tvs1 tp2 tvs2
  | _ -> is_equal tp1 tvs1 tp2 tvs2



let principal_variable (tp:type_term) (tvs:t): int =
  let nloc = count_local tvs
  and nall = count_all tvs
  in
  let rec pvar (tp:type_term): int =
    match tp with
      Variable i when i < nloc || nall <= i ->
        i
    | Variable i when i < nall ->
        pvar (concept i tvs)
    | Variable i ->
        i
    | Application (Variable i,_,_) ->
        pvar (Variable i)
    | _ ->
        assert false
  in
  pvar tp


let principal_class (tp:type_term) (tvs:t): int =
  let nall = count_all tvs in
  let pvar = principal_variable tp tvs in
  if nall <= pvar then
    pvar - nall
  else
    assert false

let dummy_fgnames (n:int): int array =
  Array.init
    n
    (fun i ->
      let str = "@" ^ (string_of_int i) in
      Support.ST.symbol str)


let add_local (n:int) (tvs:t): t =
  {nlocal     = tvs.nlocal + n;
   concepts   = Array.map (fun tp -> Term.up n tp) tvs.concepts;
   fgs = Formals.map (Term.up n) tvs.fgs}





let remove_local (n:int) (tvs:t): t =
  assert (n <= (count_local tvs));
  try
    {nlocal     = tvs.nlocal - n;
     concepts   = Array.map (Term.down n) tvs.concepts;
     fgs = Formals.map (Term.down n) tvs.fgs}
  with Term_capture ->
    assert false (* cannot happen *)


let push_fgs (fgs:Formals.t) (tvs:t): t =
  assert (count_global tvs = 0);
  let nfgs1 = Formals.count fgs in
  if nfgs1 = 0 then
    copy tvs
  else
    begin
      assert (count_local tvs = 0);
      {nlocal = 0;
       concepts = [||];
       fgs =
         Formals.prepend
           fgs
           (Formals.map (Term.up nfgs1) tvs.fgs)}
    end




let augment_fgs
    (fgnames: int array)
    (fgconcepts:type_term array)
    (tvs:t): t =
  (* fgconcepts are in their own type environment *)
  let nfgs1 = Array.length fgconcepts in
  assert (Array.length fgnames = nfgs1);
  let cnt = count tvs
  and nfgs0 = count_fgs tvs in
  let fgs0 = Formals.map (Term.up_from nfgs1 cnt) tvs.fgs
  and concepts =
    Array.map (fun tp -> Term.up_from nfgs1 cnt tp) tvs.concepts
  and fgconcepts1 =
    Array.map (fun tp -> Term.up_from nfgs0 nfgs1 tp) fgconcepts
  in
  let fgconcepts1 = Array.map (fun tp -> Term.up cnt tp) fgconcepts1 in
  {tvs with
    concepts;
    fgs = Formals.prepend (Formals.make fgnames fgconcepts1) fgs0}




let fgs_to_global (tvs:t):t =
  assert (count tvs = 0);
  {nlocal   = 0;
   concepts = fgconcepts tvs;
   fgs = Formals.empty}




let add_involved_classes (tp:type_term) (tvs:t) (set0:IntSet.t): IntSet.t =
  let nloc = count_local tvs
  and nall = count_all   tvs in
  let rec clss (tp:type_term) (set0:IntSet.t) (n:int): IntSet.t =
    assert (0 <= n);
    Term.fold
      (fun set i ->
        if i < nloc then
          set
        else if i < nall then
          clss (concept i tvs) set (n-1)
        else
          IntSet.add (i-nall) set
      )
      set0
      tp
  in
  clss tp set0 (count_all tvs)


let involved_classes (tp:type_term) (tvs:t): IntSet.t =
  add_involved_classes tp tvs IntSet.empty


let is_class_involved (cls:int) (tp:type_term) (tvs:t): bool =
  IntSet.mem cls (involved_classes tp tvs)
