(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Support
open Container

module Option = Fmlib.Option


type term =
    Variable    of int
  | VAppl       of int * arguments * arguments * bool (* fidx, args, ags, oo *)
  | Application of term * arguments * bool (* fterm, args, inop *)
  | Lam         of formals * formals * term list * term * type_term option
  | QExp        of formals * formals * term * bool (* args, fgs, t, is_all *)
  | Ifexp       of term * term * term
  | Asexp       of term * types * term
  | Inspect     of term * (formals * term * term) array
  | Indset      of int * type_term * arguments (* name, type, rules *)
and names      = int array
and arguments  = term array
and agens      = type_term array
and types      = type_term array
and formals0   = names * arguments
and formals   = (int,type_term) Array2.t
and type_term  = term
and info_term  = term withinfo

exception Term_capture
exception Empty_term

module TermSet = Set.Make(struct
  let compare = Pervasives.compare
  type t = term
end)

module TermMap = Map.Make(struct
  let compare = Pervasives.compare
  type t = term
end)



let empty_term:    term = Variable (-1)

let empty_formals: formals0 = [||], [||]


let standard_substitution (n:int): term array =
  assert (0 <= n);
  Array.init n (fun i -> Variable i)

let is_standard_substitution (args:term array): bool =
  interval_for_all (fun i -> args.(i) = Variable i) 0 (Array.length args)

let make_type (cls:int) (ags:arguments): type_term =
  Application (Variable cls, ags, false)


let split_type (tp:type_term): int * agens =
  match tp with
  | Variable i ->
     i, [||]
  | Application(Variable i,args,_) ->
     i,args
  | _ -> assert false (* other cases not possible with types *)




let count_formals ((nms,tps):formals0): int =
  let n = Array.length nms in
  assert (n = Array.length tps);
  n

let string_of_names (nms:names): string =
  "(" ^
  (String.concat ","
     (List.map ST.string (Array.to_list nms))) ^
  ")"



module Term = struct

  let is_variable_i (t:term) (i:int): bool =
    match t with
      Variable j when i=j -> true
    | _                   -> false

  let is_permutation (a:term array): bool =
    let n = Array.length a in
    let p = Array.make n true in
    try
      for i = 0 to n - 1 do
        match a.(i) with
        | Variable j when j < n ->
           if p.(j) then
             p.(j) <- false
           else
             raise Not_found
        | _ ->
           raise Not_found
      done;
      true
    with Not_found ->
      false

  let invert_permutation (arr:term array): term array =
    let n = Array.length arr in
    let arr_perm = Array.make n empty_term in
    for i = 0 to n - 1 do
      match arr.(i) with
      | Variable j ->
         assert (j < n);
         assert (arr_perm.(j) = empty_term);
         arr_perm.(j) <- Variable i
      | _ ->
         assert false (* illegal call *)
    done;
    arr_perm



  let rec to_string (t:term): string =
    let argsstr nargs names =
      let nnames = Array.length names in
      assert (nnames=0 || nnames=nargs);
      let args = Array.init nargs string_of_int in
      String.concat "," (Array.to_list args)
    in
    let strlam nargs names pres t pred =
      let argsstr = string_of_int 0 in
      if pred then begin
        assert (pres = []); (* predicates don't have preconditions *)
        "{" ^ argsstr ^ ": " ^ (to_string t) ^ "}"
      end else
        match pres with
          [] -> "((" ^ argsstr ^ ")->" ^ (to_string t) ^ ")"
        | _ ->
            let presstr = String.concat ";" (List.map to_string pres) in
            "(agent(" ^ argsstr ^ ") require " ^
            presstr ^
            " ensure -> " ^ (to_string t) ^ " end)"
    in
    match t with
      Variable i -> string_of_int i
    | VAppl (i,args,_,_) ->
        let fstr = string_of_int i
        and argsstr = Array.to_list (Array.map to_string args)
        in
        fstr ^ "v(" ^
        (String.concat "," argsstr)
        ^ ")"
    | Application (f,args,_) ->
        let fstr = to_string f
        and argsstr = Array.to_list (Array.map to_string args)
        in
        fstr ^ "(" ^
        (String.concat "," argsstr)
        ^ ")"
    | Lam(tps,_,pres,t,rt) ->
       let nargs = Array2.count tps
       and names = Array2.first tps
       and pred  = (rt = None) in
       strlam nargs names pres t pred
    | QExp (tps,_,t,is_all) ->
       let nargs = Array2.count tps
       and names = Array2.first tps in
       let argsstr = argsstr nargs names in
        let qstr    = if is_all then "all" else "some" in
        qstr ^ "(" ^ argsstr ^ ") " ^ (to_string t)
    | Ifexp (cond,e1,e2) ->
       "(if " ^ to_string cond ^ " then " ^ to_string e1
       ^ " else " ^ to_string e2 ^ ")"
    | Asexp (e,_,pat) ->
       "( "  ^ to_string e ^ " as "  ^ to_string pat ^ ")"
    | Inspect (insp,cases) ->
       "(inspect "  ^ to_string insp
       ^ (String.concat
            ","
            (Array.to_list
               (Array.map (fun (_,pat,res) ->
                    " case " ^ to_string pat ^ " then " ^ to_string res
                  ) cases)))
       ^ ")"
    | Indset (n,nms,rs) ->
        "{(" ^ (string_of_int n) ^ "):"
        ^ (String.concat "," (List.map to_string (Array.to_list rs)))
        ^ "}"



  let variable (t:term): int =
    match t with
      Variable i -> i
    | _ -> raise Not_found


  let is_variable (t:term): bool =
    try
      let _ = variable t in
      true
    with Not_found ->
      false


  let is_variable_below (n:int) (t:term): bool =
    try
      let i = variable t in
      i < n
    with Not_found ->
      false


  let is_argument (t:term) (nargs:int): bool =
    try
      let i = variable t in i < nargs
    with Not_found -> false

  let nodes0 (t:term) (nb:int) (cargs:int array): int =
    (* The number of nodes in the term [t] which has [nb] bound variables,
       [#cargs] number of arguments. The arguments are substituted by terms
       which have a node count corresponding to the array [cargs].
     *)
    let nargs = Array.length cargs in
    let rec nds t nb =
      let ndsarr n args =
        Array.fold_left (fun sum t -> sum + nds t nb) n args
      in
      match t with
        Variable i when nb <= i && i < nb + nargs ->
          cargs.(i - nb)
      | Variable _ ->
          1
      | VAppl (i,args,_,_) ->
          ndsarr 1 args
      | Application (f,args,_) ->
          ndsarr (nds f nb) args
      | Lam (tps,_,pres,t,_) ->
         1 + nds t (Array2.count tps + nb) (* preconditions are not counted *)
      | QExp (tps,_,t,_) ->
          1 + nds t (Array2.count tps + nb)
      | Ifexp (c,e1,e2) ->
         1 + nds c nb + nds e1 nb + nds e2 nb
      | Asexp (t,tps,pat) ->
         1 + nds t nb + nds pat (nb + Array.length tps)
      | Inspect (insp,cases) ->
         1 + nds insp nb +
           Array.fold_left
             (fun sum (fs,pat,res) ->
               let nb = nb + Array2.count fs in
               sum + nds pat nb + nds res nb)
             0
             cases
      | Indset (_,_,rs) ->
          ndsarr 1 rs
    in
    nds t nb

  let rec nodes (t:term): int =
    (* The number of nodes in the term t *)
    let nodesarr arr =
      Array.fold_left (fun sum t -> sum + (nodes t)) 0 arr
    in
    match t with
      Variable _ -> 1
    | VAppl (i,args,_,_) -> 1 + nodesarr args
    | Application (f,args,_) ->
        nodes f + nodesarr args
    | Lam (_,_,pres,t,_) ->
        1 + nodes t (* preconditions are not counted *)
    | QExp (_,_,t,_) ->
        1 + (nodes t)
    | Ifexp (c,e1,e2) ->
       1 + nodes c + nodes e1 + nodes e2
    | Asexp (e, _, pat) ->
       1 + nodes e + nodes pat
    | Inspect (insp,cases) ->
       Array.fold_left
         (fun sum (_,pat,res) -> sum + nodes pat + nodes res)
         (1 + nodes insp)
         cases
    | Indset (_,_,rs) ->
        1 + nodesarr rs





  let fold_with_level (f:'a->int->int->'a) (a:'a) (t:term): 'a =
    (** Fold the free variables with their level (from the top) in the order
        in which they appear in the term [t] with the function [f] and
        accumulate the results in [a].
     *)
    let rec fld (a:'a) (t:term) (level) (nb:int): 'a =
      let var i = if nb <= i then f a (i-nb) level else a
      and fldarr a arr nb =
        Array.fold_left (fun a t -> fld a t (level+1) nb) a arr
      in
      match t with
        Variable i -> var i
      | VAppl (i,args,_,_) ->
          let a = var i in
          fldarr a args nb
      | Application (f,args,_) ->
          let a = fld a f (level+1) nb in
          fldarr a args nb
      | Lam (tps,_,pres,t,_) ->
         let n = Array2.count tps in
         let level = 1 + level
         and nb    = n + nb in
         let a = List.fold_left (fun a t -> fld a t level nb) a pres in
         fld a t level nb
      | QExp (tps,_,t,_) ->
          fld a t (level+1) (nb + Array2.count tps)
      | Ifexp (cond, e1, e2) ->
         let level = level + 1 in
         fld
           (fld
              (fld a cond level nb)
              e1 level nb
           )
           e2 level nb
      | Asexp (t,tps,pat) ->
         fld a t (level+1) nb
      | Inspect(insp, cases) ->
         let level = level + 1 in
         Array.fold_left
           (fun a (fs,pat,res) ->
             let nb = nb + Array2.count fs in
             fld a res level nb
           )
           (fld a insp level nb)
           cases
      | Indset (n,nms,rs) ->
          fldarr a rs (n+nb)
    in
    fld a t 0 0



  let fold (f:'a->int->'a) (a:'a) (t:term): 'a =
    (** Fold the free variables in the order in which they appear in the
        term [t] with the function [f] and accumulate the results in [a].
     *)
    let f0 a i level = f a i in
    fold_with_level f0 a t



  let fold_arguments (f:'a->int->'a) (a:'a) (t:term) (nargs:int): 'a =
    (** Fold the arguments in the order in which they appear in the term
        [t] with [nargs] arguments with the function [f] and accumulate
        the results in [a].
     *)
    let fargs a i =
      if i < nargs then f a i else a
    in
    fold fargs a t


  let least_free (t:term): int =
    (** The least free variable of the term [t] or [-1] if the term does not
        have free variables.
     *)
    fold
      (fun least i ->
        if least = (-1) || i < least then i
        else least)
      (-1) t


  let greatestp1_arg (t:term) (nargs:int): int =
    (** The greatest (plus 1) argument variable of the term [t] with
        [nargs] arguments or [0] if there is no argument variable
     *)
    fold_arguments
      (fun gtst i -> if gtst <= i then i+1 else gtst)
      0
      t
      nargs


  let split_variables (t:term) (n:int): IntSet.t * IntSet.t =
    (** The set of bound variables strictly below [n] and above [n]
       in the term [t].
     *)
    fold
      (fun (lset,uset) i ->
        if i < n then
          IntSet.add i lset, uset
        else
          lset, IntSet.add i uset)
      (IntSet.empty,IntSet.empty)
      t



  let variables_filtered (t:term) (f:int->bool): IntSet.t =
    (* The set of variables which satisfy the predicate [f] *)
    fold
      (fun set i ->
        if f i then
          IntSet.add i set
        else
          set)
      IntSet.empty
      t


  let free_variables (t:term) (nb:int): IntSet.t =
    (* The set of free variables above 'n' in the term 't' *)
    variables_filtered t (fun i -> nb <= i)



  let bound_variables (t:term) (nb:int): IntSet.t =
    (* The set of bound variables strictly below 'n' in the term 't' *)
    variables_filtered t (fun i -> i < nb)


  let range_variables (t:term) (start:int) (beyond:int): IntSet.t =
    (* The set of variables [i] with [start <= i < beyond] *)
    variables_filtered t (fun i -> start <= i && i < beyond)



  let equivalent (t1:term) (t2:term): bool =
    (* Are the terms [t1] and [t2] equivalent ignoring names and predicate flags? *)
    let rec eq t1 t2 nb1 nb2 =
      let eqarr arr1 arr2 nb1 nb2 =
        let n1 = Array.length arr1
        and n2 = Array.length arr2 in
        n1 = n2 &&
        interval_for_all (fun i -> eq arr1.(i) arr2.(i) nb1 nb2) 0 n1
      in
      match t1, t2 with
        Variable i, Variable j ->
          i = j
      | VAppl(i1,args1,ags1,_), VAppl(i2,args2,ags2,_)
        when i1 = i2 ->
          eqarr args1 args2 nb1 nb2 &&
          eqarr ags1 ags2 nb2 0
      | Application (f1,args1,_), Application (f2,args2,_) ->
          eq f1 f2 nb1 nb2 &&
          eqarr args1 args2 nb1 nb2
      | Lam(tps1,fgs1,pres1,t1,rt1), Lam(tps2,fgs2,pres2,t2,rt2) ->
         let n1 = Array2.count tps1
         and n2 = Array2.count tps2
         and nfgs1 = Array2.count fgs1
         and nfgs2 = Array2.count fgs2 in
         let nb1 = n1 + nb1
         and nb2 = nfgs1 + nb2 in
         n1 = n2
         && nfgs1 = nfgs2
         && (rt1 = None) = (rt2 = None)
         && (try List.for_all2 (fun t1 t2 -> eq t1 t2 nb1 nb2) pres1 pres2
             with Invalid_argument _ -> false)
         &&  eq t1 t2 nb1 nb2 (*&&
          eq tp1 tp2 nb2 0*)
      | QExp(tps1,fgs1,t1,is_all1), QExp(tps2,fgs2,t2,is_all2)
           when Array2.count tps1 = Array2.count tps2
                && is_all1 = is_all2 ->
         let nfgs1 = Array2.count fgs1
         and nfgs2 = Array2.count fgs2
         and n1 = Array2.count tps1 in
         if nfgs1 = nfgs2 then
           let nb1 = n1 + nb1
           and nb2 = nfgs1 + nb2 in
           (*eqarr fgs1 fgs2 nb2 0 &&
             eqarr tps1 tps2 nb2 0 &&*)
           eq t1 t2 nb1 nb2
         else
           false
      | Ifexp (c1, a1, b1), Ifexp (c2, a2, b2) ->
         let eq0 t1 t2 = eq t1 t2 nb1 nb2 in
         eq0 c1 c2 && eq0 a1 a1 && eq0 b1 b2
      | Asexp (t1, tps1, pat1), Asexp (t2, tps2, pat2) ->
         let len = Array.length tps1 in
         len = Array.length tps2
         && eq t1 t2 nb1 nb2
         && eqarr tps1 tps1 nb2 0
         && eq pat1 pat2 (nb1+len) nb2
      | Inspect (insp1,cases1), Inspect (insp2,cases2) ->
         let len = Array.length cases1 in
         len = Array.length cases2
         && eq insp1 insp2 nb1 nb2
         && interval_for_all
              (fun i ->
                let fs1,pat1,res1 = cases1.(i)
                and fs2,pat2,res2 = cases2.(i) in
                let n = Array2.count fs1 in
                let nb1 = nb1 + n in
                n = Array2.count fs2
                && eq pat1 pat1 nb1 nb2
                && eq res2 res2 nb1 nb2
              )
              0 len
      | Indset (nme1,tp1,rs1), Indset (nme2,tp2,rs2) ->
          eq tp1 tp2 nb2 0 &&
          eqarr rs1 rs2 (1+nb1) nb2
      | _, _ ->
          false
    in
    eq t1 t2 0 0

  let equivalent_list (lst1:term list) (lst2:term list): bool =
    List.length lst1 = List.length lst2 &&
    List.for_all2 (fun t1 t2 -> equivalent t1 t2) lst1 lst2


  let equivalent_array (arr1:term array) (arr2:term array): bool =
    let len = Array.length arr1 in
    len = Array.length arr2
      &&
    interval_for_all
      (fun i -> equivalent arr1.(i) arr2.(i))
      0 len



  let rec shift_from
      (delta1:int) (start1:int)
      (delta2:int) (start2:int)
      (t:term)
      : term =
    (* Shift all free variables by [delta1] starting from [start1] and all
       free type variables by [delta2] starting from [start2]. Raise
       [Term_capture] if a free variable gets bound.
     *)
    if delta1 = 0 && delta2 = 0 then
      t
    else
      let shift_i delta start i =
        if i < start then
          i
        else if i + delta < start then
          raise Term_capture
        else
          i + delta
      and shift_args d1 s1 d2 s2 args =
        if d1=0 && d2 = 0 then args
        else Array.map (shift_from d1 s1 d2 s2) args
      and shift_list d1 s1 d2 s2 lst =
        if d1=0 && d2 = 0 then lst
        else List.map (fun t -> shift_from d1 s1 d2 s2 t) lst
      and shift0 = shift_from delta1 start1 delta2 start2
      in
      match t with
        Variable i ->
          Variable (shift_i delta1 start1 i)
      | VAppl(j,args,ags,oo) ->
          VAppl(shift_i delta1 start1 j,
                shift_args delta1 start1 delta2 start2 args,
                shift_args delta2 start2 0 0 ags,
                oo)
      | Application(f,args,inop) ->
          Application(shift_from delta1 start1 delta2 start2 f,
                      shift_args delta1 start1 delta2 start2 args,
                      inop)
      | Lam(tps,fgs,pres,t,rt) ->
         let start1 = Array2.count tps + start1
         and start2 = Array2.count fgs + start2 in
         let shift_tp = shift_from delta2 start2 0 0 in
         Lam( Array2.map2 shift_tp tps,
              Array2.map2 shift_tp fgs,
              shift_list delta1 start1 delta2 start2 pres,
              shift_from delta1 start1 delta2 start2 t,
              Option.map shift_tp rt)
      | QExp (tps,fgs,t0,is_all) ->
         let start1 = Array2.count tps + start1
         and start2 = Array2.count fgs + start2 in
         let shift_tp = shift_from delta2 start2 0 0 in
         QExp (Array2.map2 shift_tp tps, Array2.map2 shift_tp fgs,
               shift_from delta1 start1 delta2 start2 t0,
               is_all)
      | Ifexp (cond, a, b) ->
         Ifexp (shift0 cond, shift0 a, shift0 b)
      | Asexp (t, tps, pat) ->
         let n = Array.length tps in
         Asexp (shift0 t,
                shift_args delta2 start2 0 0 tps,
                shift_from delta1 (start1+n) delta2 start2 pat)
      | Inspect (insp,cases) ->
         let shift_tp = shift_from delta2 start2 0 0 in
         Inspect(
             shift0 insp,
             Array.map
               (fun (fs,pat,res) ->
                 let n = Array2.count fs in
                 let shift1 = shift_from delta1 (start1+n) delta2 start2 in
                 (Array2.map2 shift_tp fs),
                 shift1 pat,
                 shift1 res
               )
               cases)
      | Indset (nme,tp,rs) ->
          let start1 = 1 + start1 in
          Indset(nme,
                 shift_from delta2 start2 0 0 tp,
                 shift_args delta1 start1 delta2 start2 rs)



  let shift (d1:int) (d2:int) (t:term): term =
    shift_from d1 0 d2 0 t

  let shift_type (delta:int) (t:type_term): type_term =
    shift_from delta 0 0 0 t

  let up_type (n:int) (tp:type_term): type_term =
    shift_type n tp

  let down_type (n:int) (tp:type_term): type_term =
    shift_type (-n) tp


  let up_from (n:int) (start:int) (t:term): term =
    shift_from n start 0 0 t

  let up (n:int) (t:term): term =
    shift_from n 0 0 0 t

  let array_up (n:int) (arr:term array): term array =
    if n = 0 then
      arr
    else
      Array.map (fun t -> up n t) arr

  let down_from (n:int) (start:int) (t:term): term =
    shift_from (-n) start 0 0 t

  let down (n:int) (t:term): term =
    shift_from (-n) 0 0 0 t

  let rec partial_subst_from
      (t:term)
      (n1:int) (nb1:int) (d1:int) (args1:term array)
      (n2:int) (nb2:int) (d2:int) (args2:term array)
      : term =
    (*  Perform a partial substitution.

        The term [t] has [n1] argument variables and [n2] type variables and
        below [nb1/nb2] bound variables.  The first [Array.length args] of the
        arguments and the first [Array.length ags] of the type variables will
        be substituted by the corresponding terms/types in [args/ags] and the
        others will be shifted down appropriately so that the new term has
        [Array.length args - nargs] argument variables and [Array.length ags -
        ntvs] type variables.

        The arguments come from an environment with [d1/d2] variables/type
        variables more than the term [t]. Therefore the variables/type
        variables in [t] above [n1/n2] have to be shifted up by
        [n_delta/ntvs_delta] to transform them into the environment of the
        arguments.  *)
    let len1,len2    = Array.length args1, Array.length args2  in
    assert (len1 <= n1);
    assert (len2 <= n2);
    let free i = assert (nb1+n1 <= i); i+d1-len1 in
    let sub_args args n1 nb1 d1 args1 n2 nb2 d2 args2 =
      Array.map
        (fun t -> partial_subst_from t n1 nb1 d1 args1 n2 nb2 d2 args2)
        args
    and sub_list lst n1 nb1 d1 args1 n2 nb2 d2 args2 =
      List.map
        (fun t -> partial_subst_from t n1 nb1 d1 args1 n2 nb2 d2 args2)
        lst
    and sub0 t = partial_subst_from t n1 nb1 d1 args1 n2 nb2 d2 args2
    in
    if len1=0 && d1=0 && len2=0 && d2=0 then
      t
    else
      match t with
        Variable i when i < nb1 ->
          t
      | Variable i when i < nb1+len1 ->
          if args1.(i-nb1) = empty_term then
            raise Empty_term;
          shift (nb1+n1-len1) 0 args1.(i-nb1)
      | Variable i when i < nb1+n1 ->
          Variable (i-len1)
      | Variable i ->
          Variable (free i)
      | VAppl(j,args,ags,oo) ->
          VAppl(free j,
                sub_args args n1 nb1 d1 args1 n2 nb2 d2 args2,
                sub_args ags  n2 nb2 d2 args2 0  0   0  [||],
                oo)
      | Application (f,args,inop) ->
          Application (partial_subst_from f n1 nb1 d1 args1 n2 nb2 d2 args2,
                       sub_args args n1 nb1 d1 args1 n2 nb2 d2 args2,
                       inop)
      | Lam (tps,fgs,ps,t0,rt) ->
         let nb1 = Array2.count tps + nb1
         and nb2 = Array2.count fgs + nb2 in
         let sub_tp tp = partial_subst_from tp n2 nb2 d2 args2 0 0 0 [||] in
         Lam (Array2.map2 sub_tp tps,
              Array2.map2 sub_tp fgs,
              sub_list ps n1 nb1 d1 args1 n2 nb2 d2 args2,
              partial_subst_from t0 n1 nb1 d1 args1 n2 nb2 d2 args2,
              Option.map sub_tp rt)
      | QExp (tps,fgs,t0,is_all) ->
         let nb1 = Array2.count tps + nb1
         and nb2 = Array2.count fgs + nb2 in
         let sub_tp tp = partial_subst_from tp n2 nb2 d2 args2 0 0 0 [||] in
         QExp( Array2.map2 sub_tp tps, Array2.map2 sub_tp fgs,
               partial_subst_from t0 n1 nb1 d1 args1 n2 nb2 d2 args2,
               is_all)
      | Ifexp (cond, a, b) ->
         Ifexp (sub0 cond, sub0 a, sub0 b)
      | Asexp (t, tps, pat) ->
         let n = Array.length tps in
         let nb1 = n + nb1 in
         Asexp (sub0 t,
                sub_args tps n2 nb2 d2 args2 0 0 0 [||],
                partial_subst_from pat n1 nb1 d1 args1 n2 nb2 d2 args2)
      | Inspect (insp, cases) ->
         let sub_tp tp = partial_subst_from tp n2 nb2 d2 args2 0 0 0 [||] in
         Inspect (sub0 insp,
                  Array.map
                    (fun (fs,pat,res) ->
                      let n = Array2.count fs in
                      let nb1 = n + nb1 in
                      let sub1 t =
                        partial_subst_from t n1 nb1 d1 args1 n2 nb2 d2 args2 in
                      Array2.map2 sub_tp fs,
                      sub1 pat,
                      sub1 res
                    )
                    cases)
      | Indset (nme,tp,rs) ->
          let nb1 = 1 + nb1 in
          Indset (nme,
                  partial_subst_from tp n2 nb2 d2 args2 0 0 0 [||],
                  sub_args           rs n1 nb1 d1 args1 n2 nb2 d2 args2)



  let partial_subst
      (t:          term)
      (n1:int) (d1:int) (args1:term array)
      (n2:int) (d2:int) (args2:term array)
      : term =
    (*  Perform a partial substitution.

        The term [t] has [n1] argument variables and [n2] type variables.  The
        first [Array.length args] of the arguments and the first [Array.length
        ags] of the type variables will be substituted by the corresponding
        terms/types in [args/ags] and the others will be shifted down
        appropriately so that the new term has [Array.length args - n1]
        argument variables and [Array.length ags - n2] type variables.

        The arguments come from an environment with [d1/d2] variables/type
        variables more than the term [t]. Therefore the variables/type
        variables in [t] above [n1/n2] have to be shifted up by [d1/d2] to
        transform them into the environment of the arguments.  *)
    partial_subst_from t n1 0 d1 args1 n2 0 d2 args2



  let subst0_from
      (t:term)
      (nb1:int) (d1:int) (args1:term array)
      (nb2:int) (d2:int) (args2:term array)
      : term =
    let n1,n2 = Array.length args1, Array.length args2 in
    partial_subst_from t n1 nb1 d1 args1 n2 nb2 d2 args2


  let subst0
      (t:term)
      (d1:int) (args1:term array)
      (d2:int) (args2:term array): term =
    (*  Perform a substitution.

        The term [t] has [Array.length args1] argument variables and
        [Array.length args2] type variables.  The arguments and type variables
        will be substituted by the terms/types in the arrays, the others will
        be shifted down appropriately so that the new term has neither
        argument nor type variables.

        The arguments come from an environment with [d1/d2] variables/type
        variables more than the term [t] (above the variables). Therefore the
        variables/type variables in [t] above have to be shifted up by
        [d1/d2] to transform them into the environment of the arguments.  *)
    subst0_from t 0 d1 args1 0 d2 args2


  let apply0 (t:term) (args1:term array) (args2: term array): term =
    let n1,n2 = Array.length args1, Array.length args2 in
    partial_subst_from t n1 0 0 args1 n2 0 0 args2


  let apply (t:term) (args:term array): term =
    let n1 = Array.length args in
    partial_subst_from t n1 0 0 args 0 0 0 [||]


  let subst_from (t:term) (nb:int) (d:int) (args:arguments): term =
    subst0_from t nb d args 0 0 [||]

  let subst (t:term) (d:int) (args:arguments): term =
    (* Substitute the arguments of the term [t] by the actual arguments [args] which
       have [d] more variables than the term [t] above its arguments. I.e. all
       variables in [t] above [nargs] have to be shifted up. *)
    subst_from t 0 d args


  let subst_array (arr:term array) (d:int) (args:arguments): arguments =
    (* Substitute the arguments of the array [arr] by the actual arguments
       [args] which have [d] more variables than the term [t] above its
       arguments. I.e. all variables in [t] above [nargs] have to be shifted
       up. *)
    Array.map (fun t -> subst t d args) arr


  let swap_variable_blocks (n1:int) (m1:int) (n2:int) (m2:int) (t:term): term =
    (* The term [t] has [n1+m1] variables and [n2+m2] type variables. The
       variables and type variables in the two blocks have to be swapped *)
    let new_var i n m = if i<n then m+i else i-n in
    let mkargs n m = Array.init (n+m) (fun i -> Variable(new_var i n m)) in
    let args1 = mkargs n1 m1
    and args2 = mkargs n2 m2 in
    subst0 t (n1+m1) args1 (n2+m2) args2



  let map_free (f1:int->int) (f2:int->int) (t:term): term =
    (* Map all variables [i] of the term [t] to [f1 i] and all type variables
       [j] to [f2 j]. Raise [Term_capture] of a free variable gets bound.
     *)
    let fdummy (_:int): int =
      assert false
    in
    let rec mapr (nb1:int) (nb2:int) (f1:int->int) (f2:int->int) (t:term): term =
      let mapargs nb1 nb2 f1 f2 args = Array.map (mapr nb1 nb2 f1 f2) args
      and maplst  nb1 nb2 f1 f2 lst  = List.map (mapr nb1 nb2 f1 f2) lst
      and map0 = mapr nb1 nb2 f1 f2
      in
      let g1 i =
        if f1 (i - nb1) < 0 then
          raise Term_capture
        else
          nb1 + f1 (i - nb1)
      in
      match t with
      | Variable i when i < nb1 ->
         t
      | Variable i ->
         Variable (g1 i)
      | VAppl(j,args,ags,oo) ->
         VAppl(g1 j,
               mapargs nb1 nb2 f1 f2 args,
               mapargs nb2 0 f2 fdummy ags,
               oo)
      | Application(f,args,inop) ->
         Application(mapr nb1 nb2 f1 f2 f,
                     mapargs nb1 nb2 f1 f2 args,
                     inop)
      | Lam (tps,fgs,pres,t0,rt) ->
         let nb1 = nb1 + Array2.count tps
         and nb2 = nb2 + Array2.count fgs in
         let map_tp = mapr nb2 0 f2 fdummy in
         Lam (Array2.map2 map_tp tps,
              Array2.map2 map_tp fgs,
              maplst nb1 nb2 f1 f2 pres,
              mapr nb1 nb2 f1 f2 t0,
              Option.map map_tp rt)
      | QExp (tps, fgs, t0, is_all) ->
         let nargs = Array2.count tps
         and ntvs = Array2.count fgs
         in
         let map_tp = mapr (ntvs+nb2) 0 f2 fdummy in
         QExp (Array2.map2 map_tp tps,
               Array2.map2 map_tp fgs,
               mapr (nargs+nb1) (ntvs+nb2) f1 f2 t0,
               is_all)
      | Ifexp (cond, a, b) ->
         Ifexp (map0 cond, map0 a, map0 b)
      | Asexp (t, tps, pat) ->
         let n = Array.length tps in
         Asexp(map0 t,
               mapargs nb2 0 f2 fdummy tps,
               mapr (nb1+n) nb2 f1 f2 pat)
      | Inspect (insp,cases) ->
         let map_tp = mapr nb2 0 f2 fdummy in
         Inspect (map0 insp,
                  Array.map
                    (fun (fs,pat,res) ->
                      let n = Array2.count fs in
                      let map1 t = mapr (nb1+n) nb2 f1 f2 t in
                      Array2.map2 map_tp fs,
                      map1 pat,
                      map1 res
                    )
                    cases)
      | Indset (nme,tp,rs) ->
         Indset (nme,
                 mapr nb2 0 f2 fdummy tp,
                 mapargs (1+nb1) nb2 f1 f2 rs)
    in
    mapr 0 0 f1 f2 t


  let  lambda_inner (t:term) (i:int): term =
    (* Extract a lambda inner term where variable [i] becomes variable [0], all
       other variables are shifted one up.
     *)
    let f j =
      if j=i then
        0
      else
        j+1
    and f2 j = j
    in
    map_free f f2 t


  let  lambda_inner_map (t:term) (m:int IntMap.t): term =
    (* Extract a lambda inner term where [m] maps i,j,k,... to the range
       0,1,...,n-1 where [n] is the cardinality of [m]. The variables from
       the map become the variables 0,1,...,n-1 and all other variables are
       shiftet up by [n]. *)
    let n = IntMap.cardinal m in
    let f j =
      try
        let i = IntMap.find j m in
        assert (i < n); i
      with Not_found ->
        j + n
    and f2 j = j
    in
    map_free f f2 t



  let used_variables_filtered_0
      (t:term) (f:int->bool) (dup:bool) (lst:int list)
      : int list =
    (* The list of variables of the term [t] which satisfy [f] in reversed
       order in which they appear *)
    fold
      (fun lst ivar ->
        if f ivar && (dup || not (List.mem ivar lst)) then ivar::lst
        else lst)
      lst
      t



  let used_variables_filtered (t:term) (f:int->bool) (dup:bool): int list =
    (* The list of variables of the term [t] which satisfy [f] in reversed
       order in which they appear *)
    used_variables_filtered_0 t f dup []



  let used_variables_array_filtered (arr:term array) (f:int->bool): int list =
    Array.fold_left
      (fun lst t -> used_variables_filtered_0 t f false lst)
      []
      arr


  let used_variables_0 (t:term) (nvars:int) (lst:int list): int list =
    (* The list of variables of the term [t] below [nvars] in reversed order in
       which they appear, accumulated to the list [lst] *)
    used_variables_filtered_0 t (fun i -> i < nvars) false lst


  let used_variables (t:term) (nvars:int): int list =
    (* The list of variables of the term [t] below [nvars] in reversed order in
       which they appear *)
    used_variables_0 t nvars []



  let used_variables_from (t:term) (nvars:int) (dup:bool): int list =
    (* The list of variables of the term [t] from [nvars] on in reversed order in
       which they appear *)
    used_variables_filtered t (fun i -> nvars <= i) dup



  let used_variables_transform (t:term) (nvars:int): int array * int array =
    (* Analyze the used variables of the term [t] with variables in the interval
       0,1,...,nvars-1 and return two arrays.

       arr1: 0,1,...nused-1     index of the used variable i
       arr2: 0,1,...nvars-1     position of the variable i in the term [t]
     *)
    let arr1  = Array.of_list (List.rev (used_variables t nvars)) in
    let nused = Array.length arr1 in
    let arr2  = Array.make nvars (-1) in
    for i = 0 to nused - 1 do
      arr2.(arr1.(i)) <- i
    done;
    arr1, arr2




  let used_variables_arr_transform (arr:term array) (nvars:int)
      : int array * int array =
    (* Analyze the used variables of the array of terms [arr] with variables in
       the interval 0,1,...,nvars-1 and return two arrays.

       arr1: 0,1,...nused-1     index of the used variable i
       arr2: 0,1,...nvars-1     position of the variable i in the array [arr1]
     *)
    let lst =
      Array.fold_left
        (fun lst t -> used_variables_0 t nvars lst)
        []
        arr in
    let arr1  = Array.of_list (List.rev lst) in
    let nused = Array.length arr1 in
    let arr2  = Array.make nvars (-1) in
    for i = 0 to nused - 1 do
      arr2.(arr1.(i)) <- i
    done;
    arr1, arr2



  let unused_transform
      ((nms,tps):    formals0)
      (ntvs:int)
      ((fgnms,fgcon):formals0)
      (t:term)
      : formals0  * arguments * formals0  * agens =
    (* Find the used variables in the term [t] and the used type variables in the
       types [tps].

       It is required that the type variables in the range 0,..,ntvs-1 are not
       used anymore.

       Generate arguments which map the used variables to their new position
       and actual generics which map the used type variables to their new
       position.

       The new positions are generated in the order of appearance in the term
       [t] and the types [tps].

       Transform the signature (nms,tps) and the formal generics (fgnms,fgcon)
       with the arguments and the actual generics.
     *)
    let n1,n2 = Array.length nms, ntvs + Array.length fgnms
    in
    let usd,pos = used_variables_transform t n1 in
    let n1new = Array.length usd in
    let args  = Array.map (fun i -> Variable i) pos
    and nms   = Array.init n1new (fun i -> nms.(usd.(i)))
    and tps   = Array.init n1new (fun i -> tps.(usd.(i)))
    in
    let usd,pos = used_variables_arr_transform tps n2 in
    let n2new = Array.length usd in
    assert (interval_for_all (fun i -> ntvs<=usd.(i)) 0 n2new);
    let ags   = Array.map (fun i -> Variable i) pos (* might create [empty_term] *)
    and fgnms = Array.init n2new (fun i -> fgnms.(usd.(i)))
    and fgcon = Array.init n2new (fun i -> fgcon.(usd.(i)))
    in
    let tps   = subst_array tps   n2new ags
    and fgcon = subst_array fgcon n2new ags
    in
    (nms,tps), args, (fgnms,fgcon), ags



  let remove_unused
      ((nms,tps):formals0)
      (ntvs:int)
      ((fgnms,fgcon):formals0)
      (t:term)
      : formals0 * formals0 * term =
    let (nms,tps), args, (fgnms,fgcon), ags =
      unused_transform (nms,tps) ntvs (fgnms,fgcon) t in
    let n1new = Array.length nms
    and n2new = Array.length fgnms in
    let t = subst0 t n1new args n2new ags in
    (nms,tps), (fgnms,fgcon), t

  let remove_unused_new
        (tps:formals) (fgs:formals) (t:term)
      : formals * formals * term =
    let tps,fgs,t =
      remove_unused (Array2.pair tps) 0 (Array2.pair fgs) t in
    Array2.from_pair tps, Array2.from_pair fgs, t


  let lambda_split
        (t:term)
      : formals * formals * term list * term * type_term option =
    match t with
      Lam (tps,fgs,pres,t,rt) -> tps,fgs,pres,t,rt
    | _ -> raise Not_found


  let qlambda_split_0 (t:term): formals * formals * term * bool =
    match t with
      QExp (args,fgs,t,is_all) -> args,fgs,t,is_all
    | _ ->
        Array2.empty, Array2.empty, t, false

  let qlambda_split (t:term): formals * formals * term * bool =
    match t with
      QExp (args,fgs,t,is_all) -> args,fgs,t,is_all
    | _ -> raise Not_found

  let pattern_split (t:term): formals * term =
    let fargs,_,t,is_all = qlambda_split_0 t in
    assert (not is_all);
    fargs, t

  let case_split (t1:term) (t2:term): formals * term * term =
    let fargs1,t1 = pattern_split t1 in
    let n1 = Array2.count fargs1 in
    if n1 = 0 then
      fargs1, t1, t2 (* There are not pattern variables *)
    else begin
        let fargs2,t2 = pattern_split t2 in
        let n2 = Array2.count fargs2 in
        assert (n1 = n2);
        fargs1, t1, t2
    end


  let unary (unid:int) (t:term): term =
    let args = [| t |] in
    VAppl (unid,args,[||],false)


  let unary_split (t:term) (unid:int): term =
    match t with
      VAppl (i,args,_,_) when i = unid ->
        assert (Array.length args = 1);
        args.(0)
    | _ -> raise Not_found


  let binary (binid:int) (left:term) (right:term): term =
    let args = [| left; right |] in
    VAppl (binid, args, [||],false)


  let binary_split_0 (t:term): int * term * term =
    match t with
      VAppl(i,args,_,_) when Array.length args = 2 ->
        i, args.(0), args.(1)
    | _ ->
        raise Not_found


  let binary_split (t:term) (binid:int): term * term =
    let i,a,b = binary_split_0 t in
    if i = binid then
      a,b
    else
      raise Not_found



  let quantified (is_all:bool) (args:formals) (fgs:formals) (t:term)
      : term =
    if Array2.count args = 0 then
      t
    else
      QExp (args,fgs,t,is_all)


  let all_quantified (args:formals) (fgs:formals) (t:term): term =
    quantified true args fgs t

  let some_quantified (args:formals) (t:term): term =
    quantified false args Array2.empty t


  let pattern (args:formals) (t:term): term =
    some_quantified args t


  let quantifier_split (t:term) (is_all:bool): formals * formals * term =
    let args,fgs,t0,is_all0 = qlambda_split t in
    if is_all = is_all0 then
      args,fgs,t0
    else
      raise Not_found


  let all_quantifier_split (t:term): formals * formals * term =
    quantifier_split t true



  let all_quantifier_split_1 (t:term): formals * formals * term =
    try
      all_quantifier_split t
    with Not_found ->
        Array2.empty, Array2.empty, t


  let some_quantifier_split (t:term): formals * term =
    let tps,fgs,t = quantifier_split t false in
    assert (fgs = Array2.empty);
    tps,t


  let is_all_quantified (t:term): bool =
    try ignore (all_quantifier_split t); true
    with Not_found -> false


  let is_generic (t:term): bool =
    try
      let _,fgs,_ = all_quantifier_split t in
      Array2.count fgs > 0
    with Not_found ->
      false


  let split_implication_chain (t:term) (impid:int)
      : term list * term =
    (** Extract the implication chain of the term [t], i.e. if
        [t] has the form

            a => b => ... => e => z

        it returns

            [e,...,b,a] , z

        Note:
        a) The premises are returned in reverse order.
        b) If [t] is not an implication, then the list of premises is
           empty and [t] is returned as the consequence
     *)
    let rec chrec (t:term) (ps_rev:term list): term list * term =
      try
        let a,b = binary_split t impid in
        chrec b (a::ps_rev)
      with Not_found ->
        ps_rev, t
    in
    chrec t []



  let split_left_binop_chain (t:term) (op_id:int): term list =
    (* If the term [t] has the form

           a op b op c op ... op z

       and the operator represented by [op_id] is left associative i.e.

           ((..(a op b) op c) .. ) op z

       then
       return the list

           [a,b,c,...,z]
     *)
    let rec split t lst =
      try
        let a,b = binary_split t op_id in
        split a (b :: lst)
      with Not_found ->
        t :: lst
    in
    split t []




  let split_general_implication_chain (t:term) (imp_id:int)
      : formals * formals * term list * term =
    let tps,fgs,t0,is_all = qlambda_split_0 t in
    let n = Array2.count tps in
    if n > 0 && not is_all then
      Array2.empty, Array2.empty, [], t
    else
      let ps_rev, tgt = split_implication_chain t0 (n+imp_id) in
      tps,fgs,ps_rev,tgt


  let rec make_implication_chain
      (ps_rev:term list) (tgt:term) (imp_id:int): term =
    (*  Make an implication chain from the reversed list of the premises, the target
        and the implication id.
     *)
    match ps_rev with
      [] -> tgt
    | p::ps0 ->
        make_implication_chain
          ps0
          (binary imp_id p tgt)
          imp_id


(* Name clashes:

   Variables in inner and outer contexts might have the same names. Internally
   there is no problem because De Bruijn indices are used. However the human
   reader might get confused, because he cannot determine to which context belongs
   the variable.

   Previous strategy: The inner variable names have been prefixed with '$' if the
   a variable with the same name already existed in the outer context. If the
   same prefixed name already existed another '$' had to be added until no more
   conflicts arised.

   New strategy: We use the prefix '.' as an escape. If a variable 'v' exists in
   the inner and in an outer context, the outer variable is renamed to '.v'. All
   variables in the outer context which are name '...v' where there are zero
   or more escapes get one escape more.

 *)
  let adapt_outer_names (nms_inner:int array) (nms_outer:int array): int array =
    let nms_outer = Array.copy nms_outer in
    let patch_outer inner i outer =
      let str_outer = ST.string outer in
      let nescapes,pure_str =
        try
          let nescapes = 1 + String.rindex str_outer '.' in
          nescapes,
          String.sub str_outer nescapes (String.length str_outer - nescapes)
        with Not_found ->
          0, str_outer
      in
      if ST.symbol pure_str = inner then
        nms_outer.(i) <-
          ST.symbol ("." ^  str_outer)
      else
        ()
    in
    Array.iter
      (fun nme_inner ->
        Array.iteri
          (fun i nme_outer -> patch_outer nme_inner i nme_outer)
          nms_outer
      )
      nms_inner;
    nms_outer

  let adapt_names (nms:int array) (names:int array): int array =
    (* old strategy *)
    let nms  = Array.copy nms in
    let nnms = Array.length nms in
    let patch i =
      assert (i < nnms);
      let str = "$" ^ (ST.string nms.(i)) in
      nms.(i) <- ST.symbol str
    and has i =
      assert (i < nnms);
      try
        let _ = Search.array_find_min (fun nme -> nme = nms.(i)) names in
        true
      with Not_found ->
        false
    in
    let rec patch_until_ok i =
      if has i then begin
        patch i;
        patch_until_ok i
      end
    in
    for i = 0 to nnms - 1 do
      patch_until_ok i
    done;
    nms


  let prepend_names (nms1:int array) (nms2:int array): int array =
    let nms2 = adapt_outer_names nms1 nms2 in
    Array.append nms1 nms2



  let rec prenex (t:term) (nb:int) (nb2:int) (imp_id:int): term =
    (* Calculate the prenex normal form of the term [t] with respect to
       universal quantifiers. All universal quantifiers are bubbled up in
       implication chains and merged with the upper universal
       quantifier. Unused variables in universally quantified expressions are
       eliminated. Variables are ordered according to their appearance.

       Note: The implication index is valid in the global environment!!
     *)
   let tps,fgs,t0 = prenex_0 t nb 0 nb2 imp_id true 0 in
   all_quantified tps fgs t0


  and prenex_sort (t:term) (nb:int) (nb2:int) (imp_id:int): term =
   let tps,fgs,t0 = prenex_0 t nb 0 nb2 imp_id false 0 in
   all_quantified tps fgs t0

  and prenex_bubble_one (t:term) (nb:int) (nb2:int) (imp_id:int): term =
   let tps,fgs,t0 = prenex_0 t nb 0 nb2 imp_id false 1 in
   all_quantified tps fgs t0


  and prenex_0
      (t:term)
      (nb:int)
      (nargs:int)
      (nb2:int)
      (imp_id:int)
      (recursive:bool)
      (nbubble:int)
      : formals * formals * term =
    (* Calculate the number of variables (with their names and types) which
       bubble up from the term [t] because the target of an implication chain
       is universally quantified.

       nb:    total number of variables in the environment
       nargs: number of the variables to which bubbled up variables have to
              be appended

       not recursive and nbubble = 0: just sort the variables if universally
                                      quantified

       not recursive and nbubble > 0: sort the variables and let bubble up
                                      up to nbubble universal quantifiers

       recursive:                     full recursive prenex calculation
     *)
    let pren t nb nb2 imp_id =
      if recursive then
        prenex t nb nb2 imp_id
      else
        t
    in
    let norm_args (args:term array) (nb:int) (nb2:int): term array =
      Array.map (fun t -> pren t nb nb2 imp_id) args
    and norm_lst  (lst: term list) (nb:int) (nb2:int): term list =
      List.map (fun t -> pren t nb nb2 imp_id) lst
    and pren0 t = pren t nb nb2 imp_id
    in
    match t with
    | Variable i ->
       Array2.empty, Array2.empty, t
    | VAppl(i,args,ags,oo) when i = nb + imp_id ->
        assert (Array.length args = 2);
        assert (Array.length ags  = 0);
        let a = pren args.(0) nb nb2 imp_id
        and tps,fgs,b1 =
          prenex_0 args.(1) nb nargs nb2 imp_id recursive nbubble in
        let n = Array2.count tps in
        let a1 = shift n (Array2.count fgs) a in
        assert (not recursive || not (is_all_quantified b1));
        let t = VAppl(i+n,[|a1;b1|],ags,oo) in
        tps, fgs, t
    | VAppl(i,args,ags,oo) ->
        Array2.empty, Array2.empty,
        VAppl(i, norm_args args nb nb2, ags, oo)
    | Application(f,args,inop) ->
        let f = pren f nb nb2 imp_id
        and args = norm_args args nb nb2 in
        Array2.empty, Array2.empty, Application(f,args,inop)
    | Lam(tps,fgs,ps,t0,rt) ->
       let nb  = nb + Array2.count tps
       and nb2 = nb2 + Array2.count fgs in
       let ps = norm_lst ps nb nb2
       and t0 = pren t0 nb nb2 imp_id in
       Array2.empty, Array2.empty, Lam(tps,fgs,ps,t0,rt)
    | QExp(tps,fgs,t0,true) ->
       let n0 = Array2.count tps
       and nms = Array2.first tps
       and tps = Array2.second tps
       and fgnms = Array2.first fgs
       and fgcon = Array2.second fgs in
       let nb  = nb  + n0
       and nb2 = nb2 + Array.length fgnms in
       let tps1,fgs1,t1 =
         if recursive then
           prenex_0 t0 nb (n0+nargs) nb2 imp_id recursive nbubble
         else if nbubble > 0 then
           prenex_0 t0 nb (n0+nargs) nb2 imp_id recursive (nbubble-1)
         else
           Array2.empty, Array2.empty, t0
       in
       let nms1 = Array2.first tps1
       and tps1 = Array2.second tps1
       and fgnms1 = Array2.first fgs1
       and fgcon1 = Array2.second fgs1 in
       assert (not recursive || not (is_all_quantified t1));
       let nms   = prepend_names nms1 nms
       and tps   = Array.append tps1 tps
       and fgnms = prepend_names fgnms fgnms1
       and fgcon = Array.append fgcon fgcon1
       in
       let (nms,tps),(fgnms,fgcon),t1 =
         remove_unused (nms,tps) 0 (fgnms,fgcon) t1 in
       assert (not recursive || not (is_all_quantified t1));
       (Array2.make nms tps), (Array2.make fgnms fgcon), t1
    | QExp(tps,fgs,t0,false) ->
       let n0 = Array2.count tps
       and nfgs = Array2.count fgs in
       let nb = nb + n0 and nb2 = nfgs in
       let t0 = pren t0 nb nb2 imp_id in
       let tps,fgs,t0 = remove_unused_new tps fgs t0 in
       Array2.empty, Array2.empty,
       QExp(tps,fgs,
            pren t0 nb nb2 imp_id, false)
    | Ifexp (c, a, b) ->
       Array2.empty, Array2.empty, Ifexp( pren0 c, pren0 a, pren0 b)
    | Asexp _ ->
       Array2.empty, Array2.empty, t
    | Inspect _ ->
       Array2.empty, Array2.empty, t
    | Indset (nme,tp,rs) ->
       let rs = norm_args rs (1+nb) nb2 in
       Array2.empty, Array2.empty , Indset (nme,tp,rs)







  let closure_rule (i:int) (p:term) (p_rep:term): term =
    assert (0 <= i);
    match p with
      Indset(n,nms,rs) ->
        if Array.length rs <= i then invalid_arg "Rule index out of bound";
        apply rs.(i) [|p_rep|]
    | _ ->
        invalid_arg "Not an inductive set"




  let induction_rule (imp_id:int) (i:int) (p:term) (pr:term) (q:term)
      : formals * term list * term =
    (* Calculate the induction rule [i] for the inductively defined set [p]
       represented by [pr] with the goal predicate [q].

       The closure rule [i] is

           all(x,y,...)   c1 ==> c2  ==> ...  ==> p(e)

           where each ci has the form
                all(...) di ==> p(ei)
           or degenerate without quantifier and premises

       The induction rule [i] is

           all(x,y,...)   c1(p) ==> c1(q) ==>
                          c2(p) ==> c2(q) ==>
                          ...
                          p(e)  ==> q(e)

       The function returs the formal arguments, the list of premises and the
       target.
     *)
    let pair (n:int) (t:term): term * term =
      subst_from t n 0 [|pr|],
      subst_from t n 0 [|q |]
    in
    match p with
      Indset (nme,tp,rs) ->
        let nrules = Array.length rs in
        assert (i < nrules);
        let fargs,fgs,ps_rev,tgt =
          split_general_implication_chain rs.(i) (imp_id+1) in
        let n = Array2.count fargs in
        assert (fgs = Array2.empty);
        let last,tgt = pair n tgt in
        let ps = List.fold_left
            (fun ps t ->
              try
                let t1,t2 = pair n t in
                t1 :: t2 :: ps
              with Not_found ->
                let t = down_from 1 n t in
                t :: ps)
            [last]
            ps_rev
        in
        fargs,ps,tgt
    | _ ->
        invalid_arg "Not an inductive set"



  let induction_law
      (imp_id:int) (p:term) (pr:term) (el_tp:type_term) (set_tp:type_term)
      : term =
    (* Calculate the induction law for the inductively defined set [p] represented
       by [pr]

       all(q,a) ind1 ==> ... ==> indn ==> p(a) ==> q(a)

     *)
    let imp_id = imp_id + 2
    and p      = up 2 p
    and pr     = up 2 pr (* space for a and q *)
    in
    match p with
      Indset (nme,tp,rs) ->
        let nrules = Array.length rs in
        let rule i =
          let fargs,ps,tgt = induction_rule imp_id i p pr (Variable 0) in
          let n = Array2.count fargs in
          let chn = make_implication_chain (List.rev ps) tgt (imp_id+n) in
          all_quantified fargs Array2.empty chn in
        let pa = Application (pr,[|Variable 1|],true)
        and qa = Application (Variable 0, [|Variable 1|],true) in
        let tgt = binary imp_id pa qa in
        let tgt =
          interval_fold
            (fun tgt j ->
              let i = nrules - j - 1 in
              let indi = rule i in
              binary imp_id indi tgt)
            tgt
            0 nrules in
        let nms = [|ST.symbol "q";ST.symbol "a"|]
        and tps = [|set_tp; el_tp|] in
        all_quantified (Array2.make nms tps) Array2.empty tgt
    | _ ->
        invalid_arg "Not an inductive set"
end (* Term *)




module Formals  =
  struct
    type t = (int,type_term) Array2.t
    let empty: t = Array2.empty
    let make (nms:names) (tps:types): t =
      Array2.make nms tps
    let from_pair ((nms,tps):names*types): t =
      make nms tps
    let copy (f:t): t = Array2.copy f
    let names (f:t) = Array2.first f
    let types (f:t) = Array2.second f
    let name (i:int) (f:t): int = Array2.elem1 i f
    let is_equivalent (f1:t) (f2:t): bool =
      Term.equivalent_array (types f1) (types f2)
    let typ  (i:int) (f:t): type_term = Array2.elem2 i f
    let map  (f:type_term -> type_term) (fs:t): t = Array2.map2 f fs
    let count (f:t) = Array2.count f
    let sub (start:int) (n:int) (f:t): t = Array2.sub start n f
    let prepend (f1:t) (f2:t): t =
      make
        (Term.prepend_names (names f1) (names f2))
        (Array.append (types f1) (types f2))
    let formals (f:t) = Array2.first f, Array2.second f
    let equivalent (f1:t) (f2:t): bool =
      Term.equivalent_array (types f1) (types f2)
  end (* Formals *)




module Term_sub = struct

  type t = term IntMap.t

  let to_string (sub:t): string =
    let lst = IntMap.fold
        (fun i t lst -> ((string_of_int i)^"->"^(Term.to_string t))::lst
        )
        sub
        []
    in
    "{" ^ (String.concat "," (List.rev lst)) ^ "}"

  let count (sub:t): int =
    IntMap.cardinal sub

  let for_all (f:int-> term -> bool) (sub:t): bool =
    IntMap.for_all f sub

  let iter (f:int -> term -> unit) (sub:t): unit =
    IntMap.iter f sub

  let fold (f:int->term->'a->'a) (sub:t) (a:'a): 'a =
    IntMap.fold f sub a

  let map (f:term->term) (sub:t): t =
    IntMap.map f sub

  let is_identity (sub:t): bool =
    IntMap.for_all (fun i t -> Variable i = t) sub

  let inverse (sub:t): t =
    IntMap.fold
      (fun i t inv ->
        match t with
          Variable j when not (IntMap.mem j inv) ->
            IntMap.add j (Variable i) inv
        | _ -> raise Not_found
      )
      sub
      IntMap.empty

  let is_injective (sub:t): bool =
    try
      let _ = inverse sub in
      true
    with Not_found ->
      false


  let empty = IntMap.empty

  let is_empty(sub:t) = IntMap.is_empty sub

  let singleton (i:int) (t:term): t =
    IntMap.add i t IntMap.empty

  let find (i:int) (sub:t): term =
    IntMap.find i sub

  let mem (i:int) (sub:t): bool =
    IntMap.mem i sub

  let add (i:int) (t:term) (sub:t): t =
    assert (not (mem i sub));
    IntMap.add i t sub

  let identity (n:int): t =
    assert (0 <= n);
    let res = ref empty in
    for i = 0 to n-1 do
      res := add i (Variable i) !res
    done;
    !res

  let merge (sub1:t) (sub2:t): t =
    let res = ref sub2 in
    IntMap.iter
      (fun i t1 ->
        let t2_opt =
          try Some (IntMap.find i sub2)
          with Not_found -> None
        in
        match t2_opt with
          None ->
              res := IntMap.add i t1 !res
        | Some t2 ->
            if Term.equivalent t1 t2 then ()
            else ((*Printf.printf "    Cannot merge sub\n";*) raise Not_found)
      )
      sub1;
    !res

  let to_list (sub:t): (int*term) list =
    let lst = IntMap.fold (fun i t lst -> (i,t)::lst) sub [] in
    List.rev lst

  let arguments (nargs:int) (sub:t): term array =
    assert (IntMap.cardinal sub = nargs);
    let args = Array.make nargs empty_term in
    IntMap.iter
      (fun i t ->
        assert (i<nargs);
        args.(i) <- t)
      sub;
    args

  let filled_arguments (nargs:int) (ntvs:int) (sub:t): term array =
    (* Convert the substitution [sub] into an argument array with [nargs] positions
       and fill the missing positions with [nargs - sub.cardinal] arguments. Shift
       all arguments in [sub] up by the added arguments and the corresponding
       types by [ntvs]. *)
    let nargs0 = IntMap.cardinal sub in
    assert (nargs0 <= nargs);
    let arr = Array.make nargs empty_term
    in
    let fill (pos0:int) (pos1:int) (var:int): int =
      assert (pos0 <= pos1);
      interval_iter
        (fun p -> arr.(p) <- Variable (var + p + pos0))
        pos0 pos1;
      var + pos1 - pos0
    in
    let maxpos, maxvar =
      IntMap.fold
        (fun i t (maxpos,maxvar) ->
          assert (maxpos <= i);
          let maxvar = fill maxpos i maxvar
          in
          arr.(i) <- Term.shift (nargs-nargs0) ntvs t;
          i + 1, maxvar
        )
      sub
      (0,0)
    in
    let maxvar = fill maxpos nargs maxvar in
    assert (maxvar = nargs - nargs0);
    arr

  let has_only_variables (sub:t): bool =
    for_all
      (fun i t ->
        match t with
          Variable i -> true
        | _ -> false)
      sub

end (* Term_sub *)
