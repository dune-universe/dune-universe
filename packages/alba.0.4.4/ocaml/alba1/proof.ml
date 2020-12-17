(* Copyright (C) Helmut Brandl  <helmut dot brandl at gmx dot net>

   This file is distributed under the terms of the GNU General Public License
   version 2 (GPLv2) as published by the Free Software Foundation.
*)

open Fmlib
open Container
open Term
open Printf

exception Limit_exceeded of int
exception Proof_failed of string

module Eval = struct
  type t =
      Term of term
    | Exp of (int * agens * t array * t) (* idx of function, actual generics,
                                            eval args, eval of expansion *)
    | VApply of  int * t array * type_term array
    | Apply of t * t array * t (* eval f, eval args, eval result *)
    | Lam of formals * formals * term list * t * type_term option
    | QExp of int * formals0 * formals0 * t * bool
    | Beta of t * t (* redex and reduct *)
    | Simpl of t * int * term array * agens
          (* e, idx of simplifying equality assertion, specialization arguments *)
    | If of (bool * int * t array) (* cond, idx cond, args *)
    | As of bool * t * types * term (* true or false, inspe, types, pattern *)
    | AsExp of term
    | Inspect of (term * t * int * t)
          (* uneval, eval insp, case, eval res *)
  let rec print (prefix:string) (e:t): unit =
    let print_args args =
      let prefix = prefix ^ "  " in
      Array.iter (fun e -> print prefix e) args
    in
    match e with
      Term t -> printf "%s term %s\n" prefix (Term.to_string t)
    | Exp (i,ags,args,e) ->
        printf "%s expand %d\n" prefix i;
        print (prefix ^ "    ") e
    | VApply (i,args,_) ->
        printf "%s apply %d\n" prefix i;
        print_args args
    | Apply (f,args,pr) ->
        printf "%s apply \n" prefix;
        print (prefix ^ "    ") f;
        print_args args;
        print (prefix ^ "    ") e
    | Lam (_,_,pres,e,rt) ->
       let pr = (rt = None) in
       printf "%s lambda %s\n" prefix (if pr then "predicate" else "function");
       print (prefix ^ "    ") e
    | QExp (n,_,_,e,is_all) ->
        printf "%s qexp %s\n" prefix (if is_all then "all" else "some");
        print (prefix ^ "    ") e
    | Beta (e1,e2) ->
        printf "%s beta\n" prefix;
        print (prefix ^ "    ") e1;
        print (prefix ^ "    ") e2
    | Simpl (e,idx,args,ags) ->
        printf "%s simpl eq_idx %d\n" prefix idx;
        print (prefix ^ "    ") e
    | If (cond,idx,args) ->
        printf "%s \"if\" %b idx %d\n" prefix cond idx;
        print_args args
    | As (cond,insp,tps,pat) ->
        printf "%s \"as\" %b\n" prefix cond
    | AsExp t -> printf "%s as %s\n" prefix (Term.to_string t)
    | Inspect (t,inspe,icase,rese) ->
        printf "%s \"inspect\" case %d\n" prefix icase

end


type proof_term =
    Axiom      of term
  | Assumption of term
  | Funprop    of
      int * int * term array (* idx of function,
                                idx of postcondition,
                                arguments *)
  | Indset_rule of term * int
  | Indset_ind  of term
  | Indtype    of int (* cls *)
  | Detached   of int * int  (* modus ponens *)
  | Specialize of int * arguments * agens
  | Eval       of int*Eval.t  (* index of the term evaluated,evaluation *)
  | Eval_bwd   of term*Eval.t (* term which is backward evaluated, evaluation *)
  | Witness    of int * formals0 * term * term array
        (* term [i] is a witness for [some (a,b,...) t] where
           [a,b,..] in [t] are substituted by the arguments in the term array *)
  | Someelim   of int        (* index of the existenially quantified term *)
  | Inherit    of int * int * int (* assertion, base/descendant class *)
  | Subproof   of formals0 * formals0 * int * proof_term array * bool
                        (* _,_,res,_,bubble_up *)


module Proof_term: sig
  type t = proof_term

  val adapt: int -> int -> t -> t

  val remove_unused_proof_terms: int -> int -> t array -> int * t array

  val used_variables: int -> t array -> IntSet.t

  val remove_unused_variables:
      arguments -> int -> arguments -> int -> t array -> t array

  val normalize_pair: int -> int array -> int -> term -> t array
    -> int * int array * term * t array

  val print_pt_arr:  string -> int -> t array -> unit

  val print_pt: string -> int -> t -> unit

  val split_subproof: t -> formals0 * formals0 * int * t array

  val is_subproof: t -> bool

  val short_string: t -> string

end = struct

  type t = proof_term



  let rec print_pt_arr (prefix:string) (start:int) (pt_arr: t array): unit =
    let n = Array.length pt_arr in
    for k = 0 to n-1 do
      let print_prefix () = printf "%s%3d " prefix (start+k) in
      match pt_arr.(k) with
        Axiom t             ->
          print_prefix (); printf "Axiom %s\n" (Term.to_string t)
      | Assumption t        ->
          print_prefix (); printf "Assumption %s\n" (Term.to_string t)
      | Funprop (idx,i,_)  ->
          print_prefix (); printf "Funprop %d %d\n" idx i
      | Indset_rule (t,i) ->
          print_prefix (); printf "Rule %d of %s\n" i (Term.to_string t)
      | Indset_ind t ->
          print_prefix (); printf "Set induction law %s\n" (Term.to_string t)
      | Indtype cls ->
          print_prefix (); printf "Induction law cls %d\n" cls;
      | Detached (i,j)      ->
          print_prefix (); printf "Detached %d %d\n" i j
      | Specialize (i,args,ags) ->
          print_prefix (); printf "Specialize %d\n" i
      | Eval (i,e)          ->
          print_prefix (); printf "Eval %d\n" i;
          Eval.print (prefix ^ "    ") e
      | Eval_bwd (t,e)      ->
          print_prefix ();
          printf "Eval_bwd %s\n" (Term.to_string t);
          Eval.print (prefix ^ "    ") e
      | Witness (i,_,t,args)-> print_prefix (); printf "Witness %d\n" i
      | Someelim i          -> print_prefix (); printf "Someelim %d\n" i
      | Subproof ((nms,_),(fgnms,_),i,pt_arr,_) ->
          print_prefix (); printf "Subproof nb %d/%d, i %d\n"
            (Array.length nms) (Array.length fgnms) i;
          print_pt_arr (prefix^"  ") (start+k) pt_arr
      | Inherit (i,bcls,cls)  -> print_prefix (); printf "Inherit %d\n" i
    done

  let print_pt (prefix:string) (start:int) (pt:t): unit =
    print_pt_arr prefix start [|pt|]


  let adapt (start:int) (delta:int) (pt:t): t =
    (* Shift the assertion indices from [start] on up by [delta]. *)
    let index (i:int): int =
      if i < start then i else i + delta
    in
    let rec adapt_eval (e:Eval.t): Eval.t =
      let adapt_args args = Array.map (fun e -> adapt_eval e) args
      in
      match e with
        Eval.Term t | Eval.AsExp t  -> e
      | Eval.Exp (i,ags,args,e) ->
          Eval.Exp (i,ags, adapt_args args, adapt_eval e)
      | Eval.VApply (i,args,ags) ->
          Eval.VApply (i,adapt_args args,ags)
      | Eval.Apply (f,args,e) ->
          let f = adapt_eval f
          and args = adapt_args args
          and e = adapt_eval e in
          Eval.Apply (f,args,e)
      | Eval.Lam (tps,fgs,pres,e,rt) ->
          Eval.Lam (tps, fgs, pres, adapt_eval e, rt)
      | Eval.QExp (n,tps,fgs,e,is_all) ->
          Eval.QExp (n, tps, fgs, adapt_eval e, is_all)
      | Eval.Beta (e1,e2) -> Eval.Beta (adapt_eval e1, adapt_eval e2)
      | Eval.Simpl (e,eq_idx,args,ags) ->
          Eval.Simpl (adapt_eval e, index eq_idx, args, ags)
      | Eval.If (cond,idx,args) ->
          Eval.If (cond, index idx, adapt_args args)
      | Eval.As (cond,inspe,tps,pat) ->
         Eval.As (cond, adapt_eval inspe, tps, pat)
      | Eval.Inspect (t,inspe,icase,rese) ->
          Eval.Inspect (t, adapt_eval inspe, icase, adapt_eval rese)

    in
    let rec adapt (pt:t): t =
      match pt with
        Axiom _ | Assumption _ | Funprop _
      | Indset_rule _ | Indset_ind _ | Indtype _ ->
          pt
      | Detached (a,b) ->
          Detached (index a, index b)
      | Specialize (i,args,ags) ->
          Specialize (index i, args, ags)
      | Inherit (i,bcls,cls) ->
          Inherit (index i, bcls, cls)
      | Eval (i,e)    -> Eval (index i, adapt_eval e)
      | Eval_bwd (t,e)-> Eval_bwd (t, adapt_eval e)
      | Witness (i,nms,t,args) ->
          Witness (index i,nms,t,args)
      | Someelim i   -> Someelim (index i)
      | Subproof (nargs,names,res,pt_arr,bubble) ->
          Subproof (nargs,names, index res, Array.map adapt pt_arr,bubble)
    in
    if delta = 0 then pt else adapt pt


  let count_assumptions (pt_array: t array): int =
    let n = Array.length pt_array in
    let rec count (i:int): int =
      assert (i <= n);
      if i = n then n
      else
        match pt_array.(i) with
          Assumption _ -> count (i+1)
        | _ -> i
    in
    count 0





  let rec used_below (start:int) (k:int) (below:int) (pt_arr:t array) (set:IntSet.t)
      : IntSet.t =
    (* The set of used proof terms in [pt_arr] which are below the index
       [below] added to the set [set], the indices in [pt_arr] start at
       [start]. It is assumed that [pt_arr] has no unused proof term.  *)
    let add_idx i set =
      if i < below then IntSet.add i set
      else set
    in
    let rec used_eval (e:Eval.t) (set:IntSet.t): IntSet.t =
      let used_args set args =
        Array.fold_left (fun set e -> used_eval e set) set args in
      match e with
        Eval.Term t | Eval.AsExp t -> set
      | Eval.Exp (i,_,args,e)  -> used_eval e (used_args set args)
      | Eval.VApply(i,args,_)  -> used_args set args
      | Eval.Apply (f,args,e)  -> used_eval e (used_args (used_eval f set) args)
      | Eval.Lam (_,_,_,e,_) | Eval.QExp (_,_,_,e,_) ->
          used_eval e set
      | Eval.Beta (e1,e2)      -> used_eval e1 (used_eval e2 set)
      | Eval.Simpl (e,i,args,_)-> used_eval e (add_idx i set)
      | Eval.If (cond,idx,args)-> used_args (add_idx idx set) args
      | Eval.As (cond,inspe,tps,pat) ->
         used_eval inspe set
      | Eval.Inspect (t,inspe,icase,rese) ->
          used_eval rese (used_eval inspe set)
    in
    let set = add_idx k set in
    let i,set = Array.fold_left
        (fun (k,set) pt ->
          let set =
            match pt with
              Axiom _ | Assumption _ | Funprop _
            | Indset_rule _ | Indset_ind _ | Indtype _ ->
                set
            | Detached (i,j) ->
                add_idx i (add_idx j set)
            | Specialize (i,_,_) | Witness (i,_,_,_) | Someelim i ->
                add_idx i set
            | Eval (i,e) ->
                let set = add_idx i set in
                used_eval e set
            | Eval_bwd (t,e) ->
                used_eval e set
            | Subproof (_,_,k1,pt_arr1,_) ->
                used_below k k1 below pt_arr1 set
            | Inherit (i,bcls,cls) ->
                assert false
          in
          k+1, set)
        (0,set)
        pt_arr
    in
    assert (i = Array.length pt_arr);
    set



  let used (k:int) (start:int) (pt_arr:t array): IntSet.t =
    (* The set of used proof terms in [pt_arr] which are needed to proof the
       term [k].  The index [k] is absolute, numbering in [pt_arr] starts at
       [start]. The returned set contains absolute numbers. *)
    let rec usd (k:int) (pt_arr: t array) (set:IntSet.t): IntSet.t =
      let n = Array.length pt_arr in
      assert (k < start + n);
      let rec usd_eval (e:Eval.t) (set:IntSet.t): IntSet.t =
        let usd_args set args =
          Array.fold_left (fun set e -> usd_eval e set) set args in
        match e with
          Eval.Term t | Eval.AsExp t -> set
        | Eval.Exp (i,_,args,e)   -> usd_eval e (usd_args set args)
        | Eval.VApply (i,args,_)  -> usd_args set args
        | Eval.Apply (f,args,e) ->
            let set = usd_eval f set in
            let set = usd_args set args in
            usd_eval e set
        | Eval.Lam (_,_,_,e,_) | Eval.QExp(_,_,_,e,_) -> usd_eval e set
        | Eval.Beta (e1,e2)    -> usd_eval e1 (usd_eval e2 set)
        | Eval.Simpl (e,i,args,_) ->
            let set = usd i pt_arr set in
            usd_eval e set
        | Eval.If (cond,idx,args) -> usd_args (usd idx pt_arr set) args
        | Eval.As (cond,inspe,tps,pat) ->
           usd_eval inspe set
        | Eval.Inspect(t,inspe,icase,rese) ->
            usd_eval rese (usd_eval inspe set)
      in
      if k < start then
        set
      else
        let set = IntSet.add k set in
        match pt_arr.(k-start) with
          Axiom _ | Assumption _ | Funprop _
        | Indset_rule _ | Indset_ind _ | Indtype _ ->
            set
        | Detached (i,j) ->
            assert (i < k);
            assert (j < k);
            let set = usd i pt_arr set in
            usd j pt_arr set
        | Specialize (i,_,_) | Witness (i,_,_,_)| Someelim i ->
            assert (i < k);
            usd i pt_arr set
        | Eval (i,e) ->
            assert (i < k);
            let set = usd i pt_arr set in
            usd_eval e set
        | Eval_bwd (t,e) ->
            usd_eval e set
        | Subproof (_,_,k1,pt_arr1,_) ->
            let set1 = used_below k k1 k pt_arr1 IntSet.empty in
            IntSet.fold
              (fun i set -> usd i pt_arr set)
              set1
              set
        | Inherit (i,bcls,cls) ->
            assert false
    in
    usd k pt_arr IntSet.empty





  let reindex (start:int) (map: (int*int) array) (k:int) (pt_arr:t array)
      : int * t array =
    (* Remove unused proof terms and reindex the proof terms in the array [pt_arr]

       start: index of the first proof term in [pt_arr]

       n_rem: number of removed terms

       map: old index -> new index,n_removed (-1: indicates that the term is unused)
            n_removed: number of removed terms below [old_index]
            note: indices in map are relative
     *)
    let n = Array.length pt_arr in
    assert (n = Array.length map);
    let rec reidx (start_inner:int) (below:int) (n_rem:int) (k:int) (pt_arr:t array)
        : int * t array =
      assert (n_rem <= n);
      let is_inner = below <= start_inner in
      assert (is_inner || start = start_inner);
      let index (i:int): int =
        if i < start then i
        else if i < below then
          (let idx,_ = map.(i-start) in
          assert (idx <> -1);
          idx + start)
        else
          i - n_rem
      in
      let rec transform_eval (e:Eval.t): Eval.t =
        match e with
          Eval.Term _ | Eval.AsExp _ -> e
        | Eval.Exp (i,ags,args,e) ->
            Eval.Exp(i, ags, Array.map transform_eval args, transform_eval e)
        | Eval.VApply (i,args,ags) ->
            Eval.VApply (i, Array.map transform_eval args, ags)
        | Eval.Apply (f,args,e) ->
            Eval.Apply (transform_eval f,
                        Array.map transform_eval args,
                        transform_eval e)
        | Eval.Lam (tps,fgs,pres,e,rt) ->
            Eval.Lam (tps,fgs,pres,transform_eval e,rt)
        | Eval.QExp (n,tps,fgs,e,ia) ->
            Eval.QExp (n,tps,fgs,transform_eval e,ia)
        | Eval.Beta (e1,e2)   -> Eval.Beta (transform_eval e1, transform_eval e2)
        | Eval.Simpl (e,i,args,ags) ->
            Eval.Simpl (transform_eval e, index i,args,ags)
        | Eval.If (cond,i,args) ->
            Eval.If(cond,index i, Array.map transform_eval args)
        | Eval.As (cond,inspe,tps,pat) ->
           Eval.As (cond, transform_eval inspe,tps,pat)
        | Eval.Inspect (t,inspe,icase,rese) ->
            Eval.Inspect (t, transform_eval inspe, icase, transform_eval rese)
      in
      let transform (i:int) (pt:proof_term): proof_term =
        match pt with
          Axiom _ | Assumption _  | Funprop _
        | Indset_rule _ | Indset_ind _ | Indtype _ ->
            pt
        | Detached (i,j) -> Detached (index i, index j)
        | Eval (i,e)     -> Eval   (index i, transform_eval e)
        | Eval_bwd (t,e) -> Eval_bwd (t, transform_eval e)
        | Specialize (i,args,ags) -> Specialize (index i, args,ags)
        | Witness (i,nms,t,args)  -> Witness (index i, nms, t, args)
        | Someelim i -> Someelim (index i)
        | Subproof (nargs,nms,k,pt_arr1,bubble) ->
            let start_inner = start_inner + i in
            let below = if is_inner then below else start_inner in
            let n_rem = if is_inner then n_rem else snd map.(i) in
            let k,pt_arr = reidx start_inner below n_rem k pt_arr1 in
            assert (k < start_inner + Array.length pt_arr);
            Subproof (nargs, nms, k, pt_arr,bubble)
        | Inherit (i,bcls,cls) ->
            assert false
      in
      let lst,i =
        Array.fold_left
          (fun (lst,i) pt ->
            assert (is_inner || i < n);
            let add = is_inner || (fst map.(i)) <> -1 in
            if add then
              (transform i pt)::lst, i+1
            else
              lst, i+1)
          ([],0)
          pt_arr
      in
      assert (i = Array.length pt_arr);
      (index k), Array.of_list (List.rev lst)
    in
    reidx start (start+n) 0 k pt_arr



  let remove_unused_proof_terms
      (k:int) (start:int) (pt_arr:t array)
      : int * t array =
    (* Remove unused proof terms and reindex the proof terms in the array
       [pt_arr] which proves assertion [k].

       start: index of the first proof term in [pt_arr]

       Result: index of the proved term, array of proof terms
     *)
    let n = Array.length pt_arr in
    assert (0 < n);
    assert (k < start + n);
    let usd  = used k start pt_arr
    and nreq = count_assumptions pt_arr
    and map  = Array.make n (-1,0)
    in
    let pos   = ref nreq
    in
    for i = 0 to nreq-1 do (* all assumptions *)
      map.(i) <- i, 0
    done;
    let n_rem = ref 0
    and next  = ref nreq in
    IntSet.iter
      (fun i ->
        assert (i < start + n);
        if start + nreq <= i then begin (* used, but not assumption *)
          let i_rel = i - start in
          assert (!next <= i_rel);
          assert (!n_rem <= n);
          n_rem := !n_rem + (i_rel - !next);
          map.(i_rel) <- !pos, !n_rem;
          pos   := !pos + 1;
          next  := i_rel + 1
        end)
      usd;
    reindex start map k pt_arr




  let used_in_term (nb:int) (nargs:int) (t:term) (set:IntSet.t): IntSet.t =
    Term.fold
      (fun set ivar ->
        if ivar < nb || nb + nargs <= ivar then
          set
        else
          IntSet.add (ivar-nb) set
      )
      set
      t



  let used_variables (nargs:int) (pt_arr: t array): IntSet.t =
    let rec uvars (nb:int) (pt_arr: t array) (set:IntSet.t): IntSet.t =
      let uvars_term (t:term) (set:IntSet.t): IntSet.t =
        used_in_term nb nargs t set
      in
      let uvars_args (args: term array) (set:IntSet.t): IntSet.t =
        Array.fold_left
          (fun set t -> uvars_term t set)
          set
          args
      in
      Array.fold_left
        (fun set pt ->
          match pt with
            Axiom t
          | Assumption t
          | Indset_rule (t,_)
          | Indset_ind t
          | Eval_bwd (t,_) ->
              uvars_term t set
          | Indtype _
          | Detached _
          | Eval _
          | Someelim _ ->
              set
          | Funprop (_,_,args)
          | Specialize (_,args,_)
          | Witness (_,_,_,args) ->
              uvars_args args set
          | Subproof ((nms,_),_,i,pt_arr,_) ->
              let nb = nb + Array.length nms in
              uvars nb pt_arr set
          | Inherit (i,bcls,cls) ->
              assert false
        )
        set
        pt_arr
      in
    if nargs = 0 then
      IntSet.empty
    else
      uvars 0 pt_arr IntSet.empty


  let remove_unused_variables
      (args:  term array)
      (nargs: int)
      (ags:   type_term array)
      (ntvs:  int)
      (pt_arr:t array)
      : t array =
    (* Remove unused variables in [pt_arr]. The array of proof terms [pt_arr]
       has [args.length,ags.length] variables/type variables but only
       [nargs,ntvs] of them are used. The [args/ags] arrays map variables/type
       variables to their new names ([i -> Variable j]: i: old variable, j:
       new variable). The unused variables map to [Variable (-1)].

       Note: It might be possible that no variables are removed, but that the
       variables are just permuted. *)
    assert (nargs <= Array.length args);
    assert (ntvs  <= Array.length ags);
    let rec shrink (nb:int) (nb2:int) (pt_arr:t array): t array =
      let shrink_inner (t:term) (nba:int) (nb2a:int): term =
        Term.subst0_from t (nb+nba) nargs args (nb2+nb2a) ntvs ags
      in
      let shrink_term (t:term): term = shrink_inner t 0 0
      in
      let shrink_args_inner (args:term array) (nb:int) (nb2:int): term array =
        Array.map (fun a -> shrink_inner a nb nb2) args
      and shrink_list_inner (lst:term list) (nb:int) (nb2:int): term list =
        List.map (fun a -> shrink_inner a nb nb2) lst
      in
      let shrink_args (args:term array): term array = shrink_args_inner args 0 0
      in
      let shrink_type_inner (tp:type_term) (nb2a:int): type_term =
        Term.subst_from tp (nb2+nb2a) ntvs ags in
      let shrink_types_inner (ags:type_term array) (nb2:int): type_term array =
        Array.map (fun tp -> shrink_type_inner tp nb2) ags
      in
      let shrink_type (tp:type_term): type_term =
        Term.subst_from tp nb2 ntvs ags in
      let shrink_types (ags:agens): agens =
        Array.map shrink_type ags
      in
      let shrink_eval (e:Eval.t): Eval.t =
        let var t =
          assert (Term.is_variable t);
          Term.variable t
        in
        let rec shrnk e nb nb2 =
          let shrnk_eargs args = Array.map (fun e -> shrnk e nb nb2) args in
          match e with
            Eval.Term t ->
              Eval.Term (shrink_inner t nb nb2)
          | Eval.AsExp t ->
              Eval.AsExp (shrink_inner t nb nb2)
          | Eval.Exp (idx,ags,args,e) ->
              let idx  = var (shrink_inner (Variable idx) nb nb2)
              and ags  = shrink_types_inner ags nb2
              and args = shrnk_eargs args
              and e    = shrnk e nb nb2 in
              Eval.Exp (idx,ags,args,e)
          | Eval.VApply(i,args,ags) ->
              let i  = var (shrink_inner (Variable i)  nb nb2)
              and args = shrnk_eargs args
              and ags  = shrink_types_inner ags nb2 in
              Eval.VApply (i,args,ags)
          | Eval.Apply(f,args,e) ->
              let f = shrnk f nb nb2
              and args = shrnk_eargs args
              and e = shrnk e nb nb2 in
              Eval.Apply (f,args,e)
          | Eval.Lam (tps,fgs,pres,e,rt) ->
             let shrnk_tp tp = shrink_type_inner tp nb2 in
             Eval.Lam (Formals.map shrnk_tp tps,
                       Formals.map shrnk_tp fgs,
                       shrink_list_inner pres (1+nb) nb2,
                       shrnk e (1+nb) nb2,
                       Option.map shrnk_tp rt)
          | Eval.QExp (n,(nms,tps),(fgnms,fgcon),e,is_all) ->
              assert ((fgnms,fgcon) = empty_formals);
              let nb  = nb + n
              and nb2 = nb2 + Array.length fgnms in
              let e = shrnk e nb nb2
              and tps = shrink_types_inner tps nb2 in
              Eval.QExp (n,(nms,tps),(fgnms,fgcon), e, is_all)
          | Eval.Beta (e1,e2) ->
              Eval.Beta (shrnk e1 nb nb2, shrnk e2 nb nb2)
          | Eval.Simpl (e,idx,args,ags) ->
              let e    = shrnk e nb nb2
              and args = shrink_args_inner args nb nb2
              and ags  = shrink_types_inner ags nb2 in
              Eval.Simpl (e, idx, args, ags)
          | Eval.If(cond,idx,args)-> Eval.If(cond,idx,shrnk_eargs args)
          | Eval.As(cond,inspe,tps,pat) ->
             let n = Array.length tps in
             Eval.As (cond,
                      shrnk inspe nb nb2,
                      shrink_types_inner tps nb2,
                      shrink_inner pat (nb+n) nb2)
          | Eval.Inspect(t,inspe,icase,rese) ->
              let t = shrink_inner t nb nb2
              and inspe = shrnk inspe nb nb2
              and rese  = shrnk rese  nb nb2 in
              Eval.Inspect(t,inspe,icase,rese)
        in
        shrnk e 0 0
      in
      Array.map
        (fun pt ->
          match pt with
            Axiom t ->
              Axiom (shrink_term t)
          | Assumption t ->
              Assumption (shrink_term t)
          | Indset_rule (t,i) ->
              Indset_rule(shrink_term t,i)
          | Indset_ind t ->
              Indset_ind(shrink_term t)
          | Indtype _
          | Detached _ ->
              pt
          | Funprop(idx,i,args) ->
              Funprop(idx,i,shrink_args args)
          | Specialize (i,args,ags) ->
              Specialize (i, shrink_args args, shrink_types ags)
          | Eval (i,e)     -> Eval (i, shrink_eval e)
          | Eval_bwd (t,e) -> Eval_bwd (shrink_term t, shrink_eval e)
          | Witness (i,(nms,tps),t,args) ->
              let nargs = Array.length args
              and args  = shrink_args args
              and tps   = shrink_types tps
              in
              let t = shrink_inner t nargs 0 in
              Witness (i,(nms,tps),t,args)
          | Someelim i ->
              Someelim i
          | Subproof ((nms,tps),(fgnms,fgcon),i,pt_arr,bubble) ->
              let nb,nb2 = nb + Array.length nms, nb2 + Array.length fgnms
              in
              let pt_arr = shrink nb nb2 pt_arr
              and tps    = shrink_types tps
              and fgcon  = shrink_types fgcon
              in
              Subproof ((nms,tps),(fgnms,fgcon),i, pt_arr,bubble)
          | Inherit (i,bcls,cls) ->
              assert false
        )
        pt_arr
    in
    shrink 0 0 pt_arr


  let normalize_pair
      (nargs:int) (nms:int array) (start:int) (t:term) (pt_arr: t array)
      : int * int array * term  * t array =
    assert false
    (*assert (nargs = Array.length nms);
    let usd,pos = Term.used_variables_transform t nargs in
    let nargs1 = Array.length usd in
    let uvars_pt = used_variables nargs pt_arr in
    if not (nargs1 = IntSet.cardinal uvars_pt &&
            interval_for_all (fun i -> IntSet.mem usd.(i) uvars_pt) 0 nargs1)
    then begin
      printf "normalize_pair\n";
      printf "   nargs %d, nargs1 %d, card uvars_pt %d, nms %s\n"
        nargs nargs1 (IntSet.cardinal uvars_pt)
        ("(" ^ (String.concat ","
                  (List.map Support.ST.string (Array.to_list nms))) ^ ")");
      raise Not_found end;
    let args = Array.map (fun i -> Variable i) pos
    and nms1 = Array.init nargs1 (fun i -> nms.(usd.(i))) in
    let t      = Term.sub t args nargs1
    and pt_arr = remove_unused_variables args nargs1 pt_arr in
    nargs1, nms1, t, pt_arr
*)




  let split_subproof (pt:t): formals0 * formals0 * int * t array =
    match pt with
      Subproof (nb,nms,i,pt_arr,_) -> nb,nms,i,pt_arr
    | _ -> raise Not_found


  let is_subproof (pt:t): bool =
    try ignore(split_subproof pt); true
    with Not_found -> false


  let short_string (pt:t): string =
    match pt with
      Axiom _  -> "ax"
    | Assumption _ -> "ass"
    | Funprop _ -> "prop"
    | Indset_rule (_,_) -> "rule"
    | Indset_ind _ -> "indset"
    | Indtype _    -> "ind"
    | Detached (i,j) -> "mp " ^ (string_of_int i) ^ " " ^ (string_of_int j)
    | Specialize (i,_,_) -> "spec " ^ (string_of_int i)
    | Inherit (i,bcls,cls)     -> "inh " ^ (string_of_int i)
    | Eval (i,_)          -> "eval " ^ (string_of_int i)
    | Eval_bwd _          -> "eval"
    | Witness (i,nms,t,args) -> "wit " ^ (string_of_int i)
    | Someelim i             -> "selim " ^ (string_of_int i)
    | Subproof (nargs,names,res,pt_arr,_) -> "sub"
end
