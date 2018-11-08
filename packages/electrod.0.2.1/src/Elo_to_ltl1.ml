(*******************************************************************************
 * electrod - a model finder for relational first-order linear temporal logic
 * 
 * Copyright (C) 2016-2018 ONERA
 * Authors: Julien Brunel (ONERA), David Chemouil (ONERA)
 * 
 * This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/.
 * 
 * SPDX-License-Identifier: MPL-2.0
 * License-Filename: LICENSE.md
 ******************************************************************************)

(** Functor that provides a {!Elo_to_LTL_intf.S} converter given an
    implementation of LTL *)

open Containers
open Exp_bounds

module G = Elo
module TS = Tuple_set

type stack = Tuple.t list

let pp_subst out subst =
  Fmtc.(brackets @@ list @@ parens @@ pair int Tuple.pp) out
    (List.mapi (fun i tuple -> (i, tuple)) subst)

let all_different ~eq xs =
  let rec walk acc = function
    | [] -> true
    | [hd] -> not @@ List.mem ~eq hd acc
    | hd::tl -> not @@ List.mem ~eq hd acc && walk (hd::acc) tl
  in walk [] xs

module Make (Ltl : Solver.LTL) = struct
  open Ltl
  open Ltl.Infix

  type atomic = Ltl.Atomic.t

  type ltl = Ltl.t

  type goal = G.t

  (***************************************************************** 
   * Semantic function
   ****************************************************************)

  (* FIRST: some functions used for the semantics of a transitive closure. *)

  (* given a 2-tuple set ts, this function computes the domain and the
     co-domain of ts, i.e., the set (sequence) of atoms that are the
     first elements of a 2-tuple in ts, and the set (sequence) of atoms
     thare are the second elements of a 2-tuple in ts *)
  let compute_domain_codomain ts =
    let ar = TS.inferred_arity ts in
    assert (ar = 2);
    let module S = Sequence in
    let s = TS.to_seq ts in
    let split_seq (s1_acc, s2_acc) tup =
      (S.cons (Tuple.ith 0 tup) s1_acc,
       S.cons (Tuple.ith 1 tup) s2_acc)
    in
    S.fold split_seq (S.empty, S.empty) s
    |> Fun.tap
    @@ fun res ->
    Msg.debug (fun m ->
          m "compute_domain_codomain(%a) --> (ar = %d)@ = %a"
            TS.pp ts
            ar
            (Fmtc.parens @@
             Pair.pp ~sep:", "
               (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp)
               (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp)) res)


  (* given a 2-tuple set, this function computes the maximum length of
     a path (x1, ... xn) such that each 2-tuple (xi, xi+1) is in the
     tuple set.  Used to compute the number of iterations needed for
     transitive closure term. *)
  let compute_tc_length ts =
    let tsarity = TS.inferred_arity ts in
    Msg.debug (fun m ->
          m "compute_tc_length: arity of relation : %d\n" tsarity);

    assert (tsarity = 2 || tsarity = 0);
    if tsarity = 0 then 0
    else
      let module S = Sequence in
      let dom, cod = compute_domain_codomain ts in    
      let core_ats = S.inter ~eq:Atom.equal ~hash:Atom.hash dom cod in
      Msg.debug (fun m ->
            m "compute_tc_length: inter %a %a = %a\n"
              (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp) dom
              (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp) cod
              (Fmtc.braces_ @@ S.pp_seq ~sep:", " Atom.pp) core_ats
          );   
      let core_length = S.length core_ats in
      (* is it possible that x1 is not in the core (intersection of the
         domain and the codomain) ? *)
      let first_elt_in_core =
        S.subset ~eq:Atom.equal ~hash:Atom.hash dom core_ats in
      Msg.debug (fun m ->
            m "compute_tc_length: first_elt_in_core = %B\n"
              first_elt_in_core
          );

      (* is it possible that xn is not in the core (intersection of the
         domain and the codomain) ? *)
      let last_elt_in_core =
        S.subset ~eq:Atom.equal ~hash:Atom.hash cod core_ats in
      Msg.debug (fun m ->
            m "compute_tc_length: last_elt_in_core = %B\n"
              last_elt_in_core
          );

      (match first_elt_in_core, last_elt_in_core with
        | true, true -> core_length
        | false, false -> core_length + 2
        | _ -> core_length + 1)

      |> Fun.tap (fun res ->
            Msg.debug (fun m -> m "compute_tc_length --> length = %d" res))


  (* computes the transitive closure of the term acc_term by k iterative
     squares (t+t.t)+(t+t.t)(t+t.t) + ... *)

  let rec iter_squares (acc_term : G.exp) k =
    match k with
      | 0 -> G.none
      | 1 -> acc_term
      | _ ->
          let ar = G.arity acc_term in
          let new_exp =
            G.(rbinary ~ar acc_term union
               @@ rbinary ~ar acc_term join acc_term)
          in
          iter_squares new_exp (max (k lsr 1) ((k + 1) lsr 1))

  (* computes the transitive closure of the term t by k joins
     (alternative to iter_squares) t + t.t + t.t.t + ... *)

  let iter_tc (t : G.exp) k =
    if k = 0 then G.none
    else
      let ar = G.arity t in
      let t_to_the_k = ref t in
      let tc = ref t in
      for _ = 2 to k do
        t_to_the_k := G.(rbinary ~ar !t_to_the_k join t);
        tc := G.(rbinary ~ar !tc union !t_to_the_k);
      done;
      !tc


  (* utility function for build_Join *)
  let eligible_pairs (tuple, r_sup, s_sup : Tuple.t * TS.t * TS.t)
    : (Tuple.t * Tuple.t) Sequence.t =
    let open List in
    let r_sup_list = TS.to_list r_sup in
    let s_sup_list = TS.to_list s_sup in
    fold_left (fun pairs x_r ->
          filter_map
            (fun x_s ->
               if Tuple.is_in_join tuple x_r x_s then
                 Some (x_r, x_s)
               else
                 None) s_sup_list
          |> rev_append pairs) empty r_sup_list
    |> to_seq




  class environment (elo : Elo.t) = object (_ : 'self)
    val bounds_exp_aux = Exp_bounds.make_bounds_exp elo.Elo.domain

    method must_may_sup (subst : stack) (exp : G.exp) =
      bounds_exp_aux (exp, subst)

    method relation_arity name =
      match Domain.get name elo.Elo.domain with
        | None -> assert false
        | Some rel -> Relation.arity rel

    method make_atom (name : Name.t) (t : Tuple.t) =
      assert (Domain.mem name elo.Elo.domain);
      Ltl.atomic @@ Atomic.make elo.Elo.domain name t 

    method is_const (name : Name.t) =
      assert (Domain.mem name elo.Elo.domain);
      Domain.get_exn name elo.Elo.domain |> Relation.is_const
  end

  class ['subst] converter (env : environment) = object (self : 'self)
    constraint 'subst = stack  (* a stack *)

    inherit ['self] Elo_recursor.recursor 

    method build_Add (_ : stack) (a : term) (b : term) : term = plus a b
    method build_All (_ : stack) = G.all
    method build_And (_ : stack) (a : ltl) (b : ltl) : ltl = and_ a (lazy b)
    method build_Block (_ : stack) = conj
    method build_Card subst r r' =
      let { must; may; _ } = env#must_may_sup subst r in
      let must_card = num @@ TS.size must in
      let may_card =
        count @@ List.map r' @@ TS.to_list may
      in
      plus must_card may_card

    (* re-defining this method to avoid going down in the block as a
       substitution must be made first *)
    method! visit_Compr env _visitors_c0 _visitors_c1 =
      let _visitors_r0 =
        self#visit_list
          (fun env (_visitors_c0, _visitors_c1, _visitors_c2) ->
             let _visitors_r0 =
               (fun _visitors_this -> _visitors_this) _visitors_c0 in
             let _visitors_r1 =
               (fun _visitors_this -> _visitors_this) _visitors_c1 in
             let _visitors_r2 = self#visit_'exp env _visitors_c2 in
             (_visitors_r0, _visitors_r1, _visitors_r2)) env
          _visitors_c0 in
      let _visitors_r1 = [true_] in
      self#build_Compr env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    method private allocate_sbs_to_tuples
                     (ranges : G.exp list)
                     (tuple : Tuple.t) : Tuple.t list =
      let rec walk ranges atoms = match ranges with
        | [] -> []
        | hd::tl ->
            let xs, ys = List.take_drop (G.arity hd) atoms in 
            Tuple.of_list1 xs :: walk tl ys
      in 
      walk ranges @@ Tuple.to_list tuple

    (* check if the disj's in the comprehension sim_bindings are respected *)
    method private check_compr_disj 
                     (sbs : (bool * int * G.exp) list) (split_tuples : Tuple.t list) : bool =
      let rec walk sbs tuples = match sbs with
        | [] -> true
        | (true, nbvars, _)::tl -> 
            let xs, ys = List.take_drop nbvars tuples in 
            let alldiff = all_different ~eq:Tuple.equal xs in
            Msg.info (fun m -> 
                  m "check_compr_disj (true, %d, _) tuples = %a alldiff = %B"
                    nbvars
                    Fmtc.(brackets @@ list ~sep:sp @@ Tuple.pp) tuples
                    alldiff
                );            
            alldiff &&  walk tl ys
        | (false, nbvars, _)::tl ->
            let ys = List.drop nbvars tuples in 
            walk tl ys
      in walk sbs split_tuples


    (* shape: [{ sb1, sb2,... | b }]. Each [sb] is of shape [disj nbvar: e] .

       The first item implies that we have to fold over the [sb]'s to substitute
       previously-bound variables. In the following function, we perform these
       substitutions and then compute separately the semantics of every binding,
       before computing the whole resulting formula.
    *)
    method build_Compr
             (subst : stack) 
             (sbs : (bool * int * G.exp) list) (body : G.fml list) 
             __sbs' __body' tuple = 
      let compr_ar = 
        List.fold_left (fun acc (_, n, r) -> acc + n * G.arity r) 0 sbs in 
      let depth = List.length subst in 
      (if Tuple.arity tuple <> compr_ar then
         Msg.err (fun m -> 
               m "%s.build_Compr [[{%a@ |@ %a}]]_%a(%a): \
                  tuple arity (%d) incompatible with expression arity (%d)"
                 __MODULE__
                 (G.pp_sim_bindings depth) sbs
                 (G.pp_block depth) body
                 pp_subst subst 
                 Tuple.pp tuple
                 (Tuple.arity tuple)
                 compr_ar
             ));
      (* the tuple is (in principle) of arity equal to the sum of arities of ranges of bound variables. To build the corresponding substitutions, we must first split this tuple into as many tuples as variables, each one with the adequate arity *)
      let ranges = 
        List.flat_map (fun (_, nbvars, range) -> List.repeat nbvars [range]) sbs
      in 
      let split_tuples = self#allocate_sbs_to_tuples ranges tuple 
      in 
      if self#check_compr_disj sbs split_tuples then
        (* semantics of [b] is [[ b [tuples / variables] ]] *)
        let b' = self#visit_fml (List.rev split_tuples @ subst) @@ G.block body
        in 
        (* every single sim_binding contains possibly many variables and they may depend over previous bindings of the same comprehension. Because of the many variables, we use [fold_flat_map] which is like a fold returning a pair of an accumulator and a list, the latter undergoing flattening *)
        let (_, ranges') = 
          List.fold_flat_map
            (fun (acc_split_tuples, acc_subst) (_, nbvars, r) ->
               let boundvars, remaining = 
                 List.take_drop nbvars acc_split_tuples 
               in
               let r' = self#visit_exp acc_subst r (Tuple.concat boundvars) in 
               (* copy range nbvars times *)
               let rs' = List.repeat nbvars [r'] in 
               let new_subst = List.rev boundvars @ acc_subst in
               ((remaining, new_subst), rs')
            )
            (split_tuples, subst)
            sbs 
        in 
        conj (b' :: ranges')
        |> Fun.tap (fun res -> 
              Msg.debug (fun m -> m "build_Compr --> %a" pp res))
      else 
        (Msg.debug (fun m -> m "build_Compr --> false (disj case)"); 
         false_)

    method build_Diff (_ : stack) (_ : G.exp) (_ : G.exp) e' f' = 
      fun (tuple : Tuple.t) ->
        e' tuple +&& lazy (not_ (f' tuple))
    method build_F (_ : stack) (a : ltl) : ltl = eventually a
    method build_FIte (_ : stack) _ _ _ (c : ltl) (t : ltl) (e : ltl) : ltl  = 
      ifthenelse c t e
    method build_False (_ : stack) : ltl = false_
    method build_G (_ : stack) (a : ltl) : ltl = always a
    method build_Gt (_ : stack) : tcomp = gt
    method build_Gte (_ : stack) : tcomp = gte
    method build_H (_ : stack) (a : ltl) : ltl = historically a
    method build_IBin (_ : stack) _ _ _ i1' op' i2' = op' i1' i2'
    method build_IComp (_ : stack) __e1 _ __e2 e1_r op e2_r = comp op e1_r e2_r
    method build_IEq (_ : stack) : tcomp = eq
    method build_INEq (_ : stack) : tcomp = neq
    method build_IUn (_ : stack) _ _ op' i' = op' i'
    method build_Iden (_ : stack) tuple = 
      assert (Tuple.arity tuple = 2);
      if Atom.equal (Tuple.ith 0 tuple) (Tuple.ith 1 tuple) then
        true_
      else
        false_
    method build_Iff (_ : stack) (a : ltl) (b : ltl) : ltl = iff a b
    method build_Imp (_ : stack) (a : ltl) (b : ltl) : ltl = implies a (lazy b)
    method build_In subst r (__s : G.exp) r' s' =
      let { must; may; _} = env#must_may_sup subst r in
      wedge ~range:(TS.to_seq must) (fun t -> lazy (s' t))
      +&& lazy (wedge ~range:(TS.to_seq may)
                  (fun bs -> lazy (r' bs @=> lazy (s' bs))))
    method build_Inter (_ : stack) _ _ e1 e2 tuple =
      e1 tuple +&& lazy (e2 tuple)
    method build_Join subst r s r' s' tuple =
      let sup_r = (env#must_may_sup subst r).sup in
      let sup_s = (env#must_may_sup subst s).sup in
      let pairs = eligible_pairs (tuple, sup_r, sup_s) in
      vee ~range:pairs (fun (bs, cs) -> lazy (r' bs +&& lazy (s' cs)))
    method build_LBin (_ : stack) _ _ _ f1' op' f2' =
      op' f1' f2'
    method build_LProj (_ : stack) _ _ s' r' tuple =
      (s' @@ Tuple.(of_list1 [ith 0 tuple])) +&& lazy (r' tuple)
    method build_LUn (_ : stack) _ _ op' f' =
      op' f'
    method build_Lt (_ : stack) : tcomp = lt
    method build_Lte (_ : stack) : tcomp = lte
    method build_Name (subst : stack) rel _ tuple =
      let { must; may; _ } =
        env#must_may_sup subst 
        @@ G.name ~ar:(env#relation_arity rel) rel
      in
      if TS.mem tuple must then
        true_
      else if TS.mem tuple may then 
        env#make_atom rel tuple
      else
        false_
    method build_Neg (_ : stack) (a : term) : term = neg a
    method build_No (_ : stack) = G.no_
    method build_None_ (_ : stack) __tuple = false_
    method build_Not (_ : stack) (a : ltl) : ltl = not_ a
    method build_NotIn (subst : stack) r s r' s' =
      not_ @@ self#build_In subst r s r' s'
    method build_Num (_ : stack) n _ = num n
    method build_O (_ : stack) (a : ltl) : ltl = once a
    method build_Or (_ : stack) (a : ltl) (b : ltl) : ltl = or_ a (lazy b)
    method build_Over (subst : stack) __r s r' s' tuple = 
      let { must; may; _ } = env#must_may_sup subst s in
      let proj1 x = Tuple.(of_list1 [ith 0 x]) in
      let mustpart =
        wedge
          ~range:(TS.to_seq must)
          (fun t -> lazy (if Tuple.equal (proj1 t) (proj1 tuple)
                          then false_ else true_))
      in
      (* [newmay] helps compute the last AND above: it removes duplicate tuples
         that may appear in this translation: *)
      let newmay =
        let mktup t =
          let _, from_snd_elt = Tuple.split t 1 in
          Tuple.(proj1 tuple @@@ from_snd_elt)
        in
        TS.map mktup may
      in
      let maypart =
        not_ @@ vee ~range:(TS.to_seq newmay) (fun t -> lazy (s' t))
      in
      s' tuple +|| lazy (mustpart +&& lazy ((r' tuple) +&& lazy maypart))
    method build_P (_ : stack) (a : ltl) : ltl = yesterday a
    method build_Prime (_ : stack) _ e' tuple = 
      next @@ e' tuple
    method build_Prod (_ : stack) r s r' s' tuple =
      (* we need to split [tuple] so we need the arity of [r]. If the
         arity is [None] (for 'none'), then we must just return
         false. Otherwise the tuple is split. *)
      match G.arity r, G.arity s with
        | 0, _
        | _, 0 -> false_
        | ar_r, _ ->
            let t1, t2 = Tuple.split tuple ar_r in
            r' t1 +&& lazy (s' t2)
    method! visit_Quant subst q sb block =
      let q' = self#visit_quant subst q in
      let sb' =
        (fun (disj, nbvars, range) ->
           let range' = self#visit_'exp subst range in
           (disj, nbvars, range')) sb in
      let range' = [true_] in 
      self#build_Quant subst q sb block q' sb' range'

    method build_Quant subst quant (disj, nbvars, s) blk _ (_, _, s') _ = 
      let tuples_of_sim_binding (dom : Tuple.t list) : Tuple.t list Sequence.t =
        let open List in
        (* create as many copies as necessary (= nb of variables) of the domain *)
        init nbvars (fun __idx -> dom)
        (* take their cartesian product *)
        |> cartesian_product
        (* remove lines where there are tuples in common if [disj = true] *)
        |> (if disj then
              filter
                (fun l ->
                   let sorted = sort_uniq ~cmp:Tuple.compare l in
                   length l = length sorted)
            else Fun.id)
        |> to_seq
      in
      (* [pos_or_neg] tells whether the quantifier was a [no ...], in
         which case we consider the whole as [all ... | not ...]. [link]
         tells how to connect a premise and a test in the may part of the
         formula. *)
      let (bigop, smallop, link, pos_or_neg) = match quant with
        | G.All -> (wedge, and_, implies, Fun.id)
        | G.Some_ -> (vee, or_, and_, Fun.id)
        | G.No -> (wedge, and_, implies, not_)
      in
      let sem_of_substituted_blk tuples = 
        lazy (pos_or_neg
              @@ (self#visit_fml @@ List.rev tuples @ subst) (* [[...]] *)
              @@ G.block blk) 
      in
      let { must; may; _ } = env#must_may_sup subst s in
      let mustpart =
        bigop
          ~range:(tuples_of_sim_binding @@ TS.to_list must)
          (fun tuples -> sem_of_substituted_blk tuples)
      in
      let maypart =
        lazy 
          (bigop
             ~range:(tuples_of_sim_binding @@ TS.to_list may)
             (fun tuples ->
                (* if several variables were bound to the same range, then
                   we must apply the characteristic function thereof to
                   every candidate tuples for these variables; and then
                   take the conjunction. Note: if several variables range
                   in the same set, then we will apply the characteristic
                   function many times to the same tuples: as an
                   optimization, we keep --only in the computation of the
                   premise-- only unique tuples to avoid this superfluous
                   repetition. *)
                let premise = 
                  wedge
                    ~range:List.(to_seq @@ sort_uniq ~cmp:Tuple.compare tuples)
                    (fun tuple -> lazy (s' tuple))
                in
                (* Msg.debug (fun m -> m "(build_Quant.premise) %a" Ltl.pp premise); *)
                lazy (link premise @@ sem_of_substituted_blk tuples)
             ))
      in
      (smallop mustpart maypart)

    method build_R (_ : stack) (a : ltl) (b : ltl) : ltl = releases a b
    method build_RBin (_ : stack) (a : G.exp) (_ : G.rbinop) (b : G.exp) a' op' b' tuple = op' a b a' b' tuple
    method build_RComp (_ : stack) f1 __op f2 f1' op' f2' =
      op' f1 f2 f1' f2'
    method build_REq subst r s r' s' =
      let r_bounds = env#must_may_sup subst r in
      let s_bounds = env#must_may_sup subst s in
      let inter = TS.inter r_bounds.may s_bounds.may in
      wedge ~range:(TS.to_seq r_bounds.must) (fun t -> lazy (s' t))
      +&& lazy (wedge ~range:(TS.to_seq s_bounds.must) (fun t -> lazy (r' t)))
      +&& lazy (wedge ~range:(TS.to_seq inter) (fun bs -> lazy (r' bs @<=> s' bs)))
      +&& lazy (wedge ~range:(TS.to_seq @@ TS.diff r_bounds.may inter)
                  (fun bs -> lazy (r' bs @=> lazy (s' bs))))
      +&& lazy (wedge ~range:(TS.to_seq @@ TS.diff s_bounds.may inter)
                  (fun bs -> lazy (s' bs @=> lazy (r' bs))))
    method build_RIte (_ : stack) (__c : G.fml) (__t : G.exp) __e c' t' e' tuple =   
      (c' @=> lazy (t' tuple)) +&& lazy ((not_ c') @=> lazy (e' tuple))
    method build_RNEq (subst : stack) r s r' s' =
      not_ @@ self#build_REq subst r s r' s'
    method build_RProj (_ : stack) _ _ r' s' tuple =
      let lg = Tuple.arity tuple in
      (s' @@ Tuple.of_list1 [Tuple.ith (lg - 1) tuple]) +&& lazy (r' tuple)
    method build_RTClos subst r _ = fun tuple ->
      self#build_Iden subst tuple 
      +|| lazy (self#visit_RUn subst G.tclos r tuple)
    method build_RUn (_ : stack) (_ : G.runop) (e : G.exp) op' e'  = op' e e'
    method build_S (_ : stack) (a : ltl) (b : ltl) : ltl = since a b
    method build_Some_ (_ : stack) = G.some
    method build_Sub (_ : stack) (a : term) (b : term) : term = minus a b
    method build_TClos subst r __r' =
      Msg.debug
        (fun m -> m "%s.build_TClos <-- %a"
                    __MODULE__  
                    G.(pp_exp (arity r)) r);
      let { sup ; _ } = env#must_may_sup subst r in
      let k = compute_tc_length sup in
      (* let tc_naif = iter_tc r k in *)
      let tc_square = iter_squares r k in
      Msg.debug (fun m ->
            m "TC bound: %d" k);
      Msg.debug (fun m ->
            m "TC term using iterative squares: %a" 
              G.(pp_exp (arity tc_square)) (tc_square));
      self#visit_exp subst tc_square
    method build_Transpose (_ : stack) _ r' tuple =
      r' @@ Tuple.transpose tuple
    method build_True (_ : stack) = true_
    method build_U (_ : stack) (a : ltl) (b : ltl) : ltl = until a b
    method build_Union (_ : stack) _ _ e1 e2 =
      (fun x -> e1 x +|| lazy (e2 x))
    method build_Univ (_ : stack) __tuple = true_
    method build_Var (subst : stack) idx _ tuple = 
      match List.get_at_idx idx subst with
        | None -> 
            Fmtc.kstrf failwith "%s.build_Var: variable %d not found in %a" __MODULE__ 
              idx
              pp_subst subst
        | Some value ->
            if Tuple.equal value tuple
            then
              true_
            else
              false_
    method build_X (_ : stack) (a : ltl) : ltl = next a
    method build_oexp (_ : stack) __e e' __ar tuple = e' tuple

  end (* class *)

  let formula_as_comment fml =
    let str = Fmt.to_to_string (Elo.pp_fml 0) fml in
    "-- " ^ String.replace ~which:`All ~sub:"\n" ~by:"\n-- " str

  (* Converts an Ast formula to an LTL formula, gathering at the same time the
     rigid and flexible variables having appeared during the walk. *)
  let convert elo elo_fml =
    let env = new environment elo in    
    let ltl_fml = (new converter env)#visit_fml [] elo_fml in
    (formula_as_comment elo_fml, ltl_fml)


end