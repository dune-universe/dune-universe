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


module G = GenGoal
module TS = TupleSet


module Make (Ltl : Solver.LTL) = struct
  open Ltl
  open Ltl.Infix

  type atomic = Ltl.Atomic.t

  type ltl = Ltl.t

  type goal = Elo.goal

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

  let rec iter_squares (acc_term : (Elo.var, Elo.ident) G.exp) k =
    let open Location in
    match k with
      | 0 -> G.(exp None dummy none)
      | 1 -> acc_term
      | _ ->
          let new_exp =
            G.(exp acc_term.arity dummy
               @@ rbinary acc_term union
                    (exp acc_term.arity dummy @@ rbinary acc_term join acc_term))
          in
          iter_squares new_exp (max (k lsr 1) ((k+1) lsr 1))

  (* computes the transitive closure of the term t by k joins
     (alternative to iter_squares) t + t.t + t.t.t + ... *)

  let iter_tc (t : (Elo.var, Elo.ident) G.exp) k =
    let open Location in
    if k=0 then G.(exp None dummy none)
    else
      let t_to_the_k = ref t in
      let tc = ref t in
      for _ = 2 to k do
        t_to_the_k := G.(exp t.arity dummy @@ rbinary !t_to_the_k join t);
        tc := G.(exp t.arity dummy @@ rbinary !tc union !t_to_the_k);
      done;
      !tc


  (* utility function for build_Join *)
  let eligible_pairs (tuple, r_sup, s_sup : Tuple.t * TS.t * TS.t)
    : (Tuple.t * Tuple.t) Sequence.t =
    (* Msg.debug (fun m -> *)
    (*       m "eligible_pairs <-- %a %a %a" *)
    (*         Tuple.pp tuple *)
    (*         TS.pp r_sup *)
    (*         TS.pp s_sup *)
    (*     ); *)
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
  (* |> Fun.tap (fun res -> *)
  (*       Msg.debug (fun m -> *)
  (*             m "eligible_pairs --> %a" *)
  (*               Fmtc.(braces_ @@ Sequence.pp_seq *)
  (*                     @@ parens @@ Pair.pp ~sep:", " Tuple.pp Tuple.pp) res)) *)



  class ['subst] converter env = object (self : 'self)
    constraint 'subst = (Var.t, Tuple.t) CCList.Assoc.t

    inherit ['self] GenGoalRecursor.recursor 

    method visit_'v __subst = Fun.id

    method visit_'i __subst = Fun.id

    (* fml  *)                        

    method build_fml __subst _ ltl _ = ltl

    method build_Run __subst _ = Fun.id

    method build_True __subst = true_

    method build_False __subst = false_

    method build_Block __subst _ = conj

    method build_FIte __subst _ _ _ = ifthenelse 

    method build_Let __subst __bs __block __bs' __block'= assert false (* SIMPLIFIED *)

    (* quant *)

    (* re-defining this method to avoid going down in the block as a
       substitution must be made first *)
    method! visit_Quant env _visitors_c0 _visitors_c1 _visitors_c2 =
      let _visitors_r0 = self#visit_quant env _visitors_c0  in
      let _visitors_r1 =
        self#visit_list self#visit_sim_binding env _visitors_c1  in
      let _visitors_r2 = [true_]  in
      self#build_Quant env _visitors_c0 _visitors_c1 _visitors_c2
        _visitors_r0 _visitors_r1 _visitors_r2

    method build_Quant subst quant sim_bindings blk _ sim_bindings' _ =
      Msg.debug
        (fun m -> m "build_Quant <-- %a"
                    (Elo.pp_prim_fml)
                    (G.quant quant sim_bindings blk)
        );
      match quant with
        | G.Lone | G.One ->
            assert false        (* SIMPLIFIED *)
        | G.All | G.Some_ | G.No ->
            assert (List.length sim_bindings = 1); (* SIMPLIFIED *)
            let disj, xs, s = List.hd sim_bindings in
            let _, _, s' = List.hd sim_bindings' in
            let tuples_of_sim_binding ~disj (vars : Elo.var list) (dom : Tuple.t list) =
              let open List in
              (* (\* substituitons take [Var.t] keys *\) *)
              (* let xs = map (fun (Elo.BVar v) -> v) vars in *)
              (* let tuples_as_idents = map (fun t1 -> G.ident @@ Elo.Tuple t1) dom in *)
              let lg = length vars in
              (* create as many copies as necessary (= nb of variables) of the domain *)
              init lg (fun _ -> dom)
              (* take their cartesian product *)
              |> cartesian_product
              (* remove lines where there are tuples in common if [disj = true] *)
              |> (if disj then
                    filter
                      (fun l ->
                         let sorted = sort_uniq ~cmp:Tuple.compare l in
                         length l = length sorted)
                  else Fun.id)
              (* |> Fun.tap (fun res -> Msg.debug (fun m-> *)
              (*       m "tuples_of_sim_binding (disj:%B) vars:%a domain:%a@\n  -->@ %a " *)
              (*         disj *)
              (*         Fmtc.(brackets @@ list ~sep:sp Elo.pp_var) vars *)
              (*         Fmtc.(braces @@ list ~sep:comma Tuple.pp) dom *)
              (*         Fmtc.(vbox @@ brackets @@ list ~sep:cut *)
              (*               @@ hvbox2 @@ brackets *)
              (*               @@ list ~sep:comma Tuple.pp) res *)
              (*     )) *)
              |> to_seq
            in
            let sub_for tuples =
              (* let tuples_as_idents = *)
              (*   List.map (fun t1 -> G.ident @@ Elo.Tuple t1) tuples in *)
              let xs_as_vars = List.map (fun (Elo.BVar v) -> v) xs in
              (* we zip the bound variables and the 1-tuples to get a list of
                 substitutions *)
              List.combine xs_as_vars tuples
              (* |> Fun.tap (fun res -> *)
              (*       Msg.debug (fun m -> *)
              (*             m "sub_for %a = %a" *)
              (*               (Fmtc.(list Tuple.pp)) tuples *)
              (*               (Fmtc.(brackets *)
              (*                      @@ list *)
              (*                      @@ pair ~sep:(const string ":=") Var.pp *)
              (*                      @@ Tuple.pp)) res *)
              (*           )) *)
            in

            (* [pos_or_neg] tells whether the quantifier was a [no ...], in
               which case we consider the whole as [all ... | not ...]. [link]
               tells how to connect a premise and a test in the may part of the
               formula. *)
            let (bigop, smallop, link, pos_or_neg) = match quant with
              | G.All -> (wedge, and_, implies, Fun.id)
              | G.Some_ -> (vee, or_, and_, Fun.id)
              | G.No -> (wedge, and_, implies, not_)
              | G.Lone | G.One -> assert false (* SIMPLIFIED *)
            in
            let { must; may; _ } = env#must_may_sup subst s in
            let sem_of_substituted_blk tuples = 
              lazy (pos_or_neg
                    @@ (self#visit_prim_fml @@ sub_for tuples @ subst) (* [[...]] *)
                    @@ G.block blk
                    (* |> Fun.tap (fun s -> *)
                    (*       Msg.debug (fun m -> *)
                    (*             m "build_Quant: [[%a]]_[%a] -->@ %a" *)
                    (*               Elo.pp_block blk *)
                    (*               Fmtc.(list @@ Pair.pp ~sep:":=" Var.pp Tuple.pp) (sub_for tuples @ subst) *)
                    (*               Ltl.pp s *)
                    (*           )) *)
                   ) (* blk [tuples / xs] *)
            in
            Msg.debug (fun m ->
                  m "build_Quant: ENTERING MUSTPART, range = (%a)"
                    Fmtc.(Sequence.pp_seq ~sep:", " @@ brackets @@ list @@ Tuple.pp)
                    (tuples_of_sim_binding ~disj xs @@ TS.to_list must)
                );
            let mustpart =
              bigop
                ~range:(tuples_of_sim_binding ~disj xs @@ TS.to_list must)
                (fun tuples -> sem_of_substituted_blk tuples)
            in
            Msg.debug (fun m ->
                  m "build_Quant: must(%a) = %a@\nmustpart = %a@\nENTERING MAYPART"
                    Elo.pp_exp s
                    TS.pp must
                    Ltl.pp mustpart);
            let maypart =
              lazy 
                (bigop
                   ~range:(tuples_of_sim_binding ~disj xs @@ TS.to_list may)
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
            Msg.debug (fun m ->
                  m "build_Quant: may(%a) = %a@\nmaypart = %a"
                    (Elo.pp_exp) s
                    TS.pp may
                    Ltl.pp (Lazy.force maypart));
            (smallop mustpart maypart)
            |> Fun.tap (fun res ->
                  Msg.debug (fun m ->
                        m "build_Quant [[%a %a %a]] -->@ %a"
                          G.pp_quant quant
                          (Fmtc.(list ~sep:comma) @@ Elo.pp_sim_binding) sim_bindings
                          Elo.pp_block blk
                          Ltl.pp res
                      ))

    method build_One __subst = G.One

    method build_Lone __subst = G.Lone

    method build_All __subst = G.All

    method build_No __subst = G.No 

    method build_Some_ __subst = G.Some_    

    (* lbinop *)      

    method build_LBin __subst f1 __op f2 f1' op' f2' =
      op' f1 f2 f1' f2'
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug (fun m -> *)
    (*             m "build_LBin [[%a %a %a]] -->@ %a" *)
    (*               Elo.pp_fml f1 *)
    (*               G.pp_lbinop op *)
    (*               Elo.pp_fml f2 *)
    (*               Ltl.pp res *)
    (*           )) *)

    method build_And __subst _ _ = fun a b -> and_ a (lazy b)

    method build_Iff __subst _ _ = iff

    method build_Imp __subst _ _ = fun a b -> implies a (lazy b)

    method build_U __subst _ _ = until

    method build_Or __subst _ _ = fun a b -> or_ a (lazy b)

    method build_R __subst _ _ = releases

    method build_S __subst _ _ = since

    (* lunop *)                     

    method build_LUn __subst __op f op' f' =
      op' f f'
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug (fun m -> *)
    (*             m "[[%a %a]] -->@ %a" *)
    (*               G.pp_lunop op *)
    (*               Elo.pp_fml f *)
    (*               Ltl.pp res *)
    (*           )) *)

    method build_X __subst _ = next

    method build_F __subst _ = eventually

    method build_G __subst _ = always

    method build_H __subst _ = historically

    method build_O __subst _ = once

    method build_P __subst _ = yesterday

    method build_Not __subst _ = not_

    (* compo_op *)

    method build_RComp _ f1 __op f2 f1' op' f2' =
      op' f1 f2 f1' f2'
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug  *)
    (*         (fun m -> m "build_RComp [[%a %a %a]] --> %a" *)
    (*                     Elo.pp_exp f1 *)
    (*                     G.pp_comp_op op *)
    (*                     Elo.pp_exp f2 *)
    (*                     Ltl.pp res) *)
    (*     ) *)


    (* method build_REq __subst r s r' s' = *)
    (*   self#build_In env r s r' s' +&& lazy (self#build_In env s r s' r') *)

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

    method build_In subst r __s r' s' =
      let { must; may; _} = env#must_may_sup subst r in
      (* Msg.debug (fun m -> m "build_In: %a in %a@\nmust(%a) = %a@\nmay(%a) = %a" *)
      (*                       Elo.pp_exp r *)
      (*                       Elo.pp_exp __s *)
      (*                       Elo.pp_exp r *)
      (*                       TS.pp must *)
      (*                       Elo.pp_exp r *)
      (*                       TS.pp may *)
      (*           ); *)
      wedge ~range:(TS.to_seq must) (fun t -> lazy (s' t))
      +&& lazy (wedge ~range:(TS.to_seq may)
                  (fun bs -> lazy (r' bs @=> lazy (s' bs))))

    method build_NotIn subst r s r' s' =
      not_ @@ self#build_In subst r s r' s'

    method build_RNEq subst r s r' s' =
      not_ @@ self#build_REq subst r s r' s'

    (* icomp_op *)

    method build_IComp __subst __e1 _ __e2 e1_r op e2_r = op e1_r e2_r

    method build_Gt __subst = comp gt

    method build_Gte __subst = comp gte

    method build_IEq __subst = comp eq

    method build_INEq __subst = comp neq

    method build_Lt __subst = comp lt

    method build_Lte __subst = comp lte

    (* rqualify *)

    method build_Qual __subst __q __r __q' __r' = assert false (* SIMPLIFIED *)

    method build_RLone __subst = G.rlone (* SIMPLIFIED *)

    method build_RNo __subst = assert false (* SIMPLIFIED *)

    method build_ROne __subst = assert false (* SIMPLIFIED *)

    method build_RSome __subst = assert false (* SIMPLIFIED *)

    (************************** exp  ********************************)

    method build_exp __subst _ pe' _ _ = pe'


    (* re-defining this method to avoid going down in the block as a
       substitution must be made first *)
    method! visit_Compr env _visitors_c0 _visitors_c1 =
      let _visitors_r0 =
        self#visit_list self#visit_sim_binding env _visitors_c0
      in
      let _visitors_r1 = [true_]  in
      self#build_Compr env _visitors_c0 _visitors_c1 _visitors_r0 _visitors_r1

    method private allocate_sbs_to_tuples
                     (vars : (Var.t * (Elo.var, Elo.ident) G.exp) list)
                     (l : Atom.t list) =
      match vars with
      | [] -> []
      | (_, r)::tl ->
         let xs, ys = List.take_drop Option.(get_exn r.G.arity) l in (* FIXME: get_exn may return None *)
         Tuple.of_list1 xs :: self#allocate_sbs_to_tuples tl ys

    (* shape: [{ sb1, sb2,... | b }]. Each [sb] is of shape [disj x1, x2 : e] .

       The first item implies that we have to fold over the [sb]'s to substitute
       previously-bound variables. In the following function, we perform these
       substitutions and then compute separately the semantics of every binding,
       before computing the whole resulting formula.
    *)
    method build_Compr subst sbs b _ _ = fun tuple ->
      (* assert (List.for_all (fun (disj, _, _) -> not disj) sbs); *)
      (* compute the subtitution of elements in [tuple] for all bound variables *)
      let ranging_vars = 
        List.(flat_map (fun (_, vs, r) ->
              List.map (fun (Elo.BVar v) -> (v, r)) vs) sbs) in
      let split_tuples =
        self#allocate_sbs_to_tuples ranging_vars @@ Tuple.to_list tuple in
      (* let split =  List.map Fun.(G.ident % Elo.tuple_ident) split_tuples in *)
      let all_vars =
        List.flat_map (fun (_, vs, _) ->
              List.map (fun (Elo.BVar v) -> v) vs) sbs in
      let sub = List.combine all_vars split_tuples @ subst in
      (* Msg.debug (fun m -> *)
      (*       m "build_Compr: tuple = %a split = %a sub = %a " *)
      (*         Tuple.pp tuple *)
      (*         Fmtc.(brackets *)
      (*               @@ list ~sep:sp *)
      (*               @@ Elo.pp_prim_exp) split *)
      (*         Fmtc.(list @@ brackets  *)
      (*               @@ pair ~sep:(const string " <- ") Var.pp *)
      (*               @@ Elo.pp_prim_exp) sub *)
      (*     ); *)
      (* semantics of [b] is [[ b [atoms / variables] ]] *)
      let b' = self#visit_prim_fml sub @@ G.block b
      in
      (* get a tuple out of a list of variables in sub *)
      let sub_tuples = List.combine all_vars split_tuples in
      let get_tuple vs =
        vs
        |> List.flat_map (fun (Elo.BVar v) ->
              Tuple.to_list @@
              List.Assoc.get_exn ~eq:Var.equal v sub_tuples) 
        |> Tuple.of_list1
      in
      (* we bluntly substitute in ranges also, as bound-variable names are
         unique!, instead of folding more finely over the list of bindings *)
      let ranges =
        List.map (fun (_, vs, e) ->
              self#visit_exp sub e
              @@ get_tuple vs) sbs
      in
      conj (b' :: ranges)

    method build_Iden __subst = fun tuple -> 
      assert (Tuple.arity tuple = 2);
      if Atom.equal (Tuple.ith 0 tuple) (Tuple.ith 1 tuple) then
        true_
      else
        false_



    method build_BoxJoin __subst __call __args __call' __args' = (* SIMPLIFIED *)
      assert false

    method build_Ident subst _ id = fun tuple ->
      (match id with
        | Elo.Var v ->
            if Tuple.equal (CCList.Assoc.get_exn ~eq:Var.equal v subst) tuple
            then
              true_
            else
              false_
        | Elo.Name r ->
            let { must; may; _ } =
              env#must_may_sup subst @@
              G.exp (Some (env#relation_arity r)) Location.dummy @@ G.ident id
            in
            (* Msg.debug (fun m -> *)
            (*       m "build_Ident: must/may(%a) = %a / %a | tuple = %a" *)
            (*         Name.pp r *)
            (*         TS.pp must *)
            (*         TS.pp may *)
            (*         Tuple.pp tuple); *)
            if TS.mem tuple must then
              true_
            else if TS.mem tuple may then 
              env#make_atom r tuple
            else
              false_)
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug (fun m -> *)
    (*             m "build_Ident [[%a]](%a) (with subst %a) --> %a" *)
    (*               Elo.pp_ident id *)
    (*               Tuple.pp tuple *)
    (*               Fmtc.(List.pp @@ brackets @@ Pair.pp ~sep:":=" Var.pp Tuple.pp) subst *)
    (*               Ltl.pp res *)
    (*           )) *)

    method build_None_ __subst = fun _ -> false_

    method build_Univ __subst = fun _ -> true_

    method build_Prime __subst _ e' = fun tuple -> next @@ e' tuple

    method build_RIte __subst _ _ _ f_r e1_r e2_r = fun tuple -> 
      (f_r @=> lazy (e1_r tuple)) +&& lazy ((not_ f_r) @=> lazy (e2_r tuple))


    (* rbinop *)

    method build_RBin __subst f1 __op f2 f1' op' f2' =
      op' f1 f2 f1' f2'

    method build_Union __subst _ _ e1 e2 =
      (fun x -> e1 x +|| lazy (e2 x))

    method build_Inter __subst _ _ e1 e2 = fun x -> e1 x +&& lazy (e2 x)

    method build_Join subst r s r' s' =  fun tuple ->
      (* let open List in *)
      (* Msg.debug (fun m -> *)
      (*       m "build_Join <-- [[%a . %a]](%a) " *)
      (*         Elo.pp_exp r *)
      (*         Elo.pp_exp s *)
      (*         Tuple.pp tuple); *)
      let sup_r = (env#must_may_sup subst r).sup in
      let sup_s = (env#must_may_sup subst s).sup in
      let pairs = eligible_pairs (tuple, sup_r, sup_s) in
      vee ~range:pairs (fun (bs, cs) -> lazy (r' bs +&& lazy (s' cs)))
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug (fun m -> *)
    (*             m "build_Join [[%a . %a]](%a) --> %a" *)
    (*               Elo.pp_exp r *)
    (*               Elo.pp_exp s *)
    (*               Tuple.pp tuple *)
    (*               Ltl.pp res *)
    (*           )) *)



    method build_LProj __subst _ _ s' r' = fun tuple -> 
      (s' @@ Tuple.(of_list1 [ith 0 tuple])) +&& lazy (r' tuple)


    method build_RProj __subst _ _ r' s' = fun tuple -> 
      let lg = Tuple.arity tuple in
      (s' @@ Tuple.of_list1 [Tuple.ith (lg - 1) tuple]) +&& lazy (r' tuple)

    method build_Prod __subst r s r' s' = fun tuple ->
      Msg.debug (fun m ->
          m "build_Prod <-- [[%a × %a]](%a)"
            Elo.pp_exp r
            Elo.pp_exp s
            Tuple.pp tuple
        );
      (* we need to split [tuple] so we need the arity of [r]. If the
      arity is [None] (for 'none'), then we must just return
      false. Otherwise the tuple is split. *)
      match r.G.arity, s.G.arity with
      | None, _
        | _, None -> false_
      | Some ar_r, Some _ ->
         let t1, t2 = Tuple.split tuple ar_r in
         r' t1 +&& lazy (s' t2)
         
                       
    (* |> Fun.tap (fun res -> *)
    (*       Msg.debug *)
    (*         (fun m -> m "build_Prod [[%a->%a]](%a) (split as %a, %a) = %a (ar(%a) = %d)" *)
    (*                     Elo.pp_exp r *)
    (*                     Elo.pp_exp __s *)
    (*                     Tuple.pp tuple *)
    (*                     Tuple.pp t1 *)
    (*                     Tuple.pp t2 *)
    (*                     Ltl.pp res *)
    (*                     Elo.pp_exp r *)
    (*                     ar_r *)
    (*         )) *)

    method build_Diff __subst _ _ e' f' = fun tuple ->
      e' tuple +&& lazy (not_ (f' tuple))

    (* [[r++ s]](a) = [[s]](a) or ([[r]](a) and not [[pi_1(s)]](a_1)) 

       which for the last part gives:

       AND_{t in must(s)} [ if a1 = pi_1(t) then false_ else true_]

       and AND_{t in may(s)} not [ [[s]](a_1 t_2 ...)]
    *)
    method build_Over subst __r s r' s' = fun tuple ->
      Msg.debug (fun m ->
            m "build_Over <-- [[%a ++ %a]](%a)"
              Elo.pp_exp __r
              Elo.pp_exp s
              Tuple.pp tuple
          );
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
      |> Fun.tap
           (fun res ->
              Msg.debug
                (fun m ->
                   m "build_Over [[%a ++ %a]](%a) = %a"
                     Elo.pp_exp __r
                     Elo.pp_exp s
                     Tuple.pp tuple
                     Ltl.pp res
                ))


    (* runop *)

    method build_RUn __subst _ e op e_r = op e e_r

    method build_RTClos subst r _ = fun tuple ->
      self#build_Iden subst tuple +|| lazy (self#visit_RUn subst G.TClos r tuple)

    method build_Transpose __subst _ r' = fun tuple -> 
      r' @@ Tuple.transpose tuple

    method build_TClos subst r __r' =
      Msg.debug
        (fun m -> m "Elo_to_LTL1.build_TClos <-- %a"
                    Elo.pp_exp r);
      let { sup ; _ } = env#must_may_sup subst r in
      (* Msg.debug *)
      (*   (fun m -> m "Elo_to_LTL1.build_TClos: sup(%a) = %a" *)
      (*               Elo.pp_exp r *)
      (*               TS.pp sup); *)
      let k = compute_tc_length sup in
      (* let tc_naif = iter_tc r k in *)
      let tc_square = iter_squares r k in
      (* let suptc =  *)
      (*   (env#must_may_sup (G.exp Location.dummy @@ G.runary G.tclos r)).sup *)
      (* in *)
      (* let suptc2 = *)
      (*   (env#must_may_sup tc_square).sup *)
      (* in *)
      Msg.debug (fun m ->
            m "TC bound: %d" k);
      (* Msg.debug (fun m -> *)
      (*     m "terme de TC naif : (%a)" (Elo.pp_exp) (tc_naif)); *)
      Msg.debug (fun m ->
            m "TC term using iterative squares: %a" (Elo.pp_exp) (tc_square));
      (* Msg.debug (fun m -> *)
      (*     m "sup(%a) = %a" (Elo.pp_prim_exp) (G.runary G.tclos r) *)
      (*       TS.pp suptc); *)
      (* Msg.debug (fun m -> *)
      (*     m "sup(%a) = %a" (Elo.pp_exp) (tc_square) *)
      (*       TS.pp suptc2); *)

      self#visit_exp subst tc_square

    (*********************************** iexp **************************************)

    method build_iexp __subst _ iexp' _ = iexp'

    method build_IBin __subst _ _ _ i1' op' i2' = op' i1' i2'

    method build_IUn __subst _ _ op' i' = op' i'

    method build_Num __subst _ = num

    method build_Add __subst = plus

    method build_Neg __subst = neg

    method build_Sub __subst = minus

    method build_Card subst r r' =
      let { must; may; _ } = env#must_may_sup subst r in
      let must_card = num @@ TS.size must in
      let may_card =
        count @@ List.map r' @@ TS.to_list may
      in
      plus must_card may_card


  end                           (* end converter *)


  class environment (elo : Elo.t) = object (_ : 'self)
    method relation_arity name =
      match Domain.get name elo.Elo.domain with
        | None -> assert false
        | Some rel -> Relation.arity rel

    method must_may_sup (subst : (Var.t, Tuple.t) CCList.Assoc.t)
             (exp : (Elo.var, Elo.ident) G.exp) =
      bounds subst elo.Elo.domain exp

    method make_atom (name : Name.t) (t : Tuple.t) =
      assert (Domain.mem name elo.Elo.domain);
      Ltl.atomic @@ Atomic.make elo.Elo.domain name t 

    method is_const (name : Name.t) =
      assert (Domain.mem name elo.Elo.domain);
      Domain.get_exn name elo.Elo.domain |> Relation.is_const
  end

  (* Computes the color (Invar, Static_prop, Init or Temporal) of an
     elo formula *)                                     
  let color elo elo_fml =
    let open Elo in
    let open Invar_computation in
    let env = new environment elo in
    let color = (new invarComputation)#visit_fml env elo_fml in
    color



  let formula_as_comment fml =
    let str = Fmt.to_to_string Elo.pp_fml fml in
    "-- " ^ String.replace ~which:`All ~sub:"\n" ~by:"\n-- " str

  (* Converts an Elo formula to an LTL formula, gathering at the same time the
     rigid and flexible variables having appeared during the walk. *)
  let convert elo elo_fml =
    let open Elo in
    let env = new environment elo in    
    let ltl_fml = (new converter env)#visit_fml [] elo_fml in
    (formula_as_comment elo_fml, ltl_fml)


end

