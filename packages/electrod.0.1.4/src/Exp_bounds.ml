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

open Containers

module G = GenGoal
module TS = TupleSet

type bounds = {
  must : TS.t;
  sup : TS.t;
  may : TS.t;
}

let rec bounds subst domain exp =
  let { must; sup; _ } = bounds_exp subst domain exp in
  (if not (TS.subset must sup) then
     Msg.err (fun m ->
         m
           "Exp_bounds.bounds@ %a@ with@\nsubst= %a@\n%a@\nmust(%a)=@ \
            %a@\nsup(%a)=@ %a@."
           Elo.pp_exp exp
           Fmtc.(brackets @@ list @@ parens @@ pair Var.pp Tuple.pp) subst
           Domain.pp domain
           Elo.pp_exp exp
           TS.pp must
           Elo.pp_exp exp
           TS.pp sup)
  );
  { must; sup; may = TS.diff sup must }  

(* Helper function.  NOTICE: [may] is voluntarily wrong because its
   computation can be costly while the interesting [may] is that of the
   toplevel expression for which it's called, so it's fixed in [bounds]
   above *)
and make_bounds must sup =
  { must; sup; may = TS.empty }           


(* TODO? 20171010: Considering the possibly large number of walks to perform on
   expressions, we could cache the results for *ground* expressions as their
   bounds are known. One way to do so would be to equip expressions with a field
   telling if the term is ground and its bound is already known. Left for future
   optimization if it's needed in practice (which may not be the case
   considering that real models will enjoy symmetries that may make the actual
   number of walks not so large...). *)
and bounds_exp subst domain exp =
  bounds_prim_exp subst domain exp.G.prim_exp 

and bounds_prim_exp subst domain pe =
  let open G in 
  match pe with         
  | BoxJoin (_,_) -> assert false (* SIMPLIFIED *)
  | Ident (Elo.Var v) -> 
     let singleton =
       TS.singleton @@ CCList.Assoc.get_exn ~eq:Var.equal v subst
     in
     make_bounds singleton singleton
     |> Fun.tap
          (fun _ ->
            Msg.debug (fun m ->
                m "bounds_prim_exp(%a) =[exact] %a"
                  Var.pp v
                  TS.pp singleton))
  | Ident (Elo.Name n) -> 
     let rel = Domain.get_exn n domain in
     make_bounds (Relation.must rel) (Relation.sup rel)
  | None_ ->
     make_bounds TS.empty TS.empty
  | Univ ->
     let univ = Domain.univ_atoms domain in
     make_bounds univ univ
  | Iden ->
     let iden = Domain.get_exn Name.iden domain in
     make_bounds (Relation.must iden) (Relation.sup iden)
  | RUn (Transpose, e) ->
     let b = bounds_exp subst domain e in
     make_bounds (TS.transpose b.must) (TS.transpose b.sup)
  | RUn (TClos, e) -> 
     let b = bounds_exp subst domain e in
     make_bounds (TS.transitive_closure b.must) (TS.transitive_closure b.sup)
     |> Fun.tap
          (fun res ->
            Msg.debug (fun m ->
                m
                  "bounds_prim_exp(TClos):@\n\
                   must(%a) = %a@\nmay(%a) = %a"
                  Elo.pp_prim_exp pe
                  TS.pp res.must
                  Elo.pp_prim_exp pe
                  TS.pp res.may))
  | RUn (RTClos, e) -> 
     let iden = Domain.get_exn Name.iden domain in
     let b = bounds_exp subst domain e in
     make_bounds (TS.union (TS.transitive_closure b.must) @@ Relation.must iden)
                 (TS.union (TS.transitive_closure b.sup) @@ Relation.sup iden)
     |> Fun.tap
          (fun res ->
            Msg.debug (fun m ->
                m
                  "bounds_prim_exp(TClos):@\n\
                   must(%a) = %a@\nmay(%a) = %a"
                  Elo.pp_prim_exp pe
                  TS.pp res.must
                  Elo.pp_prim_exp pe
                  TS.pp res.may))

  | RBin (e1, Union ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.union b1.must b2.must) (TS.union b1.sup b2.sup)
  | RBin (e1, Inter ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.inter b1.must b2.must) (TS.inter b1.sup b2.sup)
  | RBin (e1, Over ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.override b1.must b2.must) (TS.override b1.sup b2.sup)
  | RBin (e1, LProj ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.lproj b1.must b2.must) (TS.lproj b1.sup b2.sup)
  | RBin (e1, RProj ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.rproj b1.must b2.must) (TS.rproj b1.sup b2.sup)
  | RBin (e1, Prod ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.product b1.must b2.must) (TS.product b1.sup b2.sup)
  | RBin (e1, Diff ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     (* compute the tuples that are necessarily in e1 and necessarily not in e2 *)
     let must_diff = TS.filter (fun tup -> not (TS.mem tup b2.sup)) b1.must in
     make_bounds must_diff (TS.diff b1.sup b2.must) (* b2.MUST! *)
  | RBin (e1, Join ,e2) -> 
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.join b1.must b2.must) (TS.join b1.sup b2.sup)
  | RIte (_, e1, e2) ->
     let b1 = bounds_exp subst domain e1 in
     let b2 = bounds_exp subst domain e2 in
     make_bounds (TS.inter b1.must b2.must) (TS.union b1.sup b2.sup) 
  | Prime e ->
     bounds_exp subst domain e
  | Compr (sim_bindings, _) ->
     (* The must of a compr set is empty. Indeed, it would be wrong
       to compute the must from the sim_bindings only, because the
       formula could be in contradiction with this must. Taking the
       formula into account is not possible, an we consider an empty
       must. *)
     let pmust = TS.empty in  
     let psup =
       TS.of_tuples
       @@ bounds_sim_bindings (fun { sup; _} -> sup) domain [] sim_bindings
     in
     Msg.debug (fun m ->
         m
           "bounds_exp(Compr):\
            @\nmust(%a) = @[<hov>%a@]\
            @\nsup(%a) = @[<hov>%a@]"
           (Fmtc.(list ~sep:(const string ": "))
            @@ Elo.pp_sim_binding)
           sim_bindings
           TS.pp pmust
           (Fmtc.(list ~sep:(const string ": "))
            @@ Elo.pp_sim_binding)
           sim_bindings
           TS.pp psup);
     make_bounds pmust psup


(* Computes the bounds for a sim_binding, in the case of a set defined by
   comprehension.

   The whole idea of this function (and the following auxiliary ones) is to
   apply the following scheme.  Say we want to compute: 

   must({x : s, y : x.r | ...})

   The result must be:

   must(s) -> { UNION_(tuple in must(s)) must( x.r [tuple/x] ) }

   as every pair in the comprehension is s.t. the range 
   for y depends on the value for x.

   E.g.: Taking must(s) = { a b }, must(r) = { (a b) (b c) (a c) (a d) }:

   x : s, y : x.r

   should yield:

   (a x a.r) union (b x b.r) = {(a b) (a c) (a d) (b c)}

   So what we do is the following: Walk left-to-right along some sim_bindings
   and compute the corresponding bound, while accumulating already computed
   bounds. Finally everything is composed.  

   [subst] is a substitution from already-visited sim_bindings. [fbound] is the
   function returning the bound (say the must or the sup).  *)
and bounds_sim_bindings
      fbound
      domain
      (subst : (Var.t, Tuple.t) CCList.Assoc.t)
      (sbs : (Elo.var, Elo.ident) G.sim_binding list)
    : Tuple.t list =  
  let open! List in
  match sbs with
  | [] -> assert false      (* nonempty list *)
  | [(disj, vs, r)] -> 
     (* compute the product of bounds for the head sim_binding, it yields a
           list of combinations (= lists) of tuples *)
     bounds_sim_binding fbound domain subst disj vs r
     (* as we're done, we concat tuples in every combination *)
     >|= Tuple.concat
  | (disj, vs, r)::tl ->
     (* compute the product of bounds for the head sim_binding, it yields a
           list of combinations (= lists) of tuples *)
     let hd_bnd =
       bounds_sim_binding fbound domain subst disj vs r
     in
     (* cast to create a substitution below *)
     let vars = map (fun (Elo.BVar v) -> v) vs in
     (* as explained above in the example: for every possible combination
           of tuples in [hd_bnd], we have to substitute *this* combination in
           the tail, then compute the resulting product and add it to the
           whole possibilities *)
     fold_left
       (fun acc combination ->
         acc
         @ (* we use @ instead of union because it should be faster and
                  the end result will be converted back into a set anyway *)
           product_for_one_hd_combination
             fbound domain subst vars tl combination)
       []                  (* empty accumulator *)
       hd_bnd

(* given the list of tuples corresponding to the bound for the head sim
   binding (so it's a list of tuples, as there are multiple variables bound
   to a given range), compute all the products with all the results obtained
   by substituting in the tail according to the substitutions induced by the
   said head tuples *)
and product_for_one_hd_combination fbound domain subst vars
                                   (tail : (Elo.var, Elo.ident) G.sim_binding list)
                                   (hd_combination : Tuple.t list)
    : Tuple.t list =
  let open! List in
  (* compute the corresponding substitution for this instance of the head bound *)
  let subst2 = combine vars hd_combination @ subst in
  (* compute all the tuples for the tail with *this* subtitution *)
  let tl_bnd =
    bounds_sim_bindings fbound domain subst2 tail
  in
  (* create all combinations with this instance of the head bound and the
     tuples computed for the tail *)
  product Tuple.(@@@) [Tuple.concat hd_combination] tl_bnd

(* for one sim_binding *)
and bounds_sim_binding 
      fbound
      domain
      (subst : (Var.t, Tuple.t) CCList.Assoc.t) 
      disj
      vars
      (dom : (Elo.var, Elo.ident) G.exp)
    : Tuple.t list list
  =
  let open List in
  (* compute the bound for the range of *this* sim binding. NOTE: [range_bnd]
     is given as a [Tuple.t list] instead of a [TS.t] for technical purposes
     only. As a consequence, the returned value for the whole function is a
     set of sets of tuples represented by a list of lists... *)
  let range_bnd =
    TS.to_list @@ fbound @@ bounds_exp subst domain dom
  in
  (* compute the bound for the whole head sim binding  *)
  let lg = length vars in
  let prod =
    (* create as many copies as necessary (= nb of variables) of the domain *)
    init lg (fun _ -> range_bnd)
    (* & compute product (NOTE: tuples are not concatenated, just put in the
       same list representing a combination of tuples).  

       E.g.: suppose `range_bnd = [(1, 2); (3, 4)]`. 
       Then we obtain, for `lg = 3`:

       [
       [(3, 4); (3, 4); (3, 4)]; 
       [(3, 4); (3, 4); (1, 2)]; 
       [(3, 4); (1, 2); (3, 4)];
       [(3, 4); (1, 2); (1, 2)]; 
       [(1, 2); (3, 4); (3, 4)]; 
       [(1, 2); (3, 4); (1, 2)];
       [(1, 2); (1, 2); (3, 4)]; 
       [(1, 2); (1, 2); (1, 2)]
       ] *)
    |> cartesian_product 
  in
  (* Remove lines where there are tuples in common if [disj = true].
     [all_different] is defined below. *)
  (if disj then filter all_different prod else prod)
(* |> Fun.tap *)
(* @@ fun res -> *)
(* Msg.debug *)
(*   (fun m -> *)
(*      m "bounds_sim_binding %B %a %a --> @[<hov>%a@]" *)
(*        disj *)
(*        Fmtc.(list ~sep:(const string ", ")@@ Elo.pp_var) vars *)
(*        Fmtc.(list ~sep:sp @@ Tuple.pp) range_bnd *)
(*        Fmtc.(brackets @@ list ~sep:sp @@ brackets @@ list ~sep:sp @@ Tuple.pp) res) *)


(* says whether all tuples in a list are diffferent form one another *)
and all_different (tuples : Tuple.t list) : bool = match tuples with
  | [] -> true
  | hd::tl when List.mem ~eq:Tuple.equal hd tl -> false
  | _::tl -> all_different tl
