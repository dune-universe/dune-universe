(******************************************************************************)
(*                                                                            *)
(*                                  Inferno                                   *)
(*                                                                            *)
(*                       François Pottier, Inria Paris                        *)
(*                                                                            *)
(*  Copyright Inria. All rights reserved. This file is distributed under the  *)
(*  terms of the MIT License, as described in the file LICENSE.               *)
(*                                                                            *)
(******************************************************************************)

open UnifierSig

module Make (S : STRUCTURE) (U : UNIFIER with type 'a structure = 'a S.structure) = struct

(* -------------------------------------------------------------------------- *)

(* The [Generalization] module manages the [rank] fields of the unification
   variables, as well as a global notion of ``current rank'', stored in the
   field [state.young]. Ranks can be thought of as de Bruijn levels, in the
   following sense: whenever the left-hand side of a [CLet] constraint is
   entered, the current rank is incremented by one. Thus, the rank of a
   variable indicates where (i.e., at which [CLet] construct) this variable is
   (existentially) bound. *)

(* The rank of a variable is set to the current rank when the variable is
   first created. During the lifetime of a variable, its rank can only
   decrease. Decreasing a variable's rank amounts to hoisting out the
   existential quantifier that binds this variable. *)

(* Ranks are updated in a lazy manner. Only one rank maintenance operation
   takes place during unification: when two variables are unified, the rank of
   the merged variable is set to the minimum of the ranks of the two
   variables. (This operation is performed by the unifier.) Two other rank
   maintenance operations are performed here, namely downward propagation and
   upward propagation. Downward propagation updates a child's rank, based on
   its father rank; there is no need for a child's rank to exceed its father's
   rank. Upward propagation updates a father's rank, based the ranks of all of
   its children: there is no need for a father's rank to exceed the maximum of
   its children's ranks. These operations are performed at generalization time
   because it would be costly (and it is unnecessary) to perform them during
   unification. *)

(* The [rank] field maps every variable to the [CLet] construct where it is
   bound. Conversely, the [Generalization] module keeps track, for every
   active [CLet] construct, of a (complete) list of variables that are bound
   there. This takes the form of an array, stored in the field [state.pool].
   For every rank comprised between 1 and [state.young], both included, this
   array stores a list of the variables that are bound there. This array is
   again updated in a lazy manner, at generalization time. Because the unifier
   updates the ranks, but does not know about this array, the property that
   holds in general is: if a variable [v] has rank [i], then it appears in
   pool number [j], where [i <= j] holds. Immediately after generalization has
   been performed, the array has been updated, so [i = j] holds. *)

type state = {
  (* An array of pools (lists of variables), indexed by ranks. *)
  pool: U.variable list InfiniteArray.t;
  (* The current rank. *)
  mutable young: int;
}

(* -------------------------------------------------------------------------- *)

(* The [Generalization] module is in charge of constructing and instantiating
   type schemes, or graph fragments that contain universally quantified (i.e.,
   to-be-copied) variables as well as free (i.e., not-to-be-copied) variables.
   This happens when we exit the left-hand side of a [CLet] constraint, i.e.,
   when we move from a context of the form [let x v = <hole> in c] to a
   context of the form [let x = scheme in <hole>]. At this moment, the current
   rank [state.young] is decremented by one, and all variables whose rank was
   precisely [state.young] become universally quantified, or generic. These
   variables are no longer stored in any pool, as they are no longer
   existentially quantified. Their rank is set to the constant [generic].
   This allows [instantiate] to recognize them easily. *)

(* The generic variables that have no structure are the ``quantifiers'' of the
   type scheme. A type scheme is internally represented as a pair of a list of
   quantifiers and a root variable, the ``body''. The order of the quantifiers
   is arbitrarily chosen, but once fixed, matters; the functions [quantifiers]
   and [instantiation] must agree on this order. The quantifiers are exactly
   the variables that are reachable from the body, have rank [generic], and
   have no structure. *)

(* Technical note (mostly to myself). The representation of type schemes is
   not stable, in the following sense. When a scheme is first created, its
   universal quantifiers versus free variables are recognized by their rank
   (rank [generic], versus a positive rank). This remains valid as long as
   this type scheme is in scope, i.e., as long as the current rank does not
   decrease below its current value. However, the current rank ultimately
   decreases (all the way down to zero), and at this final point, all
   variables have rank [generic], so the type schemes that were previously
   created are no longer useable. We could fix this problem (if desired) by
   not relying on the rank within [instantiate]; we would instead rely on a
   list of all generic variables (with or without structure). Note: the
   field [quantifiers] is a list of the generic variables *without* structure. *)

(* TEMPORARY consider doing this? maybe benchmark.
   If we DON'T do it, then we should document the fact that [instantiate]
   won't work any more after the current rank decreases below its current
   value. (Not really a problem, as the end-end-user is not supposed to
   have access to this function.) *)

type scheme = {
  (* A list of quantifiers. *)
  quantifiers: U.variable list;
  (* A distinguished variable forms the body of the type scheme. *)
  body: U.variable
}

(* -------------------------------------------------------------------------- *)

(* The constant [generic] is defined as [-1]. This rank is used for the variables
   that form the generic (to-be-copied) part of a type scheme. *)

let generic =
  -1

(* The rank [no_rank] is defined as [0]. This rank is used when a variable is
   freshly created and is not known to us yet. *)

let no_rank =
  0

(* The positive ranks are valid indices into the pool array. *)

let base_rank =
  no_rank + 1

(* -------------------------------------------------------------------------- *)

(* The initial state. *)

(* The pool array is initially populated with empty pools. The current rank is
   initially set to 0, so the first rank that is actually exploited is 1. *)

let init () = {
  pool = InfiniteArray.make 8 [];
  young = no_rank;
}

(* -------------------------------------------------------------------------- *)

(* Accessors for type schemes. *)

let quantifiers { quantifiers; _ } =
  quantifiers

let body { body; _ } =
  body

(* -------------------------------------------------------------------------- *)

(* A trivial constructor of type schemes. *)

let trivial body =
  { quantifiers = []; body }

(* -------------------------------------------------------------------------- *)

(* The internal function [register_at_rank] assumes that [v]'s rank is already
   a valid positive rank, and registers [v] by inserting it into the appropriate
   pool. *)

let register_at_rank ({ pool; _ } as state) v =
  let rank = U.rank v in
  assert (0 < rank && rank <= state.young);
  InfiniteArray.set pool rank (v :: InfiniteArray.get pool rank)

(* The external function [register] assumes that [v]'s rank is uninitialized.
   It sets this rank to the current rank, [state.young], then registers [v]. *)

let register state v =
  assert (U.rank v = no_rank);
  U.set_rank v state.young;
  register_at_rank state v

(* -------------------------------------------------------------------------- *)

(* Debugging utilities. *)

let show_variable v =
  Printf.printf "id = %d, rank = %d\n" (U.id v) (U.rank v)

let show_pool state k =
  Printf.printf "Pool %d:\n" k;
  List.iter show_variable (InfiniteArray.get state.pool k)

let show_young state =
  Printf.printf "state.young = %d\n" state.young

let show_pools state =
  for k = base_rank to state.young do
    show_pool state k
  done

let show_state label state =
  Printf.printf "%s:\n" label;
  show_young state;
  show_pools state

(* -------------------------------------------------------------------------- *)

(* [enter] simply increments the current rank by one. The corresponding pool is
   in principle already empty. *)

let enter state =
  state.young <- state.young + 1;
  assert (InfiniteArray.get state.pool state.young = [])

(* -------------------------------------------------------------------------- *)

(* The internal function [make_scheme] turns a variable, [body], into a type
   scheme. The body of the type scheme is [body]. The quantifiers of the type
   scheme are exactly the generic structureless variables that are reachable,
   in the unification graph, from [body]. The function [is_generic] determines
   which variables are generic. *)

(* The order in which the quantifiers appear is determined in an arbitrary
   manner. *)

let make_scheme (is_generic : U.variable -> bool) (body : U.variable) : scheme =

  (* Prepare to mark which variables have been visited. *)
  let visited : unit U.VarMap.t = U.VarMap.create 128 in

  let rec traverse v quantifiers =

    (* If this variable is not generic or has been discovered already, then
       we must stop. *)

    if not (is_generic v) || U.VarMap.mem visited v then
      quantifiers
    else begin

      (* Mark this variable as visited. If it carries no structure, then it is
         a leaf in the generic part of this type scheme, that is, a
         quantifier: add it to the list of quantifiers. Otherwise, traverse
         its descendants. Note that the variable must be marked before the
         recursive call, so as to guarantee termination in the presence of
         cyclic terms. *)

      U.VarMap.add visited v ();
      match U.structure v with
      | None ->
          v :: quantifiers
      | Some t ->
          S.fold traverse t quantifiers

    end

  in
  (* Discover which quantifiers are accessible from [body]. *)
  let quantifiers = traverse body [] in
  (* Build a type scheme. *)
  { quantifiers; body }

(* -------------------------------------------------------------------------- *)

(* [exit] is where the moderately subtle generalization work takes place. *)

let exit rectypes state roots =

  (* Get the list [vs] of all variables in the young generation. *)
  let vs = InfiniteArray.get state.pool state.young in

  (* This hash table stores all of these variables, so that we may check
     membership in the young generation in constant time. *)
  let young_generation : unit U.VarMap.t = U.VarMap.create 128 in

  (* This array stores all of these variables, indexed by rank. The use
     of a bucket sort is theoretically costly if the [CLet]-nesting depth
     is not considered a constant, because of the need to walk through
     possibly-empty buckets; in that case, a standard sort algorithm, or
     (even better) no sort at all would suffice. (Sorting helps us compute
     better ranks; but distinguishing between [young] and non-[young] would
     be enough.) In practice, the [CLet]-nesting depth should remain low,
     and walking through empty buckets (in the loop that follows) should
     cost almost nothing. So we adopt this approach, even though it violates
     the complexity claim of the paper. *)
  let sorted : U.variable list array = Array.make (state.young + 1) [] in

  (* Initialize these data structures. *)
  List.iter (fun v ->
    U.VarMap.add young_generation v ();
    let rank = U.rank v in
    assert (0 < rank && rank <= state.young);
    sorted.(rank) <- v :: sorted.(rank)
  ) vs;

  (* Define a membership test for the young generation. *)
  let is_young v =
    U.VarMap.mem young_generation v
  in

  (* If the client would like us to detect and rule out recursive types, then
     now is the time to perform an occurs check over the young generation. *)
  if not rectypes then
    List.iter (U.new_occurs_check is_young) vs;

  (* Now, update the rank of every variable in the young generation. Downward
     propagation and upward propagation, as described above, are performed. A
     single depth-first traversal of the young generation achieves
     both. Roughly speaking, downward propagation is achieved on the way down,
     while upward propagation is achieved on the way up. (In reality, all rank
     updates takes place during the upward phase.)

     It may be worth noting that downward propagation is required, as (for
     instance) [instantiate] assumes that a non-generic variable cannot have
     generic children. Upward propagation is an optional optimization; without
     it, we would perform slightly more copying, but that would be harmless.

     During each traversal, every visited variable is marked as such, so as to
     avoid being visited again. To ensure that visiting every variable once is
     enough, the roots must be processed by increasing order of rank. In the
     absence of cycles, this enforces the following invariant: when performing
     a traversal whose starting point has rank [k], every variable marked as
     visited has rank [k] or less already. (In the presence of cycles, this
     algorithm is incomplete and may compute ranks that are slightly higher
     than necessary.) Conversely, every non-visited variable must have rank
     greater than or equal to [k]. This explains why [k] remains constant as
     we go down (i.e., discovering [v] does not improve the value of [k] that
     we are pushing down). *)

  let visited : unit U.VarMap.t = U.VarMap.create 128 in

  for k = base_rank to state.young do

    (* A postcondition of [traverse v] is [U.rank v <= k]. (This is downward
       propagation.) *)
    let rec traverse v =
      assert (U.rank v > 0);
      (* If [v] was visited before, then its rank must be below [k], as we
         adjust ranks on the way down already. *)
      if U.VarMap.mem visited v then
        assert (U.rank v <= k)
      else begin
        (* Otherwise, immediately mark it as visited, and immediately adjust
           its rank so as to be at most [k]. (This is important if cyclic
           graphs are allowed.) *)
        U.VarMap.add visited v ();
        U.adjust_rank v k;
        (* If [v] is part of the young generation, and if it has structure,
           then traverse its children (updating their ranks) and on the way
           back up, adjust [v]'s rank again (this is upward propagation). If
           [v] has structure but no children, then it is a constant, and it
           receives the base rank; it will be moved to the oldest pool. If
           [v] has no structure, do nothing; it would be wrong to move its
           rank down to the base rank. *)
        if is_young v then begin
          (* The rank of this variable can't have been below [k], because
             it is young but was not visited yet. Thus, it must have been
             at or above [k], and since we have just adjusted it, it must
             now be [k]. *)
          assert (U.rank v = k);
          Option.iter (fun t ->
            U.adjust_rank v (
              S.fold (fun child accu ->
                traverse child;
                max (U.rank child) accu
              ) t base_rank (* the base rank is neutral for [max] *)
            )
          ) (U.structure v)
        end
        (* If [v] is old, stop. *)
        else
          assert (U.rank v < state.young)
      end

    in
    List.iter traverse sorted.(k)

  done;

  (* The rank of every variable in the young generation has now been
     determined as precisely as possible.

     Every variable that has become an alias for some other (old or young)
     variable is dropped. We keep only one representative of each class.

     Every variable whose rank has become strictly less than [young] may be
     safely turned into an old variable. It is moved into the pool that
     corresponds to its rank.

     Every variable whose rank is still [young] must be generalized. That is,
     it becomes universally quantified in the type scheme that is being
     created. We set its rank to [generic]. By convention, a variable of rank
     [generic] is considered universally quantified. *)

  let vs =
    List.filter (fun v ->
      U.is_representative v && begin
        if U.rank v < state.young then begin
          register_at_rank state v;
          false
        end
        else begin
          assert (U.rank v = state.young);
          U.set_rank v generic;
          U.structure v = None
        end
      end
    ) vs
  in

  (* Update the state by emptying the current pool and decreasing [young]. *)
  InfiniteArray.set state.pool state.young [];
  state.young <- state.young - 1;

  (* This auxiliary function recognizes the variables that we have just
     marked as generic. *)
  let is_generic v =
    U.rank v = generic
  in

  (* The generic variables are now unreachable from the variables that still
     have positive rank and inhabit one of the pools. *)
  assert (
    (* For every [v] in the young generation, *)
    U.VarMap.fold (fun v () ok ->
      ok && (
        (* If [v] is not generic, *)
        is_generic v ||
        match U.structure v with
        | None ->
            true
        | Some t ->
            (* then its child [w] is not generic. *)
            S.fold (fun w ok -> ok && not (is_generic w)) t true
      )
    ) young_generation true
  );

  (* Return the list of unique generalizable variables that was constructed
     above, and a list of type schemes, obtained from the list [roots]. *)
  vs,
  List.map (make_scheme is_generic) roots

(* -------------------------------------------------------------------------- *)

(* Instantiation amounts to copying a fragment of a graph. The fragment that
   must be copied is determined by inspecting the rank -- [generic] means
   copy, a positive rank means don't copy. *)

let instantiate state { quantifiers; body } =

  (* Prepare to mark which variables have been visited and record their copy. *)
  let visited : U.variable U.VarMap.t = U.VarMap.create 128 in

  (* If the variable [v] has rank [generic], then [copy v] returns a copy of
     it, and copies its descendants recursively. If [v] has positive rank,
     then [copy v] returns [v]. Only one copy per variable is created, even if
     a variable is encountered several times during the traversal. *)

  let rec copy v =

    (* If this variable has positive rank, then it is not generic: we must
       stop. *)

    if U.rank v > 0 then
      v

    (* If a copy of this variable has been created already, return it. *)

    else begin
      assert (U.rank v = generic);

      try
        U.VarMap.find visited v
      with Not_found ->

        (* The variable must be copied, and has not been copied yet. Create a
           new variable, register it, and update the mapping. Then, copy its
           descendants. Note that the mapping must be updated before making a
           recursive call to [copy], so as to guarantee termination in the
           presence of cyclic terms. *)

        let v' = U.fresh None state.young in
        register_at_rank state v';
        U.VarMap.add visited v v';
        U.set_structure v' (Option.map (S.map copy) (U.structure v));
        v'

    end

  in
  List.map copy quantifiers, copy body

end
