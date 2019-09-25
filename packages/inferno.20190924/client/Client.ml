(* This sample client performs type inference for a fragment of ML and translates
   it down to a fragment of System F. *)

(* -------------------------------------------------------------------------- *)

(* The unifier will use the following type structure. *)

module S = struct

  type 'a structure =
    | TyArrow of 'a * 'a
    | TyProduct of 'a * 'a

  let map f t =
    match t with
    | TyArrow (t1, t2) ->
        let t1 = f t1 in
        let t2 = f t2 in
        TyArrow (t1, t2)
    | TyProduct (t1, t2) ->
        let t1 = f t1 in
        let t2 = f t2 in
        TyProduct (t1, t2)

  let fold f t accu =
    match t with
    | TyArrow (t1, t2)
    | TyProduct (t1, t2) ->
        let accu = f t1 accu in
        let accu = f t2 accu in
        accu

  let iter f t =
    let _ = map f t in
    ()

  exception Iter2

  let iter2 f t u =
    match t, u with
    | TyArrow (t1, t2), TyArrow (u1, u2)
    | TyProduct (t1, t2), TyProduct (u1, u2) ->
        f t1 u1;
        f t2 u2
    | _, _ ->
        raise Iter2

end

(* -------------------------------------------------------------------------- *)

(* The unifier type structure is decoded into the target calculus type structure
   as follows. *)

module O = struct

  type tyvar =
    int

  type 'a structure =
    'a S.structure

  type ty =
    F.nominal_type

  let variable x =
    F.TyVar x

  let structure t =
    match t with
    | S.TyArrow (t1, t2) ->
        F.TyArrow (t1, t2)
    | S.TyProduct (t1, t2) ->
        F.TyProduct (t1, t2)

  let mu x t =
    F.TyMu (x, t)

  type scheme =
    tyvar list * ty

end

(* -------------------------------------------------------------------------- *)

(* Instantiate the solver. *)

module Solver =
  Inferno.SolverHi.Make(struct include String type tevar = t end)(S)(O)

open Solver

(* -------------------------------------------------------------------------- *)

let arrow x y =
  S.TyArrow (x, y)

let product x y =
  S.TyProduct (x, y)

let product_i i x y =
  assert (i = 1 || i = 2);
  if i = 1 then
    product x y
  else
    product y x

(* Should we use smart constructors to eliminate redundant coercions when possible? *)
let smart =
  true

let flet (x, t, u) =
  match t with
  | F.Var y when smart && x = y ->
      u
  | t ->
      F.Let (x, t, u)

(* -------------------------------------------------------------------------- *)

(* The coercion [coerce vs1 vs2] converts a type of the form [forall vs1, _]
   to a type of the form [forall vs2, _], where [vs2] forms a subset of [vs1].
   This coercion allows getting rid of unused quantifiers and/or re-ordering
   quantifiers. *)

type coercion =
  F.nominal_term -> F.nominal_term

let bottom : F.nominal_type =
  let a : F.tyvar = 0 (* arbitrary *) in
  F.TyForall (a, F.TyVar a)

(* [ftyabs1 v t] builds a (capital-Lambda) abstraction of the type variable [v]
   in the term [t]. It is a smart constructor: if it recognizes an eta-redex,
   it contracts it on the fly. We are in a special case where, if [v] and [w]
   are the same variable, then this variable does not occur free in [t], so we
   don't need to perform this costly check at runtime. This eta-contraction is
   not essential anyway; it's just a way of avoiding coercion clutter in the
   common case where the coercion actually has no effect. *)

let ftyabs1 v t =
  match t with
  | F.TyApp (t, F.TyVar w) when smart && v = w ->
      t
  | t ->
      F.TyAbs (v, t)

(* TEMPORARY find a better name for [coerce] *)

let coerce (vs1 : O.tyvar list) (vs2 : O.tyvar list) : coercion =
  (* Assume the term [t] has type [forall vs1, _]. *)
  fun t ->
    (* Introduce the desired quantifiers. *)
    List.fold_right ftyabs1 vs2 (
      (* Now, specialize the term [t]. For each member of [vs1],
         we must provide a suitable instantiation. *)
      F.ftyapp t (
        (* [vs1] is a superset of [vs2]. For each member of [vs1], if it is a
           member of [vs2], then we keep it (by instantiating it with itself),
           otherwise we get rid of it (by instantiating it with an arbitrary
           closed type, say [bottom]). *)
        let suitable (v : O.tyvar) : O.ty =
          if List.mem v vs2 then F.TyVar v else bottom
          (* TEMPORARY need an efficient membership test in [vs2] *)
        in
        List.map suitable vs1
      )
    )

(* -------------------------------------------------------------------------- *)

(* The client uses the combinators provided by the solver so as to transparently
   1- analyse the source term and produce constraints; and 2- decode the solution
   of the constraints and produce a term in the target calculus. These two steps
   take place in different phases, but the code is written as if there was just
   one phase. *)

(* The function [analyse] takes a source term [t] and an expected type [w].
   No type environment is required, as everything is built into the constraint via
   suitable combinators, such as [def]. *)

(* BEGIN HASTYPE *)
let rec hastype (t : ML.term) (w : variable) : F.nominal_term co
= match t with

    (* Variable. *)
  | ML.Var x ->

      (* [w] must be an instance of the type scheme associated with [x]. *)
      instance x w <$$> fun tys ->
      (* The translation makes the type application explicit. *)
      F.ftyapp (F.Var x) tys

    (* Abstraction. *)
  | ML.Abs (x, u) ->

      (* We do not know a priori what the domain and codomain of this function
         are, so we must infer them. We introduce two type variables to stand
         for these unknowns. *)
      exist (fun v1 ->
        (* Here, we could use [exist_], because we do not need [ty2]. I refrain
           from using it, just to simplify the paper. *)
        exist (fun v2 ->
          (* [w] must be the function type [v1 -> v2]. *)
          (* Here, we could use [^^], instead of [^&], so as to avoid building
             a useless pair. I refrain from using it, just to simplify the paper. *)
          w --- arrow v1 v2 ^&
          (* Under the assumption that [x] has type [domain], the term [u] must
             have type [codomain]. *)
          def x v1 (hastype u v2)
        )
      ) <$$> fun (ty1, (_ty2, ((), u'))) ->
      (* Once these constraints are solved, we obtain the translated function
         body [u']. There remains to construct an explicitly-typed abstraction
         in the target calculus. *)
      F.Abs (x, ty1, u')

    (* Application. *)
  | ML.App (t1, t2) ->

      (* Introduce a type variable to stand for the unknown argument type. *)
      exist (fun v ->
        lift hastype t1 (arrow v w) ^&
        hastype t2 v
      ) <$$> fun (_ty, (t1', t2')) ->
      F.App (t1', t2')

    (* Generalization. *)
  | ML.Let (x, t, u) ->

      (* Construct a ``let'' constraint. *)
      let1 x (hastype t)
        (hastype u w)
      <$$> fun ((b, _), a, t', u') ->
      (* [a] are the type variables that we must introduce (via Lambda-abstractions)
         while type-checking [t]. [(b, _)] is the type scheme that [x] must receive
         while type-checking [u]. Its quantifiers [b] are guaranteed to form a subset of
         [a]. Hence, in general, we must re-bind [x] to an application of a suitable
         coercion to [x]. We use smart constructors so that, if the lists [a] and
         [b] happen to be equal, no extra code is produced. *)
      F.Let (x, F.ftyabs a t',
(* IFPAPER
      F.Let (x, coerce a b (F.Var x),
      ELSE *)
      flet (x, coerce a b (F.Var x),
(* END *)
      u'))
(* END HASTYPE *)

    (* Pair. *)
  | ML.Pair (t1, t2) ->

      exist_ (fun v1 ->
        exist_ (fun v2 ->
          (* [w] must be the product type [v1 * v2]. *)
          w --- product v1 v2 ^^
          (* [t1] must have type [t1], and [t2] must have type [t2]. *)
          hastype t1 v1 ^&
          hastype t2 v2
        )
      ) <$$> fun (t1, t2) ->
      (* The System F term. *)
      F.Pair (t1, t2)

    (* Projection. *)
  | ML.Proj (i, t) ->

      exist_ (fun other ->
        lift hastype t (product_i i w other)
      ) <$$> fun t ->
      F.Proj (i, t)

(* The top-level wrapper uses [let0]. It does not require an expected
   type; it creates its own using [exist]. And it runs the solver. *)

exception Unbound = Solver.Unbound
exception Unify = Solver.Unify
exception Cycle = Solver.Cycle

let translate (t : ML.term) : F.nominal_term =
  solve false (
    let0 (exist_ (hastype t)) <$$> fun (vs, t) ->
    (* [vs] are the binders that we must introduce *)
    F.ftyabs vs t
  )
