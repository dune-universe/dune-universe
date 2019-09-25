(* This is the target calculus of the sample client. It is a core System F. *)

(* -------------------------------------------------------------------------- *)

(* Types. *)

(* We include recursive types [FTyMu] in the target calculus, not only because
   we might wish to support them, but also because even if we disallow them,
   they can still arise during unification (the occurs check is run late), so
   we must be able to display them as part of a type error message. *)

(* Nominal or de Bruijn representation of type variables and binders. The
   nominal representation is more convenient when translating from ML to F,
   while the de Bruijn representation is more convenient when type-checking
   F. *)

(* BEGIN F *)
type ('a, 'b) typ =
  | TyVar of 'a
  | TyArrow of ('a, 'b) typ * ('a, 'b) typ
  | TyProduct of ('a, 'b) typ * ('a, 'b) typ
  | TyForall of 'b * ('a, 'b) typ
  | TyMu of 'b * ('a, 'b) typ
(* END F *)

(* BEGIN F *)
type tyvar = int
(* END F *)

(* BEGIN F *)
type nominal_type = (tyvar, tyvar) typ
(* END F *)

type debruijn_type =
    (DeBruijn.index, unit) typ

(* -------------------------------------------------------------------------- *)

(* Terms. *)

(* Nominal representation of term variables and binders. *)

(* BEGIN F *)
type tevar = string
(* END F*)

(* BEGIN F *)
type ('a, 'b) term =
  | Var of tevar
  | Abs of tevar * ('a, 'b) typ * ('a, 'b) term
  | App of ('a, 'b) term * ('a, 'b) term
  | Let of tevar * ('a, 'b) term * ('a, 'b) term
  | TyAbs of 'b * ('a, 'b) term
  | TyApp of ('a, 'b) term * ('a, 'b) typ
(* END F *)
  | Pair of ('a, 'b) term * ('a, 'b) term
  | Proj of int * ('a, 'b) term

(* BEGIN F *)
type nominal_term = (tyvar, tyvar) term
(* END F *)

type debruijn_term =
    (DeBruijn.index, unit) term

(* -------------------------------------------------------------------------- *)

(* Constructors. *)
(* BEGIN F *)

let ftyabs vs t =
  List.fold_right (fun v t -> TyAbs (v, t)) vs t
let ftyapp t tys =
  List.fold_left (fun t ty -> TyApp (t, ty)) t tys
(* END F *)

(* -------------------------------------------------------------------------- *)

(* Boilerplate code for type-in-type substitutions. *)

module TypeVar : DeBruijn.VAR
  with type ('a, 'b) v = ('a, 'b) typ
= struct

  type ('v, 'b) v =
      ('v, 'b) typ

  let var x =
    TyVar x

end

module TypeInType : DeBruijn.TRAVERSE
  with type ('v, 'b) v = ('v, 'b) typ
   and type ('v, 'b) t = ('v, 'b) typ
= struct

  type ('v, 'b) v =
      ('v, 'b) typ

  type ('v, 'b) t =
      ('v, 'b) typ

  (* I note that we could perform physical equality tests in [traverse]
     so as to preserve sharing when possible, but that would require use
     of [Obj.magic]. Let's just not do it in this demo. *)

  let rec traverse lookup extend env ty =
    match ty with
    | TyVar x ->
        lookup env x
    | TyArrow (ty1, ty2) ->
        let ty1' = traverse lookup extend env ty1
        and ty2' = traverse lookup extend env ty2 in
        TyArrow (ty1', ty2')
    | TyProduct (ty1, ty2) ->
        let ty1' = traverse lookup extend env ty1
        and ty2' = traverse lookup extend env ty2 in
        TyProduct (ty1', ty2')
    | TyForall (x, ty1) ->
        let env, x = extend env x in
        let ty1' = traverse lookup extend env ty1 in
        TyForall (x, ty1')
    | TyMu (x, ty1) ->
        let env, x = extend env x in
        let ty1' = traverse lookup extend env ty1 in
        TyMu (x, ty1')

end

(* -------------------------------------------------------------------------- *)

(* Boilerplate code for type-in-term substitutions. *)

module TypeInTerm : DeBruijn.TRAVERSE
  with type ('v, 'b) v = ('v, 'b) typ
  with type ('v, 'b) t = ('v, 'b) term
= struct

  type ('v, 'b) v =
      ('v, 'b) typ

  type ('v, 'b) t =
      ('v, 'b) term

  let rec traverse lookup extend env t =
    match t with
    | Var x ->
        Var x
    | Abs (x, ty, t) ->
        let ty' = TypeInType.traverse lookup extend env ty
        and t' = traverse lookup extend env t in
        Abs (x, ty', t')
    | App (t1, t2) ->
        let t1' = traverse lookup extend env t1
        and t2' = traverse lookup extend env t2 in
        App (t1', t2')
    | Let (x, t1, t2) ->
        let t1' = traverse lookup extend env t1
        and t2' = traverse lookup extend env t2 in
        Let (x, t1', t2')
    | TyAbs (x, t) ->
        let env, x = extend env x in
        let t' = traverse lookup extend env t in
        TyAbs (x, t')
    | TyApp (t, ty) ->
        let t' = traverse lookup extend env t
        and ty' = TypeInType.traverse lookup extend env ty in
        TyApp (t', ty')
    | Pair (t1, t2) ->
        let t1' = traverse lookup extend env t1
        and t2' = traverse lookup extend env t2 in
        Pair (t1', t2')      
    | Proj (i, t) ->
        let t' = traverse lookup extend env t in
        Proj (i, t')

end

(* -------------------------------------------------------------------------- *)

(* Type-in-type weakening and substitution. *)

include DeBruijn.MakeLift(TypeVar)(TypeInType)

include DeBruijn.MakeSubst(TypeVar)(TypeInType)(TypeInType)

(* -------------------------------------------------------------------------- *)

(* Translation of nominal terms to de Bruijn terms. *)

include DeBruijn.MakeTranslate(TypeVar)(TypeInTerm)(Int)

