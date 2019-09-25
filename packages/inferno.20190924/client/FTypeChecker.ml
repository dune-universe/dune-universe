open F

(* -------------------------------------------------------------------------- *)

(* A type environment maps term variables to types in de Bruijn's representation. *)

(* Our term variables are in nominal representation, which allows us to represent
   the environment as a binary search tree. One difficulty is that when we enter a
   new type variable binder [TyAbs], we must (conceptually) shift every type in
   the range of the environment up by one. When term variables are in de Bruijn
   notation and the environment is a list, this is easily done by inserting a mark
   in front of the environment. Here, we must be more clever. We maintain an [N2DB]
   environment that tells us, for every *term* variable, how long ago it was bound,
   that is, how many *type* variables were introduced since then. Hence, this tells
   us by how much we must shift its type. It's really a bit too clever, but let's
   do this for fun. *)

module TermVar =
  String

module TermVarMap =
  Map.Make(TermVar)

module N2DB =
  DeBruijn.Nominal2deBruijn(TermVar)

type env = {
  (* A mapping of term variables to types. *)
  types: debruijn_type TermVarMap.t;
  (* A translation environment of TERM variables to TYPE indices. *)
  names: N2DB.env
}

let empty =
  { types = TermVarMap.empty; names = N2DB.empty }

exception UnboundTermVariable of tevar

let lookup { types; names } x =
  try
    (* Obtain the type associated with [x]. *)
    let ty = TermVarMap.find x types in
    (* Find how long ago [x] was bound. *)
    let w = N2DB.lookup names x in
    (* Shift the type [ty] by this amount, so that it makes sense in the
       current scope. *)
    lift w 0 ty
  with Not_found ->
    (* must have been raised by [TermVarMap.find] *)
    raise (UnboundTermVariable x)

let extend_with_tevar { types; names } x ty =
  (* Map the name [x] to [ty], and record when it was bound, without
     incrementing the time. *)
  { types = TermVarMap.add x ty types;
    names = N2DB.slide names x }

let extend_with_tyvar { types; names } =
  (* Increment the time. *)
  { types; names = N2DB.bump names }

(* -------------------------------------------------------------------------- *)

(* Destructors. *)

let unfold ty =
  match ty with
  | TyMu ((), body) ->
      subst ty 0 body
  | _ ->
      assert false

exception NotAnArrow of debruijn_type

let rec as_arrow ty =
  match ty with
  | TyArrow (ty1, ty2) ->
      ty1, ty2
  | TyMu _ ->
      as_arrow (unfold ty)
  | _ ->
      raise (NotAnArrow ty)

exception NotAProduct of debruijn_type

let rec as_product ty =
  match ty with
  | TyProduct (ty1, ty2) ->
      ty1, ty2
  | TyMu _ ->
      as_product (unfold ty)
  | _ ->
      raise (NotAProduct ty)

exception NotAForall of debruijn_type

let rec as_forall ty =
  match ty with
  | TyForall ((), ty) ->
      ty
  | TyMu _ ->
      as_forall (unfold ty)
  | _ ->
      raise (NotAForall ty)

(* -------------------------------------------------------------------------- *)

(* An equality test. *)

(* In the absence of recursive types, syntactic equality would work. However,
   in their presence, one must unfold recursive types during the equality
   check. *)

(* Note that comparing two [TyMu] types just by comparing their bodies is
   correct, but incomplete. We do not do it. Naively unfolding every [TyMu]
   type is complete, but leads to potential non-termination. We do that, with
   a budget limit, which makes the algorithm unsound (two types may be
   considered equal when they are different). I should really implement
   my own algorithm (Gauthier & Pottier, ICFP 2004) but that will be for
   some other time. *)

exception TypeMismatch of debruijn_type * debruijn_type

let rec equal budget ty1 ty2 =
  match ty1, ty2 with
  | (TyMu _ as ty1), ty2
  | ty2, (TyMu _ as ty1) ->
      budget = 0 || equal (budget - 1) (unfold ty1) ty2
  | TyVar x1, TyVar x2 ->
      x1 = x2
  | TyArrow (ty1a, ty1b), TyArrow (ty2a, ty2b) ->
      equal budget ty1a ty2a && equal budget ty1b ty2b
  | TyProduct (ty1a, ty1b), TyProduct (ty2a, ty2b) ->
      equal budget ty1a ty2a && equal budget ty1b ty2b
  | TyForall ((), ty1), TyForall ((), ty2) ->
      equal budget ty1 ty2
  | _, _ ->
      false

let budget =
  4

let (--) ty1 ty2 =
  if not (equal budget ty1 ty2) then
    raise (TypeMismatch (ty1, ty2))

(* -------------------------------------------------------------------------- *)

(* The type-checker. *)

let rec typeof env (t : debruijn_term) : debruijn_type =
  match t with
  | Var x ->
      lookup env x
  | Abs (x, ty1, t) ->
      let ty2 = typeof (extend_with_tevar env x ty1) t in
      TyArrow (ty1, ty2)
  | App (t, u) ->
      let ty1, ty2 = as_arrow (typeof env t) in
      typeof env u -- ty1;
      ty2
  | Let (x, t, u) ->
      let env = extend_with_tevar env x (typeof env t) in
      typeof env u
  | TyAbs ((), t) ->
      TyForall ((), typeof (extend_with_tyvar env) t)
  | TyApp (t, ty2) ->
      subst ty2 0 (as_forall (typeof env t))
  | Pair (t1, t2) ->
      TyProduct (typeof env t1, typeof env t2)
  | Proj (i, t) ->
      assert (i = 1 || i = 2);
      let ty1, ty2 = as_product (typeof env t) in
      if i = 1 then ty1 else ty2

let typeof =
  typeof empty
