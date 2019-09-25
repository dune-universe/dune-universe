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

type ('a, 'b) typ =
  | TyVar of 'a
  | TyArrow of ('a, 'b) typ * ('a, 'b) typ
  | TyProduct of ('a, 'b) typ * ('a, 'b) typ
  | TyForall of 'b * ('a, 'b) typ
  | TyMu of 'b * ('a, 'b) typ

type tyvar =
    int

type nominal_type =
    (tyvar, tyvar) typ

type debruijn_type =
    (DeBruijn.index, unit) typ

(* -------------------------------------------------------------------------- *)

(* Terms. *)

(* Nominal representation of term variables and binders. *)

(* Nominal or de Bruijn representation of type variables and binders. *)

type tevar =
    string

type ('a, 'b) term =
  | Var of tevar
  | Abs of tevar * ('a, 'b) typ * ('a, 'b) term
  | App of ('a, 'b) term * ('a, 'b) term
  | Let of tevar * ('a, 'b) term * ('a, 'b) term
  | TyAbs of 'b * ('a, 'b) term
  | TyApp of ('a, 'b) term * ('a, 'b) typ
  | Pair of ('a, 'b) term * ('a, 'b) term
  | Proj of int * ('a, 'b) term

type nominal_term =
    (tyvar, tyvar) term

type debruijn_term =
    (DeBruijn.index, unit) term

(* -------------------------------------------------------------------------- *)

(* Constructors. *)

val ftyabs: 'b list -> ('a, 'b) term -> ('a, 'b) term
val ftyapp: ('a, 'b) term -> ('a, 'b) typ list -> ('a, 'b) term

(* -------------------------------------------------------------------------- *)

(* Type-in-type weakening and substitution. *)

(* [lift w k ty] is the type [ty] where the variables at or above index [k]
   are lifted up by [w]. *)

val lift: int -> DeBruijn.index -> debruijn_type -> debruijn_type

(* [subst xty x ty] is the type [ty] where the type variable [x] has been
   replaced with the type [xty]. *)

val subst: debruijn_type -> DeBruijn.index -> debruijn_type -> debruijn_type

(* -------------------------------------------------------------------------- *)

(* Translation of nominal terms to de Bruijn terms. *)

val translate: nominal_term -> debruijn_term

