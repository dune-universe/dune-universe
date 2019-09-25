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
open SolverSig

(* -------------------------------------------------------------------------- *)

(* The solver is parameterized over the nature of term variables, the type
   structure, and the client's representation of types. *)
(* BEGIN HI *)
module Make
  (X : TEVAR)
  (S : STRUCTURE)
  (O : OUTPUT with type 'a structure = 'a S.structure)
: sig

  (* The type [tevar] of term variables is provided by [X]. *)
  open X

  (* The type ['a structure] of shallow types is provided by [S]
     (and repeated by [O]). *)

  (* The types [tyvar] and [ty] of decoded type variables and decoded types
     are provided by the module [O]. *)
  open O

  (* ---------------------------------------------------------------------- *)

  (* The type [variable] describes the solver's type variables. *)
  type variable

  (* The type ['a co] is the type of a constraint, which, once solved, produces
     a result of type ['a]. Put another way, it is the type of a computation
     which, once run to completion, produces a result of type ['a]. *)

  type 'a co

  (* ---------------------------------------------------------------------- *)

  (* The applicative functor interface: tautology, conjunction, post-processing. *)

  (* The following three operations form the applicative functor interface.
     [pure] represents the true constraint, while [^&] builds a conjunction
     of constraints. [map] does not per se produce any constraint; it only
     performs postprocessing, i.e., it applies a user-supplied function to
     the value that is obtained after constraint solving. *)
  val pure: 'a -> 'a co
  val (^&): 'a co -> 'b co -> ('a * 'b) co
  val map: ('a -> 'b) -> 'a co -> 'b co

  (* ---------------------------------------------------------------------- *)
  (* END HI *)

  (* Variants of the above. *)

  (* [<$$>] is just [map] with reversed argument order. *)
  val (<$$>): 'a co -> ('a -> 'b) -> 'b co

  (* The function [^^], a variation of [^&], also builds a conjunction
     constraint, but drops the first component of the resulting pair, and keeps
     only the second component. [f ^^ g] is equivalent to [f ^& g <$$> snd]. *)
  val (^^): 'a co -> 'b co -> 'b co

  (* BEGIN HI *)
  (* ---------------------------------------------------------------------- *)

  (* Equations. *)

  (* [v -- w] is an equality constraint on the type variables [v] and [w]. *)
  val (--): variable -> variable -> unit co

  (* [v --- t] is analogous, but its right-hand side is a shallow type, as
     opposed to a type variable. In fact, [---] is just [lift (--)]. *)
  val (---): variable -> variable structure -> unit co

  (* ---------------------------------------------------------------------- *)

  (* Existential quantification. *)

  (* Assume that the user-supplied function [c], applied to a fresh type
     variable [v], produces a constraint of type ['a]. Then, [exist c] wraps it
     in an existential quantifier for [v]. This results in a constraint of type
     [ty * 'a], where the left-hand component of the pair, a decoded type, is
     the witness for [v]. *)
  val exist:                            (variable -> 'a co) -> (ty * 'a) co

  (* END HI *)
  (* [construct t c] is analogous to [exist c], but additionally constrains
     the type variable [v] to be equal to the type [t]. So, it is really a
     way of constructing a variable that stands for a shallow term. *)
  val construct:  variable structure -> (variable -> 'a co) -> (ty * 'a) co

  (* ---------------------------------------------------------------------- *)

  (* Variants of the above. *)

  (* The following variants are used when one does not need access to the
     witness. [exist_ body] is logically equivalent to [exist body <$$> snd]. *)
  val exist_:                           (variable -> 'a co) -> 'a co
  val construct_: variable structure -> (variable -> 'a co) -> 'a co

  (* ---------------------------------------------------------------------- *)

  (* A utility. *)

  (* If [f] is a binary predicate whose second argument is a type variable,
     then [lift f] is a binary predicate whose second argument is a shallow
     type. It is defined in terms of [construct_] and [f]. *)
  val lift: ('a -> variable -> 'b co) -> 'a -> variable structure -> 'b co

  (* BEGIN HI *)
  (* ---------------------------------------------------------------------- *)

  (* Application of constraint abstractions, a.k.a. instantiation. *)

  (* The constraint [instance x v] requires [v] to be an instance of the type
     scheme denoted by the term variable [x]. Or, in other words, it is an
     application of the constraint abstraction denoted by [x] to the variable
     [v]. *)
  val instance: tevar -> variable -> ty list co

  (* ---------------------------------------------------------------------- *)

  (* Construction of constraint abstractions, a.k.a. generalization. *)

  (* [def x v c] binds the term variable [x] to the trivial (monomorphic) type
     scheme [v] in the constraint [c]. *)
  val def: tevar -> variable -> 'a co -> 'a co

  (* [let1 x c1 c2] binds the term variable [x] to the constraint abstraction
     [c1] in the constraint [c2]. (Technically, [c1] is a function of a fresh
     type variable to a constraint, as in [exist].) The resulting constraint
     produces a tuple of 4 results, namely:
     - the (decoded) type scheme that was assigned to [x] while solving [c2].
     - a list [vs] of (decoded) type variables that may appear in [a1], and (hence)
       should be universally bound by the client in [a1]. In a typical scenario,
       [a1] is a System F term, and the client will build the type abstraction
       [\Lambda vs.a1].
     - the value [a1] produced by the constraint [c1].
     - the value [a2] produced by the constraint [c2]. *)
  val let1: tevar -> (variable -> 'a co) -> 'b co ->
            (scheme * tyvar list * 'a * 'b) co

  (* END HI *)
  (* [let0 c] has the same meaning as [c], but, like [let1], produces a list [vs]
     of the type variables that may appear in the result of [c]. The argument of
     [run] should always be an application of [let0] -- see below. *)
  val let0: 'a co  -> (tyvar list * 'a) co

  (* [letn xs c1 c2] binds [n] term variables [xs] to [n] constraint abstractions
     in the constaint [c2]. Here, [c1] is a function of [n] fresh type variables
     [vs] to a constraint. The [i]-th term variable, [x_i], ends up bound to the
     constraint abstraction of the [i]-th type variable in [c_1], which one could
     write [\lambda v_i.c_1]. *)
  val letn: tevar list -> (variable list -> 'a co) -> 'b co ->
            (scheme list * tyvar list * 'a * 'b) co

  (* BEGIN HI *)
  (* ---------------------------------------------------------------------- *)

  (* Evaluation. *)

  (* [solve rectypes c] evaluates the constraint [c]. The flag [rectypes] tells
     whether recursive types are considered legal. The constraint [c] is solved.
     If a term variable [x] is out of scope, [Unbound x] is raised. If the
     constraint is unsatisfiable, then [Unify] or [Cycle] is raised. ([Cycle]
     can be raised only if [rectypes] is [false].) After the constraint has been
     found to be satisfiable, it is evaluated (in particular, the functions that
     were supplied as arguments to [map] during the construction of the
     constraint are invoked) and a final result of type ['a] is produced. *)

  (* [solve] destroys its argument. It is not permitted to call [solve] twice
     on the same constraint. *)

  (* The argument to [solve] must have an application of [let0] at its root. This
     is a way of ensuring that the final result has no free type variables. *)

  exception Unbound of tevar
  exception Unify of ty * ty
  exception Cycle of ty
  val solve: bool -> 'a co -> 'a

(* END *)
end
(* END HI *)
