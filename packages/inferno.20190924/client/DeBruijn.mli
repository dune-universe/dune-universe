(* This module defines facilities for working with syntax, in nominal and
   de Bruijn representations. *)

type index =
  int

(* -------------------------------------------------------------------------- *)

(* The following module offers a facility for translating names to de Bruijn
   indices. It is parameterized over the type of names, which must come with
   a comparison function, so as to allow an efficient implementation of maps. *)

module Nominal2deBruijn (N : Map.OrderedType) : sig

  (* A type of translation environments. *)

  type env

  (* The empty environment. *)

  val empty: env

  (* [lookup env x] translates the name [x] to a de Bruijn index. The
     environment [env] must have been previously extended with [x],
     otherwise [Unbound x] is raised. *)

  exception Unbound of N.t

  val lookup: env -> N.t -> index

  (* [extend env x] extends the environment with a new binding of the
     name [x], producing a new environment. Any previous bindings of
     this name are shadowed. *)

  val extend: env -> N.t -> env

  (* [slide env x] extends the environment with a new binding of the
     name [x], producing a new environment. Any previous bindings of
     this name are shadowed. In contrast with [extend env x], instead
     of mapping [x] to a distinct index, [slide env x] maps [x] to the
     same index as the last introduced name. This exotic function can
     be useful, e.g., when mapping term variables to type indices. *)

  val slide: env -> N.t -> env

  (* [bump env] skips an index, i.e., it shifts the range of the environment
     [env] up by one. This exotic function can be useful, e.g., when mapping
     term variables to type indices.  Note that [extend env x] is equivalent
     to [slide (bump env) x]. *)

  val bump: env -> env

end

(* -------------------------------------------------------------------------- *)

(* This signature defines the functions that we need in order to be able
   to implement binding representations on top of some syntax. *)

(* The signatures that follow mention two types [v] and [t]. Think of them as
   ``values'' and ``terms'', where the syntax of terms supports substitution
   of values for variables. They can be the same type, of course. They are
   parameterized over a representation of variables ['var] and a
   representation of binders ['binder]. *)

module type VAR = sig

  type ('var, 'binder) v

  (* The function [var] expects a variable [x] and injects it into the
     syntax of values. *)

  val var:
    'var ->
    ('var, 'binder) v

end

module type TRAVERSE = sig

  type ('var, 'binder) v
  type ('var, 'binder) t

  (* [traverse lookup extend env t] can be thought of, roughly, as the result of
     applying the substitution [lookup env] to the term [t] in a
     capture-avoiding manner. The function [lookup], when supplied with a
     suitable environment, maps variables to values. The function [extend]
     updates the environment when a binder is crossed; it returns a pair of
     the updated environment and a new binder. The parameter [env] is the
     initial environment. *)

  val traverse:
    ('env -> 'var1 -> ('var2, 'binder2) v) ->
    ('env -> 'binder1 -> 'env * 'binder2) ->
    'env -> ('var1, 'binder1) t -> ('var2, 'binder2) t

end

(* -------------------------------------------------------------------------- *)

(* A renaming function, [map], is defined in terms of [var] and [traverse].
   It is analogous to [traverse], except that its argument [lookup] has type
   ['env -> 'var1 -> 'var2]. *)

module MakeMap
  (V : VAR)
  (T : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v)
: sig

  open T

  val map :
    ('env -> 'var1 -> 'var2) ->
    ('env -> 'binder1 -> 'env * 'binder2) ->
    'env -> ('var1, 'binder1) t -> ('var2, 'binder2) t

end

(* -------------------------------------------------------------------------- *)

(* Translation of a nominal representation to a de Bruijn representation. *)

module MakeTranslate
  (V : VAR)
  (T : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v)
  (N : Map.OrderedType)
: sig

  val translate: (N.t, N.t) T.t -> (index, unit) T.t

end

(* -------------------------------------------------------------------------- *)

(* Weakening in a de Bruijn representation; also known as lifting. *)

module MakeLift
  (V : VAR)
  (T : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v)
: sig

  (* [lift_var w k x] adds [w] to the index [x] if it is [k]-free, that is, if
     [x] is at or above [k]. *)

  val lift_var: int -> index -> index -> index

  (* [lift w k t] adds [w] to the [k]-free indices of the term [t]. *)

  val lift: int -> index -> (index, unit) T.t -> (index, unit) T.t

end

(* -------------------------------------------------------------------------- *)

(* Substitution in a de Bruijn representation. *)

module MakeSubst
  (V : VAR)
  (TVV : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v and type ('v, 'b) t = ('v, 'b) V.v)
  (TVT : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v)
: sig

  open TVT

  (* [subst_var v k x] replaces the variable [k] with the value [v], leaves
     the variables below [k] unchanged, and decrements the variables above [k]
     by one. *)

  val subst_var:
    (index, unit) v ->
    index ->
    index ->
    (index, unit) v

  (* [subst v k t] replaces the variable [k] with the value [v] in the term [t]. *)

  val subst:
    (index, unit) v ->
    index ->
    (index, unit) t ->
    (index, unit) t

end

