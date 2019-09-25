(* This module defines facilities for working with syntax, in nominal and
   de Bruijn representations. *)

type index =
  int

(* -------------------------------------------------------------------------- *)

(* The following module offers a facility for translating names to de Bruijn
   indices. It is parameterized over the type of names, which must come with
   a comparison function, so as to allow an efficient implementation of maps. *)

module Nominal2deBruijn (N : Map.OrderedType) = struct

  module M =
    Map.Make(N)

  (* For efficiency reasons, we must work internally with de Bruijn levels,
     as opposed to de Bruijn indices, and perform the conversion to an index
     at the last moment. *)
  
  type env = {
    (* A map of every name to a de Bruijn level. *)
    map:     int M.t;
    (* The current de Bruijn level. It is equal to the level of the most
       recently bound name. Its absolute value is irrelevant. *)
    current: int;
  }

  (* The empty environment. *)

  let empty =
    { map = M.empty; current = 0 }

  (* [lookup env x] translates the name [x] to a de Bruijn index. The
     environment [env] must have been previously extended with [x],
     otherwise [Unbound x] is raised. *)

  exception Unbound of N.t

  let lookup { map; current } x =
    (* Looking up the map produces a de Bruijn level, which we
       convert to a de Bruijn index by subtracting it from the
       current level. *)
    try
      current - M.find x map
    with Not_found ->
      raise (Unbound x)

  (* [extend env x] extends the environment with a new binding of the
     name [x], producing a new environment. Any previous bindings of
     this name are shadowed. *)

  let extend { map; current } x =
    let current = current + 1 in
    let map = M.add x current map in
    { map; current }

  (* [slide env x] extends the environment with a new binding of the
     name [x], producing a new environment. Any previous bindings of
     this name are shadowed. In contrast with [extend env x], instead
     of mapping [x] to a distinct index, [slide env x] maps [x] to the
     same index as the last introduced name. This exotic function can
     be useful, e.g., when mapping term variables to type indices. *)

  let slide { map; current } x =
    (* Do not increment [current]. Thus, [x] receives the de Bruijn
       index 0, as in [extend], but the indices of the previous
       variables are not shifted up. *)
    let map = M.add x current map in
    { map; current }

  (* [bump env] skips an index, i.e., it shifts the range of the environment
     [env] up by one. This exotic function can be useful, e.g., when mapping
     term variables to type indices.  Note that [extend env x] is equivalent
     to [slide (bump env) x]. *)

  let bump { map; current } =
    let current = current + 1 in
    { map; current }

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
= struct

  let map lookup extend env t =
    T.traverse (fun env x -> V.var (lookup env x)) extend env t

end

(* -------------------------------------------------------------------------- *)

(* Translation of a nominal representation to a de Bruijn representation. *)

module MakeTranslate
  (V : VAR)
  (T : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v)
  (N : Map.OrderedType)
= struct

  include MakeMap(V)(T)

  include Nominal2deBruijn(N)
  
  let translate t =
    map
      lookup
      (fun env x -> extend env x, ())
      empty
      t

end

(* -------------------------------------------------------------------------- *)

(* Weakening in a de Bruijn representation; also known as lifting. *)

module MakeLift
  (V : VAR)
  (T : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v)
= struct

  include MakeMap(V)(T)

  let lift_var w k x =
    if k <= x then w + x else x

  let cross k () =
    k + 1, ()

  let lift w k t =
    if w = 0 then
      (* Fast path. *)
      t
    else
      map (lift_var w) cross k t

end

(* -------------------------------------------------------------------------- *)

(* Substitution in a de Bruijn representation. *)

module MakeSubst
  (V : VAR)
  (TVV : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v and type ('v, 'b) t = ('v, 'b) V.v)
  (TVT : TRAVERSE with type ('v, 'b) v = ('v, 'b) V.v)
= struct

  include MakeLift(V)(TVV)

  let subst_var v k x =
    if x < k then
      V.var x
    else if x = k then
      v
    else
      V.var (x - 1)

  let subst v k t =
    TVT.traverse (fun l x ->
      subst_var (lift l 0 v) (l + k) x
    ) cross 0 t

end

