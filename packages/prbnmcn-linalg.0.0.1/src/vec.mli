(** Linalg: vectors *)

(** [Make] takes as input the signature of an object language and
    generates a module to perform vector computations in this language. *)
module Make : functor
  (Repr : Basic_intf.Lang.Empty)
  (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
  (S : Basic_intf.Lang.Shape with type 'a m = 'a Repr.m)
  (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
  (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
  (R_storage : Basic_intf.Lang.Storage
                 with type 'a m = 'a Repr.m
                  and type elt = R.t)
  (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
  (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
  ->
  Intf.Vec
    with type 'a k = 'a Monad.t
     and type 'a m = 'a Repr.m
     and type 'a shape = 'a S.t
     and type ('a, 'b) morphism = ('a, 'b) S.Morphism.t
     and type elt = R.t

(** [Array_backed] yields a module that allows to create vectors backed by arrays. *)
module Array_backed : functor
  (Repr : Basic_intf.Lang.Empty)
  (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
  (S : Intf.Tensor with type 'a m = 'a Repr.m)
  (A : Basic_intf.Lang.Array with type index = S.pos and type 'a m = 'a Repr.m)
  -> sig
  val of_array :
    A.t Repr.m ->
    (S.pos S.t, S.pos Repr.m, A.elt Repr.m) Intf.vec
    * (S.pos S.t, S.pos Repr.m, A.elt Repr.m, unit Repr.m) Intf.ovec

  val in_of_array :
    A.t Repr.m -> (S.pos S.t, S.pos Repr.m, A.elt Repr.m) Intf.vec

  val out_of_array :
    A.t Repr.m -> (S.pos S.t, S.pos Repr.m, A.elt Repr.m, unit Repr.m) Intf.ovec
end

(** [Make_native] specializes [Make] to native execution. *)
module Make_native : functor
  (R : Basic_intf.Lang.Ring with type 'a m = 'a)
  (R_storage : Basic_intf.Lang.Storage with type 'a m = 'a and type elt = R.t)
  ->
  Intf.Vec
    with type 'a k = 'a
     and type 'a m = 'a
     and type 'a shape = 'a Tensor.Int.t
     and type ('a, 'b) morphism = ('a, 'b) Tensor.Int.Morphism.t
     and type elt = R.t

(** [Float] allows to manipulate float-valued vectors *)
module Float :
  Intf.Vec
    with type 'a k = 'a
     and type 'a m = 'a
     and type 'a shape = 'a Tensor.Int.t
     and type ('a, 'b) morphism = ('a, 'b) Tensor.Int.Morphism.t
     and type elt = float

(** [Rational] allows to manipulate [Q.t]-valued vectors *)
module Rational :
  Intf.Vec
    with type 'a k = 'a
     and type 'a m = 'a
     and type 'a shape = 'a Tensor.Int.t
     and type ('a, 'b) morphism = ('a, 'b) Tensor.Int.Morphism.t
     and type elt = Q.t

(**/**)

module Generic : sig
  val vec : 'a -> ('b -> 'c) -> ('a, 'b, 'c) Intf.vec

  val ovec : 'a -> ('b -> 'c -> 'd) -> ('a, 'b, 'c, 'd) Intf.ovec

  val map : ('a -> 'b) -> ('c, 'd, 'a) Intf.vec -> ('c, 'd, 'b) Intf.vec

  val mapi : ('a -> 'b -> 'c) -> ('d, 'a, 'b) Intf.vec -> ('d, 'a, 'c) Intf.vec
end

module Make_internal : functor
  (Repr : Basic_intf.Lang.Empty)
  (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
  (S : Basic_intf.Lang.Shape with type 'a m = 'a Repr.m)
  (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
  (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
  (R_storage : Basic_intf.Lang.Storage
                 with type 'a m = 'a Repr.m
                  and type elt = R.t)
  (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
  (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
  -> sig
  include
    Intf.Vec
      with type 'a k = 'a Monad.t
       and type 'a m = 'a Repr.m
       and type 'a shape = 'a S.t
       and type ('a, 'b) morphism = ('a, 'b) S.Morphism.t
       and type elt = R.t

  val unsafe_get : ('a, 'b, 'c) Intf.vec -> 'b -> 'c

  val unsafe_dot : 'a shape -> 'a t -> 'a t -> elt m
end

(**/**)
