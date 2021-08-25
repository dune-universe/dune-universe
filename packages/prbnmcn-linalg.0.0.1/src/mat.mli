(** Linalg: matrices *)

(** [Make] takes as input the signature of an object language and
    generates a module to perform matrix computations in this language. *)
module Make : functor
  (Repr : Basic_intf.Lang.Empty)
  (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
  (S : Intf.Tensor with type 'a m = 'a Repr.m and type 'a k = 'a Monad.t)
  (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
  (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m)
  (R_storage : Basic_intf.Lang.Storage
                 with type 'a m = 'a Repr.m
                  and type elt = R.t)
  (I_ring : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m and type t = S.pos)
  (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
  (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m)
  (P : Basic_intf.Lang.Product with type 'a m = 'a Repr.m)
  ->
  Intf.Mat
    with type 'a k = 'a Monad.t
     and type 'a m = 'a Repr.m
     and type base_index = I_ring.t
     and type 'a shape = 'a S.t
     and type ('a, 'b) morphism = ('a, 'b) S.Morphism.t
     and type elt = R.t

(** [Array_backed_column_major] yields a module that allows to create matrices backed by arrays.
    This module enforces a column-major layout for the matrix data (Fortran-style).
 *)
module Array_backed_column_major
    (Repr : Basic_intf.Lang.Empty)
    (Monad : Basic_intf.Codegen_monad with type 'a m = 'a Repr.m)
    (S : Intf.Tensor with type 'a m = 'a Repr.m)
    (B : Basic_intf.Lang.Bool with type 'a m = 'a Repr.m)
    (R : Basic_intf.Lang.Ring with type 'a m = 'a Repr.m and type t = S.pos)
    (R_ord : Basic_intf.Lang.Infix_order
               with type 'a m = 'a Repr.m
                and type t = R.t)
    (A : Basic_intf.Lang.Array with type index = R.t and type 'a m = 'a Repr.m)
    (E : Basic_intf.Lang.Exn with type 'a m = 'a Repr.m)
    (P : Basic_intf.Lang.Product with type 'a m = 'a Repr.m)
    (M : Basic_intf.Lang.Sequencing with type 'a m = 'a Repr.m) : sig
  type index = S.pos * S.pos

  val of_array :
    ('a * S.pos) S.t ->
    A.t Repr.m ->
    ((('a * S.pos) S.t, (S.pos * S.pos) Repr.m, A.elt Repr.m) Intf.vec
    * ( ('a * S.pos) S.t,
        (S.pos * S.pos) Repr.m,
        A.elt Repr.m,
        unit Repr.m )
      Intf.ovec)
    Monad.t

  val in_of_array :
    ('a * S.pos) S.t ->
    A.t Repr.m ->
    (('a * S.pos) S.t, (S.pos * S.pos) Repr.m, A.elt Repr.m) Intf.vec Monad.t

  val out_of_array :
    ('a * S.pos) S.t ->
    A.t Repr.m ->
    ( ('a * S.pos) S.t,
      (S.pos * S.pos) Repr.m,
      A.elt Repr.m,
      unit Repr.m )
    Intf.ovec
    Monad.t
end

(** [Make_native] specializes [Make] to native execution. *)
module Make_native
    (R : Basic_intf.Lang.Ring with type 'a m = 'a)
    (R_storage : Basic_intf.Lang.Storage with type 'a m = 'a and type elt = R.t) :
  Intf.Mat
    with type 'a k = 'a
     and type 'a m = 'a
     and type base_index = Tensor.Int.pos
     and type 'a shape = 'a Tensor.Int.t
     and type ('a, 'b) morphism = ('a, 'b) Tensor.Int.Morphism.t
     and type elt = R.t

(** [Float] allows to manipulate float-valued matrices *)
module Float :
  Intf.Mat
    with type 'a k = 'a
     and type 'a m = 'a
     and type base_index = Tensor.Int.pos
     and type 'a shape = 'a Tensor.Int.t
     and type ('a, 'b) morphism = ('a, 'b) Tensor.Int.Morphism.t
     and type elt = float

(** [Float] allows to manipulate [Q.t]-valued matrices *)
module Rational :
  Intf.Mat
    with type 'a k = 'a
     and type 'a m = 'a
     and type base_index = Tensor.Int.pos
     and type 'a shape = 'a Tensor.Int.t
     and type ('a, 'b) morphism = ('a, 'b) Tensor.Int.Morphism.t
     and type elt = Q.t
