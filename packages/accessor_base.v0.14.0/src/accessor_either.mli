open! Base
open! Import

include sig
  type ('f, 's) t =
    | First of 'f
    | Second of 's
  [@@deriving accessors]
end
with type ('f, 's) t := ('f, 's) Either.t

(** Access an either where the cases are swapped with each other. *)
val swapped
  : ( 'i -> ('a, 'b) Either.t -> ('c, 'd) Either.t
    , 'i -> ('b, 'a) Either.t -> ('d, 'c) Either.t
    , [< isomorphism ] )
      Accessor.t

(** [assocl] and [assocr] reassociate nested eithers. *)

val assocl
  : ( 'i -> (('a, 'b) Either.t, 'c) Either.t -> (('d, 'e) Either.t, 'f) Either.t
    , 'i -> ('a, ('b, 'c) Either.t) Either.t -> ('d, ('e, 'f) Either.t) Either.t
    , [< isomorphism ] )
      Accessor.t

val assocr
  : ( 'i -> ('a, ('b, 'c) Either.t) Either.t -> ('d, ('e, 'f) Either.t) Either.t
    , 'i -> (('a, 'b) Either.t, 'c) Either.t -> (('d, 'e) Either.t, 'f) Either.t
    , [< isomorphism ] )
      Accessor.t

module Index : sig
  type t =
    | First
    | Second
  [@@deriving accessors, compare, hash, sexp_of]
end

(** Access the value stored by the either, regardless of which constructor it lives under.
    This value is guaranteed to exist, so this accessor is a field instead of a variant.
*)
val each
  : ('i -> 'a -> 'b, 'i -> ('a, 'a) Either.t -> ('b, 'b) Either.t, [< field ]) Accessor.t

(** Like [each], but also provides you with which constructor the value was under. *)
val eachi
  : ( Index.t * 'i -> 'a -> 'b
    , 'i -> ('a, 'a) Either.t -> ('b, 'b) Either.t
    , [< field ] )
      Accessor.t

module First : Accessor.Monad.S2 with type ('a, 'b) t := ('a, 'b) Either.t
module Second : Accessor.Monad.S2 with type ('a, 'b) t := ('b, 'a) Either.t
