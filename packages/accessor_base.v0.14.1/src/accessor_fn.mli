open! Base
open! Import

(** Access the inverse of a predicate. *)
val negated
  : ( 'i -> ('a -> bool) -> 'b -> bool
    , 'i -> ('a -> bool) -> 'b -> bool
    , [< isomorphism ] )
      Accessor.t

(** You can't really extract the result of a function without applying it to something,
    but you can still map over it. *)
val result : ('i -> 'a -> 'b, 'i -> ('c -> 'a) -> 'c -> 'b, [< mapper ]) Accessor.t

(** Like [result], but uses whatever argument is supplied to the resulting function as an
    index. *)
val resulti : ('c * 'i -> 'a -> 'b, 'i -> ('c -> 'a) -> 'c -> 'b, [< mapper ]) Accessor.t

(** Access a function with its arguments flipped. *)
val flipped
  : ( 'i -> ('a -> 'b -> 'c) -> 'd -> 'e -> 'f
    , 'i -> ('b -> 'a -> 'c) -> 'e -> 'd -> 'f
    , [< isomorphism ] )
      Accessor.t

(** This applicative interface allows you to pass some environment around as you access
    something. *)
include
  Accessor.Applicative.S2 with type ('output, 'input) t := 'input -> 'output
