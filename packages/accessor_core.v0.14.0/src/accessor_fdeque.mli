open! Core_kernel
open! Import

(** Access [()] iff the queue is empty. *)
val empty : (_, unit, 'a Fdeque.t, [< variant ]) Accessor.Simple.t

(** Access the front element and remainder of the queue, if it is non-empty. *)
val front : (_, 'a * 'a Fdeque.t, 'a Fdeque.t, [< variant ]) Accessor.Simple.t

(** Access the back element and remainder of the queue, if it is non-empty. *)
val back : (_, 'a Fdeque.t * 'a, 'a Fdeque.t, [< variant ]) Accessor.Simple.t

(** Access the first element of the queue, if it is non-empty. *)
val first : (_, 'a, 'a Fdeque.t, [< optional ]) Accessor.Simple.t

(** Access the last element of the queue, if it is non-empty. *)
val last : (_, 'a, 'a Fdeque.t, [< optional ]) Accessor.Simple.t

(** Access a reversed version of the queue. *)
val reversed
  : ( 'i -> 'a Fdeque.t -> 'b Fdeque.t
    , 'i -> 'a Fdeque.t -> 'b Fdeque.t
    , [< isomorphism ] )
      Accessor.t

(** Access each element of the queue. *)
val each : ('i -> 'a -> 'b, 'i -> 'a Fdeque.t -> 'b Fdeque.t, [< many ]) Accessor.t

(** The indexed version of [each] provides a numeric index for each element. *)
val eachi
  : (int * 'i -> 'a -> 'b, 'i -> 'a Fdeque.t -> 'b Fdeque.t, [< many ]) Accessor.t

include Accessor.Monad.S with type 'a t := 'a Fdeque.t
