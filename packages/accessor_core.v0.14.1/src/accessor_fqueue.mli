open! Core_kernel
open! Import

(** Access [()] iff the queue is empty. *)
val empty : (_, unit, 'a Fqueue.t, [< variant ]) Accessor.Simple.t

(** Access each element of the queue. *)
val each : ('i -> 'a -> 'b, 'i -> 'a Fqueue.t -> 'b Fqueue.t, [< many ]) Accessor.t

(** The indexed version of [each] provides a numeric index for each element. *)
val eachi
  : (int * 'i -> 'a -> 'b, 'i -> 'a Fqueue.t -> 'b Fqueue.t, [< many ]) Accessor.t

include Accessor.Monad.S with type 'a t := 'a Fqueue.t
