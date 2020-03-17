(** Generalizes the notion of an asynchronous stream. *)
type 'a t = private ('a -> unit) -> unit

type 'a input = 'a t

(** Asynchronous output streams are repesented identically to input streams ({i push}-based) .*)
type 'a output = 'a t

(** Represents a connection-based input stream with a synchronous output stream to close it. *)
type 'a connection = ('a input, unit Sync.output) Connection.t

(** Represents a connection-based input stream with an asynchronous output stream to close it. *)
type 'a connection' = ('a input, unit output) Connection.t

val make : (('a -> unit) -> unit) -> 'a t

(**
 Listens for values from an asynchronous stream, somewhat similar to {!Sync.next} but push-based
 instead of pull-based.
 *)
val listen : ('a -> unit) -> 'a t -> unit

val pure : 'a -> 'a t

val map : ('a -> 'b) -> 'a t -> 'b t

val filter : ('a -> bool) -> 'a t -> 'a t

val scan : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b t

module type INTERVAL = sig
  (**
   This functor is for cross-platform compatibility. The functionality needs to work like
   {{:https://bucklescript.github.io/bucklescript/api/Js.Global.html#VALsetInterval} Js.Global.setInterval}.
   *)

  type interval_id

  val set_interval : (unit -> unit) -> int -> interval_id

  val clear_interval : interval_id -> unit
end

module Interval (I : INTERVAL) : sig
  (**
   Creates an infinite async stream that enumerates the natural numbers every [ms].

   If this stream is converted to a finite stream with {!Lambda_streams.Finite.Async.take'} it will
   still be considered active (the interval will not be cleared). It will just stop sending signals
   after whatever [n] was passed to [take'].

   To get an infinite async stream that can be converted later into a finite stream that ends, use
   {!make} and pass the output stream that closes it to {!Lambda_streams.Finite.Async.take'}.
   *)
  val forever : ms:int -> int t

  (**
   Creates an async stream that enumerates the natural numbers every [ms]. This is a
   connection-based version that get closed (no more signals are sent) when the output stream is
   called.
   *)
  val make : ms:int -> int connection
end
