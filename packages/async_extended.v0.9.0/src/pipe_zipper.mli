open! Core
open! Async

type 'a t

val create : ?max_history:int -> (unit -> 'a Pipe.Reader.t Deferred.t) -> 'a t

val prev : 'a t -> 'a Or_error.t Deferred.t

val next : 'a t -> 'a Or_error.t Deferred.t

val zip : 'a t -> [`next | `prev] -> int -> 'a Or_error.t Deferred.t

val find : 'a t -> f:('a -> bool) -> 'a Or_error.t Deferred.t

val find_rev : 'a t -> f:('a -> bool) -> 'a Or_error.t Deferred.t

val find_first_larger_or_equal :
  'a t -> compare_with_target:('a -> int) -> 'a Or_error.t Deferred.t
