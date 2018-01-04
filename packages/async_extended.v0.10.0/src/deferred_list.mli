
open! Core
open! Async

type how = [`parallel|`sequential]

val iter : how -> 'a list -> f:('a -> unit Deferred.t) -> unit Deferred.t

val map : how -> 'a list -> f:('a -> 'b Deferred.t) -> 'b list Deferred.t

val filter_map : how -> 'a list -> f:('a -> 'b option Deferred.t) -> 'b list Deferred.t

