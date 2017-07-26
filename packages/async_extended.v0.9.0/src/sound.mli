
open! Async

type t

val default : t
val of_file : string -> t
val to_file : t -> string
val play : ?quiet:bool -> t -> unit Deferred.t
