open! Core
open! Async

type t

(** [create name username host] create a new enforcer controller *)
val create : string -> string -> string -> t

val name : t -> string
val host : t -> string
val username : t -> string

(** [running t] return true if the enforcer is running *)
val running : t -> bool Deferred.t

(** [start t] start the enforcer *)
val start : t -> unit Deferred.t

(** [stop t] stop the enforcer *)
val shutdown : t -> unit Deferred.t

(** [kill t] kill the enforcer *)
val kill : t -> unit Deferred.t

