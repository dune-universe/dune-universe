open! Core
open! Async

val edit_file : ?success_message:string
  -> post_hook:(unit -> unit Deferred.t)
  -> path:string
  -> unit
  -> unit Deferred.t

