open! Core
open Async

module type S = sig
  type t [@@deriving of_sexp]
end

(** Generates a command that validates sexp files *)
val create
  :  ?multiple_sexps_per_file:bool (* default false *)
  -> name:string
  -> (module S)
  -> Command.t

(** Generates a command that validates arbitrary files, given a load function *)
val of_load
  :  name:string
  -> (string -> 'a Deferred.Or_error.t)
  -> Command.t
