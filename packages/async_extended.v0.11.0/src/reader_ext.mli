open! Core
open! Async

val input_sexps : Reader.t -> Sexp.t list Deferred.t

val open_gzip_file : string -> Reader.t Deferred.t

(** Start a process and read its stdout as input from a Reader.t.

    If the process writes anything to stderr it will be thrown as an
    exception after reading is finished.
*)
val with_input_from_process
  :  prog:string
  -> args:string list
  -> f:(Reader.t -> 'a Deferred.t)
  -> 'a Deferred.t

val with_gzip_file        : string             -> f:(Reader.t -> 'a Deferred.t) -> 'a Deferred.t
val with_hadoop_gzip_file : hadoop_file:string ->   (Reader.t -> 'a Deferred.t) -> 'a Deferred.t
val with_xzip_file        : string             -> f:(Reader.t -> 'a Deferred.t) -> 'a Deferred.t
