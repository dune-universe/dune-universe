open Core
open Async

type ('sexp, 'a, 'b) load =
  ?allow_includes:bool -> string -> ('sexp -> 'a) -> 'b Deferred.t

val load_sexp : (Sexp.t, 'a, 'a Or_error.t) load
val load_sexp_exn : (Sexp.t, 'a, 'a) load
val load_sexps : (Sexp.t, 'a, 'a list Or_error.t) load
val load_sexps_exn : (Sexp.t, 'a, 'a list) load
val included_files : string -> string list Or_error.t Deferred.t

module Macro_loader : sig
  val load_sexps_conv
    :  ?allow_includes:bool
    -> string
    -> (Sexp.t -> 'a)
    -> 'a Macro.annot_conv list Deferred.t

  val included_files : string -> string list Deferred.t
end
