open Core
open Async

type ('sexp, 'a, 'b) load = string -> ('sexp -> 'a) -> 'b Deferred.t

val load_sexp : (Sexp.t, 'a, 'a Or_error.t) load
val load_sexp_exn : (Sexp.t, 'a, 'a) load
val load_sexps : (Sexp.t, 'a, 'a list Or_error.t) load
val load_sexps_exn : (Sexp.t, 'a, 'a list) load

module Macro_loader : sig
  val load_sexps_conv : string -> (Sexp.t -> 'a) -> 'a Macro.annot_conv list Deferred.t
end
