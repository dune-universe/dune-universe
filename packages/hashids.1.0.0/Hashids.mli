(* Copyright 2016-2017 Vincent Jacques <vincent@vincent-jacques.net> *)

type t

val make: ?salt:string -> ?min_length:int -> ?alphabet:string -> unit -> t
(**
    ``salt``: an arbitrary string used to randomize the encoding.

    ``min_length``: an integer specifying the minimal length of encoded values.
    Useful to hide their order of magnitude.

    ``alphabet``: the set of characters used to encode.
*)

val encode: t -> int list -> string

val decode: t -> string -> int list

module Tests: sig
  val test: General.Testing.Test.t
end [@@autodoc.hide]
