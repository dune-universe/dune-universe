open! Core_kernel

type t [@@deriving sexp]

val of_json : Json.t -> t
val to_json : t -> Json.t
val submit_text : [ `markdown | `HTML ] -> t -> string
