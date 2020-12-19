open! Core_kernel

type t [@@deriving sexp]

include Json_object.S_with_fields with type t := t
include Json_object.S_with_kind with type t := t
