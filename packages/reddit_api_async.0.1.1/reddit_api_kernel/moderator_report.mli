open! Core_kernel

type t =
  { moderator : Username.t option
  ; report : string
  }

val of_json : Json.t -> t
