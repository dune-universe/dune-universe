open! Core

(** Encoded words as per: https://tools.ietf.org/html/rfc2047 *)
val decode : string -> string Or_error.t
