open! Core

val decode_quoted_printable : int -> Lexing.lexbuf ->
  (Bigbuffer.t * [`Ok | `Unexpected_characters ])
;;

(* quoted printable is ALWAYS encoded as text *)
val encode_quoted_printable : int -> Lexing.lexbuf -> Bigbuffer.t
