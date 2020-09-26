open! Core

type t =
  | Continuation
  | Text
  | Binary
  | Close
  | Ping
  | Pong
  | Ctrl of int
  | Nonctrl of int
[@@deriving sexp_of]

val of_int : int -> t
val to_int : t -> int
