open! Core

type t =
  [ `LF
  | `CRLF
  ]

let to_string = function
  | `LF   -> "\n"
  | `CRLF -> "\r\n"
;;
