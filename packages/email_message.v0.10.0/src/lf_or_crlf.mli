open! Core

type t =
  [ `LF  (** \n. Used to delineate new lines on most Unix systems. *)
  | `CRLF  (** \r\n. Used to delineate new lines over the network. *)
  ]

val to_string : t -> string
