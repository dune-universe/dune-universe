(** Netstring implementation *)

val decode : char Lwt_stream.t -> string Lwt.t
(** Consume and decode a netstring from a stream *)

val encode : string -> string
(** Encode a netstring: "abc" -> "3:abc," *)
