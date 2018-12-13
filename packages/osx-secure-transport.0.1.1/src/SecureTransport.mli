type t

type protocol_side =
 | Server
 | Client

type connection_type =
  | Stream
  | Datagram

exception Error of int

val string_of_error_code : int -> string

val init : protocol_side -> connection_type -> t

val set_connection : t -> Unix.file_descr -> unit

val set_peer_domain_name : t -> string -> unit

val handshake : t -> unit

val read : t -> bytes -> int -> int -> int

val write : t -> bytes -> int -> int -> int

val close : t -> unit

type certificate

val import_p12_certificate : ?password:string -> string -> certificate list

val set_certificate : t -> certificate -> unit
