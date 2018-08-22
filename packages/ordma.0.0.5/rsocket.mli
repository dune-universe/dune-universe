type rsocket
open Bigarray
type ba =  (char, int8_unsigned_elt, c_layout) Array1.t

val rsocket  : Unix.socket_domain ->
               Unix.socket_type ->
               int
               -> rsocket

val show     : rsocket -> string
val rconnect : rsocket -> Unix.sockaddr -> unit

val rsend    : rsocket -> bytes -> int -> int -> Unix.msg_flag list -> int
val rrecv    : rsocket -> bytes -> int -> int -> Unix.msg_flag list -> int

val rsend_ba : rsocket ->  ba -> int -> int -> Unix.msg_flag list -> int
val rrecv_ba : rsocket ->  ba -> int -> int -> Unix.msg_flag list -> int

val rclose   : rsocket -> unit
val rbind    : rsocket -> Unix.sockaddr -> unit
val rlisten  : rsocket -> int -> unit
val raccept  : rsocket -> rsocket * Unix.sockaddr
val rsetsockopt : rsocket -> Unix.socket_bool_option -> bool -> unit
val rgetsockopt_error : rsocket -> Unix.error option
val set_nonblock : rsocket -> unit
                                
module Version : sig
  val major : int
  val minor : int
  val patch : int
  val git_revision : string
  val summary : int * int * int * string
end
  
