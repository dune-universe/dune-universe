type lwt_rsocket
val socket      : Unix.socket_domain -> Unix.socket_type -> int -> lwt_rsocket
                                                                     
(* val show    : rsocket -> string *)
val identifier : lwt_rsocket -> int
val connect    : lwt_rsocket -> Unix.sockaddr -> unit Lwt.t
val close      : lwt_rsocket -> unit Lwt.t
val bind       : lwt_rsocket -> Unix.sockaddr -> unit
val setsockopt : lwt_rsocket -> Unix.socket_bool_option -> bool -> unit
val listen     : lwt_rsocket -> int -> unit
val recv       : lwt_rsocket -> bytes -> int -> int -> Unix.msg_flag list -> int Lwt.t
val send       : lwt_rsocket -> bytes -> int -> int -> Unix.msg_flag list -> int Lwt.t
val accept     : lwt_rsocket -> (lwt_rsocket * Unix.sockaddr ) Lwt.t

open Bigarray
module Bytes : sig
  type t =  (char, int8_unsigned_elt, c_layout) Array1.t
  val create : int -> t
  val send : lwt_rsocket ->  t -> int -> int -> Unix.msg_flag list -> int Lwt.t
  val recv : lwt_rsocket ->  t -> int -> int -> Unix.msg_flag list -> int Lwt.t
end


(* class rpoll : Lwt_engine.t *)
class rselect : Lwt_engine.t
