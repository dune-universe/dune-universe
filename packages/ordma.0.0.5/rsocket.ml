type rsocket = int

type ba =  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t
external rsocket  : Unix.socket_domain ->
                    Unix.socket_type -> int -> rsocket = "ordma_rsocket"

let show rsocket =
  string_of_int rsocket
                

external rconnect : rsocket -> Unix.sockaddr -> unit = "ordma_rconnect"
external rsend    : rsocket -> bytes -> int -> int -> Unix.msg_flag list
                    -> int = "ordma_rsend"
external rrecv    : rsocket -> bytes -> int -> int -> Unix.msg_flag list
                    -> int = "ordma_rrecv"

external rsend_ba : rsocket -> ba -> int -> int -> Unix.msg_flag list
                    -> int = "ordma_rsend_ba"
external rrecv_ba : rsocket -> ba -> int -> int -> Unix.msg_flag list
                    -> int = "ordma_rrecv_ba"

external rclose   : rsocket -> unit = "ordma_rclose"
                                        
external rbind    : rsocket -> Unix.sockaddr -> unit = "ordma_rbind"

external raccept  : rsocket -> rsocket * Unix.sockaddr = "ordma_raccept"
                                                           
external rlisten  : rsocket -> int -> unit = "ordma_rlisten"

type socket_error_option = SO_ERROR
                             
module SO: sig
  type ('opt, 'v) t
  val bool: (Unix.socket_bool_option, bool) t
  val int: (Unix.socket_int_option, int) t
  val optint: (Unix.socket_optint_option, int option) t
  val float: (Unix.socket_float_option, float) t
  val error: (socket_error_option, Unix.error option) t
  val get: ('opt, 'v) t -> rsocket -> 'opt -> 'v
  val set: ('opt, 'v) t -> rsocket -> 'opt -> 'v -> unit
end = struct
  type ('opt, 'v) t = int
  let bool = 0
  let int = 1
  let optint = 2
  let float = 3
  let error = 4
  external get: ('opt, 'v) t -> rsocket -> 'opt -> 'v
              = "ordma_rgetsockopt"
  external set: ('opt, 'v) t -> rsocket -> 'opt -> 'v -> unit
              = "ordma_rsetsockopt"
end

let rgetsockopt fd opt = SO.get SO.bool fd opt
let rsetsockopt fd opt v = SO.set SO.bool fd opt v
let rgetsockopt_error fd = SO.get SO.error fd SO_ERROR

external set_nonblock : rsocket -> unit = "ordma_set_nonblock"                                  
                                                    
module Version = struct
  include Ordma_version
end
