type bigstring =
  (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Decoder : sig
  type event =
    [ `Request of Cohttp.Request.t
    | `Data of string
    | `Need_data
    | `Error of string
    | `Request_complete ]

  type decoder

  val consumed : decoder -> int
  val decoder : unit -> decoder
  val decode : decoder -> event
  val src : decoder -> bigstring -> pos:int -> len:int -> unit
  val unconsumed : decoder -> int
  val next_cycle : decoder -> unit
end

val serialize_response : Bytebuffer.t -> Cohttp.Response.t -> unit
