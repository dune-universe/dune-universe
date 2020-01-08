open! Core
open Async

type t =
  { opcode : Opcode.t
  ; final : bool
  ; content : string
  }
[@@deriving sexp_of]

module Error : sig
  type t =
    { code : Connection_close_reason.t
    ; message : string
    }
end

val read_frame : Reader.t -> (t, Error.t) Result.t Deferred.t
val write_frame : masked:bool -> Writer.t -> t -> unit
val create : opcode:Opcode.t -> ?final:bool -> string -> t
val create_close : code:int -> ?final:bool -> string -> t
