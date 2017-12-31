open! Core_kernel

include module type of struct include Protocol.Rpc_error end

include Stringable.S with type t := t

exception Rpc of t * Info.t

val raise : t -> Info.t -> 'a
