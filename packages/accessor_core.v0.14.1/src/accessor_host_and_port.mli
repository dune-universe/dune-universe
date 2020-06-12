open! Core_kernel
open! Import

include sig
  type t =
    { host : string
    ; port : int
    }
  [@@deriving accessors]
end
with type t := Host_and_port.t

(** Access a [Host_and_port.t] as a tuple of host and port. *)
val tuple : (_, string * int, Host_and_port.t, [< isomorphism ]) Accessor.Simple.t
