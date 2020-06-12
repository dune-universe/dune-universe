open! Core_kernel
open! Import

type t = Host_and_port.t =
  { host : string
  ; port : int
  }
[@@deriving accessors]

let tuple =
  [%accessor
    Accessor.isomorphism
      ~get:(fun { Host_and_port.host; port } -> host, port)
      ~construct:(fun (host, port) -> { Host_and_port.host; port })]
;;
