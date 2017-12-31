open Core_kernel

include Protocol.Rpc_error
include Sexpable.To_stringable (Protocol.Rpc_error)

exception Rpc of t * Info.t [@@deriving sexp]
let raise t connection_description = raise (Rpc (t, connection_description))
