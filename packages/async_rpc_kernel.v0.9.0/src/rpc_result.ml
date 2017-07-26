open Core_kernel
open Async_kernel

type 'a t = ('a, Rpc_error.t) Result.t [@@deriving bin_io]

type located_error =
  { location : string
  ; exn : Exn.t
  }
[@@deriving sexp_of]

let uncaught_exn ~location exn =
  Error (Rpc_error.Uncaught_exn (sexp_of_located_error { location; exn }))
;;

let bin_io_exn ~location exn =
  Error (Rpc_error.Bin_io_exn (sexp_of_located_error { location; exn }))
;;

let try_with ?run ~location f =
  let x = Monitor.try_with ?run f in
  let join = function
    | Ok x -> x
    | Error exn -> uncaught_exn ~location exn
  in
  match Deferred.peek x with
  | None -> x >>| join
  | Some x -> return (join x)
;;

let or_error ~rpc_tag ~rpc_version ~connection_description = function
  | Ok x -> Ok x
  | Error rpc_error ->
    Or_error.error_s
      [%sexp
        { rpc_error = (rpc_error : Rpc_error.t)
        ; connection_description = (connection_description : Info.t)
        ; rpc_tag = (rpc_tag : Protocol.Rpc_tag.t)
        ; rpc_version = (rpc_version : int)
        }]
;;
