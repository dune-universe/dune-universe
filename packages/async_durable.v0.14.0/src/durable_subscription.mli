open Core_kernel
open Async_kernel
open Async_rpc_kernel

module Update : sig
  type ('response, 'error) t =
    | Attempting_new_connection
    | Connection_success of Rpc.Pipe_rpc.Metadata.t
    | Lost_connection
    | Failed_to_connect of Error.t
    | Rpc_error of 'error
    | Update of 'response
end

val create
  :  Rpc.Connection.t Durable.t
  -> ('query, 'response, 'error) Rpc.Pipe_rpc.t
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> ('response, 'error) Update.t Pipe.Reader.t
[@@deprecated "[since 2018-04] use Durable_pipe_rpc instead"]

val create_or_fail
  :  Rpc.Connection.t Durable.t
  -> ('query, 'response, 'error) Rpc.Pipe_rpc.t
  -> query:'query
  -> resubscribe_delay:Time.Span.t
  -> (('response, 'error) Update.t Pipe.Reader.t, 'error) Result.t Or_error.t Deferred.t
[@@deprecated "[since 2018-04] use Durable_pipe_rpc instead"]
