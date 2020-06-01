open! Core
open! Async
open! Import

let rpc =
  let open Core.Core_stable in
  Rpc.Pipe_rpc.create
    ~name:"heartbeats-direct"
    ~version:1
    ~bin_query:Int.V1.bin_t
    ~bin_response:Unit.V1.bin_t
    ~bin_error:Error.V1.bin_t
    ()
;;

let implementation
      (_ : Command_rpc.Command.Invocation.t)
      num_messages
      direct_stream_writer
  =
  don't_wait_for
    (let%map () =
       Deferred.for_ 0 ~to_:(num_messages - 1) ~do_:(fun (_ : int) ->
         match Rpc.Pipe_rpc.Direct_stream_writer.write direct_stream_writer () with
         | `Closed -> return ()
         | `Flushed flushed -> flushed)
     in
     Rpc.Pipe_rpc.Direct_stream_writer.close direct_stream_writer);
  Deferred.Or_error.return ()
;;

let implementations = [ Rpc.Pipe_rpc.implement_direct rpc implementation ]
