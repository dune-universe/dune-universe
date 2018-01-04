open Core
open Async

module Environment = struct
  type t = {
    connection_to_upstream_server : Rpc.Connection.t;
    connection_to_client : Rpc.Connection.t;
  }
end

(* This is just to improve errors *)
let raise_on_closed_connection connection =
  if Rpc.Connection.is_closed connection then
    failwith "rpc proxy connection closed"

let rpc_proxy :
  'q 'r. ('q, 'r) Rpc.Rpc.t -> Environment.t Rpc.Implementation.t
  = fun rpc ->
    Rpc.Rpc.implement rpc (fun (env : Environment.t) query ->
        raise_on_closed_connection env.connection_to_upstream_server;
        Rpc.Rpc.dispatch_exn rpc env.connection_to_upstream_server query)

let one_way_proxy :
  'm . 'm Rpc.One_way.t -> Environment.t Rpc.Implementation.t
  = fun rpc ->
    Rpc.One_way.implement rpc (fun (env : Environment.t) query ->
      raise_on_closed_connection env.connection_to_upstream_server;
      Or_error.ok_exn (Rpc.One_way.dispatch rpc env.connection_to_upstream_server query))

let hack_to_avoid_incomplete_pipe pipe close_reason ~connection_to_client =
  Pipe.create_reader ~close_on_exception:false
    (fun writer ->
       Pipe.transfer_id pipe writer
       >>= fun () ->
       close_reason
       >>= function
       | Rpc.Pipe_close_reason.Closed_locally ->
         assert (Pipe.is_closed writer);
         return ()
       | Closed_remotely ->
         Pipe.close writer; return ()
       | Error e ->
         Rpc.Connection.close connection_to_client
         >>| fun () -> Error.raise e)

let pipe_proxy :
  'q 'e 'r. ('q, 'e, 'r) Rpc.Pipe_rpc.t -> Environment.t Rpc.Implementation.t
  = fun rpc ->
    Rpc.Pipe_rpc.implement rpc
      (fun (env : Environment.t) query ->
         raise_on_closed_connection env.connection_to_upstream_server;
         Rpc.Pipe_rpc.dispatch rpc env.connection_to_upstream_server query
         >>= function
         | Error error -> Error.raise error
         | Ok (Error error) -> return (Error error)
         | Ok (Ok (pipe, id)) ->
           let pipe =
             hack_to_avoid_incomplete_pipe pipe (Rpc.Pipe_rpc.close_reason id)
               ~connection_to_client:env.connection_to_client
           in
           return (Ok pipe)
      )

let state_proxy :
  'q 's 'u 'e. ('q, 's, 'u, 'e) Rpc.State_rpc.t -> Environment.t Rpc.Implementation.t
  = fun rpc ->
    Rpc.State_rpc.implement rpc
      (fun (env : Environment.t) query ->
         raise_on_closed_connection env.connection_to_upstream_server;
         Rpc.State_rpc.dispatch rpc env.connection_to_upstream_server query
         >>= function
         | Error error -> Error.raise error
         | Ok (Error error) -> return (Error error)
         | Ok (Ok (state, pipe, id)) ->
           let pipe =
             hack_to_avoid_incomplete_pipe pipe (Rpc.State_rpc.close_reason id)
               ~connection_to_client:env.connection_to_client
           in
           return (Ok (state, pipe))
      )

let proxy : Rpc.Any.t -> _ = function
  | Rpc.Any.Rpc     rpc -> rpc_proxy     rpc
  | Rpc.Any.Pipe    rpc -> pipe_proxy    rpc
  | Rpc.Any.State   rpc -> state_proxy   rpc
  | Rpc.Any.One_way rpc -> one_way_proxy rpc
