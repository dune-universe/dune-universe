open Core_kernel
open Async_kernel
open Async_rpc_kernel

module Update = struct
  type ('response, 'error) t =
    | Attempting_new_connection
    | Connection_success of Rpc.Pipe_rpc.Metadata.t
    | Lost_connection
    | Failed_to_connect of Error.t
    | Rpc_error of 'error
    | Update of 'response
end

let filter_map_update update =
  let module L = Durable_state_rpc.Update in
  let module R = Update in
  match update with
  | L.Attempting_new_connection -> Some R.Attempting_new_connection
  | L.Connection_success metadata -> Some (R.Connection_success metadata)
  | L.Lost_connection -> Some R.Lost_connection
  | L.Failed_to_connect e -> Some (R.Failed_to_connect e)
  | L.Rpc_error e -> Some (R.Rpc_error e)
  | L.Update r -> Some (R.Update r)
  | L.State () -> None
;;

let create connection rpc ~query ~resubscribe_delay =
  let dispatch conn =
    Rpc.Pipe_rpc.dispatch rpc conn query
    >>|? Result.map ~f:(fun (pipe, id) -> (), pipe, id)
  in
  Durable_state_rpc.Expert.create connection ~dispatch ~resubscribe_delay
  |> Pipe.filter_map ~f:filter_map_update
;;

let create_versioned
      (type query response error)
      connection
      rpc_module
      ~(query : query)
      ~resubscribe_delay
  =
  let dispatch conn =
    let module Pipe_rpc = (val rpc_module : Versioned_rpc.Both_convert.Pipe_rpc.S
                           with type caller_query = query
                            and type caller_response = response
                            and type caller_error = error)
    in
    Pipe_rpc.dispatch_multi conn query
    >>|? Result.map ~f:(fun (pipe, id) -> (), pipe, id)
  in
  Durable_state_rpc.Expert.create connection ~dispatch ~resubscribe_delay
  |> Pipe.filter_map ~f:filter_map_update
;;

let create_versioned'
      (type query response error)
      connection
      rpc_module
      ~(query : query)
      ~resubscribe_delay
  =
  let dispatch conn =
    let module Pipe_rpc = (val rpc_module : Versioned_rpc.Caller_converts.Pipe_rpc.S
                           with type query = query
                            and type response = response
                            and type error = error)
    in
    Pipe_rpc.dispatch_multi conn query
    >>|? Result.map ~f:(fun (pipe, id) -> (), pipe, id)
  in
  Durable_state_rpc.Expert.create connection ~dispatch ~resubscribe_delay
  |> Pipe.filter_map ~f:filter_map_update
;;

let create_or_fail connection rpc ~query ~resubscribe_delay =
  let dispatch conn =
    Rpc.Pipe_rpc.dispatch rpc conn query
    >>|? Result.map ~f:(fun (pipe, id) -> (), pipe, id)
  in
  Durable_state_rpc.Expert.create_or_fail connection ~dispatch ~resubscribe_delay
  >>|? Result.map ~f:(Pipe.filter_map ~f:filter_map_update)
;;

let create_or_fail_versioned
      (type query response error)
      connection
      rpc_module
      ~(query : query)
      ~resubscribe_delay
  =
  let dispatch conn =
    let module Pipe_rpc = (val rpc_module : Versioned_rpc.Both_convert.Pipe_rpc.S
                           with type caller_query = query
                            and type caller_response = response
                            and type caller_error = error)
    in
    Pipe_rpc.dispatch_multi conn query
    >>|? Result.map ~f:(fun (pipe, id) -> (), pipe, id)
  in
  Durable_state_rpc.Expert.create_or_fail connection ~dispatch ~resubscribe_delay
  >>|? Result.map ~f:(Pipe.filter_map ~f:filter_map_update)
;;

let create_or_fail_versioned'
      (type query response error)
      connection
      rpc_module
      ~(query : query)
      ~resubscribe_delay
  =
  let dispatch conn =
    let module Pipe_rpc = (val rpc_module : Versioned_rpc.Caller_converts.Pipe_rpc.S
                           with type query = query
                            and type response = response
                            and type error = error)
    in
    Pipe_rpc.dispatch_multi conn query
    >>|? Result.map ~f:(fun (pipe, id) -> (), pipe, id)
  in
  Durable_state_rpc.Expert.create_or_fail connection ~dispatch ~resubscribe_delay
  >>|? Result.map ~f:(Pipe.filter_map ~f:filter_map_update)
;;
