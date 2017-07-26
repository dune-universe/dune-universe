open Core
open Async

module Update = struct
  type ('update, 'error) t =
    | Attempting_new_connection
    | Connection_success of Rpc.Pipe_rpc.Metadata.t
    | Lost_connection
    | Failed_to_connect of Error.t
    | Rpc_error of 'error
    | Update of 'update
end

type ('query, 'response, 'error) t =
  { update_w : ('response, 'error) Update.t Pipe.Writer.t
  ; connection : Rpc.Connection.t Durable.t
  ; rpc : ('query, 'response, 'error) Rpc.Pipe_rpc.t
  ; query : 'query
  ; resubscribe_delay : Time.Span.t
  }

let subscription_active t = not (Pipe.is_closed t.update_w)

(* [subscription_active] will only be false if the client closes the reader returned by
   [create*] *)
let write t update =
  if subscription_active t
  then Pipe.write_without_pushback t.update_w update
  else ()
;;

let try_to_get_fresh_pipe t =
  write t Update.Attempting_new_connection;
  Durable.with_ t.connection ~f:(fun connection ->
    Rpc.Pipe_rpc.dispatch t.rpc connection t.query)
  >>| function
  | Error err -> Error (`Failed_to_connect err)
  | Ok result ->
    match result with
    | Error e -> Error (`Rpc_error e)
    | Ok (pipe, id) -> Ok (pipe, id)
;;

let rec subscribe t =
  if not (subscription_active t)
  then return `Subscription_no_longer_active
  else
    begin
      try_to_get_fresh_pipe t
      >>= function
      | Error err ->
        begin
          match err with
          | `Failed_to_connect e -> write t (Update.Failed_to_connect e);
          | `Rpc_error e -> write t (Update.Rpc_error e)
        end;
        Clock.after t.resubscribe_delay
        >>= fun () ->
        subscribe t
      | Ok (pipe, id) ->
        write t (Update.Connection_success id);
        return (`Ok pipe)
    end
;;

let rec handle_update_pipe t deferred_pipe =
  deferred_pipe
  >>> function
  | `Subscription_no_longer_active -> ()
  | `Ok pipe ->
    (* Pipe.transfer_is determined when [pipe] is closed (as when we lose our connection),
       or when [t.update_w] is closed (as when the client closes the reader returned by
       [create*] *)
    Pipe.transfer pipe t.update_w ~f:(fun update -> (Update.Update update))
    >>> fun () ->
    write t Update.Lost_connection;
    handle_update_pipe t (subscribe t)
;;

let create_internal connection rpc ~query ~resubscribe_delay =
  let update_r, update_w = Pipe.create () in
  let t = { update_w ; connection ; rpc ; query ; resubscribe_delay } in
  update_r, t
;;

let create connection rpc ~query ~resubscribe_delay =
  let update_r, t = create_internal connection rpc ~query ~resubscribe_delay in
  handle_update_pipe t (subscribe t);
  update_r
;;

let create_or_fail connection rpc ~query ~resubscribe_delay =
  let update_r, t = create_internal connection rpc ~query ~resubscribe_delay in
  try_to_get_fresh_pipe t
  >>| function
  | Error (`Failed_to_connect e) -> Error e
  | Error (`Rpc_error e) -> Ok (Error e)
  | Ok (fresh_pipe, id) ->
    write t (Update.Connection_success id);
    handle_update_pipe t (return (`Ok fresh_pipe));
    Ok (Ok update_r)
;;
