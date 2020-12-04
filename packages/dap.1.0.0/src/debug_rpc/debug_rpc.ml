open Debug_protocol

let src = Logs.Src.create "dap.rpc.Debug_rpc"
module Log = (val Logs_lwt.src_log src : Logs_lwt.LOG)

type t = {
  in_ : Lwt_io.input_channel;
  out : Lwt_io.output_channel;
  mutable next_seq : int;
  wakeners : (int, Response.t Lwt.u) Hashtbl.t;
  handlers : (string, t -> Request.t -> string -> unit Lwt.t) Hashtbl.t;
  cancel_signals : (int, unit Lwt.u) Hashtbl.t;
  event : Event.t React.E.t * (?step:React.step -> Event.t-> unit);
}

let next_seq rpc =
  let seq = rpc.next_seq in
  rpc.next_seq <- rpc.next_seq + 1;
  seq

let send_message rpc msg_json =
  let raw_msg = Yojson.Safe.to_string msg_json in
  let%lwt () = Lwt_io.write rpc.out "Content-Length: " in
  let%lwt () = Lwt_io.write rpc.out (string_of_int (String.length raw_msg)) in
  let%lwt () = Lwt_io.write rpc.out "\r\n\r\n" in
  let%lwt () = Lwt_io.write rpc.out raw_msg in
  Log.debug (fun m -> m "Message sent -- %s" raw_msg)

let wait_response rpc req_seq =
  let cleanup () =
    Hashtbl.remove rpc.wakeners req_seq;
    Lwt.return ()
  in
  Lwt.finalize (fun () ->
    let (waiter, wakener) = Lwt.task () in
    assert (not (Hashtbl.mem rpc.wakeners req_seq));
    Hashtbl.replace rpc.wakeners req_seq wakener;
    let%lwt res = waiter in
    Lwt.return res
  ) cleanup

let event : type e. t -> (module EVENT with type Payload.t = e) -> e React.E.t =
  fun rpc (module The_event) ->
    let (event_e, _) = rpc.event in
    event_e |> React.E.fmap (fun evt ->
      if evt.Event.event = The_event.type_ then
        The_event.Payload.of_yojson evt.body |> Result.to_option
      else None
    )

let send_event : type e. t -> (module EVENT with type Payload.t = e) -> e -> unit Lwt.t =
  fun rpc (module The_event) body ->
    send_message rpc Event.(
      make
        ~seq:(next_seq rpc)
        ~type_:Event.Type.Event
        ~event:(The_event.type_)
        ~body:(The_event.Payload.to_yojson body)
        ()
      |> to_yojson
    )

let rec exec_command : type arg res. t -> (module COMMAND with type Arguments.t = arg and type Result.t = res) -> arg -> res Lwt.t =
  fun rpc (module The_command) arg ->
    let req_seq = next_seq rpc in
    let req = Request.make
      ~seq:req_seq
      ~type_:Request.Type.Request
      ~arguments:(The_command.Arguments.to_yojson arg)
      ~command:The_command.type_
      ()
    in
    let%lwt () = send_message rpc (Request.to_yojson req) in
    let%lwt res =
      try%lwt
        wait_response rpc req_seq
      with Lwt.Canceled -> (
        let%lwt () = exec_command rpc (module Cancel_command) Cancel_command.Arguments.(
          make ~request_id:(Some req_seq) ()
        ) in
        Lwt.fail Lwt.Canceled
      )
    in
    let res_body = The_command.Result.of_yojson res.body |> Result.get_ok in
    Lwt.return res_body

let register_command : type arg res. t -> (module COMMAND with type Arguments.t = arg and type Result.t = res) -> (t -> arg -> string -> res Lwt.t) -> unit =
  fun rpc (module The_command) f ->
    let handler rpc (req : Request.t) raw_msg =
      let%lwt res =
        try%lwt
          let%lwt res = f rpc (The_command.Arguments.of_yojson req.arguments |> Result.get_ok) raw_msg in
          Lwt.return (Response.make
            ~seq:(next_seq rpc)
            ~type_:Response.Type.Response
            ~request_seq:req.seq
            ~success:true
            ~command:The_command.type_
            ~body:(The_command.Result.to_yojson res)
            ()
          )
        with exn ->
          Lwt.return (Response.make
            ~seq:(next_seq rpc)
            ~type_:Response.Type.Response
            ~request_seq:req.seq
            ~success:true
            ~command:The_command.type_
            ~message:(Some (
              match exn with
              | Lwt.Canceled -> Response.Message.Cancelled
              | _ -> Response.Message.Custom (Printexc.to_string exn)
            ))
            ()
          )
      in
      send_message rpc (res |> Response.to_yojson)
    in
    Hashtbl.replace rpc.handlers The_command.type_ handler

let handle_cancel rpc (arg : Cancel_command.Arguments.t) _raw_msg =
  match arg.request_id with
  | Some req_seq -> (
    let cancel_signal = Hashtbl.find rpc.cancel_signals req_seq in
    Lwt.wakeup_later_exn cancel_signal Lwt.Canceled;
    Lwt.return ()
  )
  | _ -> Log.warn (fun m -> m "Unsupported cancel request")

let create ~in_ ~out ?(next_seq=0) () =
  let rpc = {
    in_;
    out;
    next_seq;
    wakeners = Hashtbl.create 0;
    handlers = Hashtbl.create 0;
    cancel_signals = Hashtbl.create 0;
    event = React.E.create ()
  } in
  register_command rpc (module Cancel_command) handle_cancel;
  rpc

let start rpc =
  let module Parsers = struct
    let message =
      let open Angstrom in
      let eol = string "\r\n" in
      let colon = string ": " in
      let is_colon = function ':' -> true | _ -> false in
      let is_eol = function '\r' -> true | _ -> false in
      let header_field = lift2
          (fun key value -> key, value)
          (take_till is_colon <* colon)
          (take_till is_eol <* eol) in
      (many1 header_field <* eol) >>= fun headers ->
      let content_length = int_of_string (List.assoc "Content-Length" headers) in
      take content_length
  end in
  let dispatch_event evt _raw_msg =
    let (_, send_event) = rpc.event in
    send_event evt;
    Lwt.return ()
  in
  let dispatch_request req raw_msg =
    match Hashtbl.find_opt rpc.handlers req.Request.command with
    | Some handler -> (
      let (waiter, wakener) = Lwt.wait () in
      Hashtbl.replace rpc.cancel_signals req.Request.seq wakener;
      let cleanup () =
        Hashtbl.remove rpc.cancel_signals req.Request.seq;
        Lwt.return ()
      in
      Lwt.finalize (fun () -> Lwt.pick [
        handler rpc req raw_msg;
        waiter;
      ]) cleanup
    )
    | None -> (
      Log.warn (fun m -> m "Can not find handler")
    )
  in
  let dispatch_response res _raw_msg =
    let () =
    match Hashtbl.find_opt rpc.wakeners res.Response.request_seq with
    | Some wakener -> Lwt.wakeup_later wakener res
    | None -> Logs.debug (fun m -> m "Response skipped")
    in
    Lwt.return ()
  in
  let dispatch raw_msg =
    let%lwt () = Log.debug (fun m -> m "Message recv -- %s" raw_msg) in
    try%lwt
      let msg_json = Yojson.Safe.from_string raw_msg in
      let msg = Protocol_message.of_yojson msg_json |> Result.get_ok in
      match msg with
      | {type_ = Protocol_message.Type.Event; _} ->
        dispatch_event (Event.of_yojson msg_json |> Result.get_ok) raw_msg
      | {type_ = Protocol_message.Type.Request; _} ->
        dispatch_request (Request.of_yojson msg_json |> Result.get_ok) raw_msg
      | {type_ = Protocol_message.Type.Response; _} ->
        dispatch_response (Response.of_yojson msg_json |> Result.get_ok) raw_msg
      | _ -> failwith "Unsupported message type"
    with err -> Log.err (
      fun m -> m "Dispatch failed: %s\n%s" (Printexc.to_string err) (Printexc.get_backtrace ())
    )
  in
  try%lwt (
    let%lwt _ = Angstrom_lwt_unix.parse_many Parsers.message dispatch rpc.in_ in
    Lwt.return_unit
  ) with Lwt_io.Channel_closed _ -> Lwt.return ()
