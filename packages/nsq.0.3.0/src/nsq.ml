open Base
open Lwt

let ephemeral_suffix = "#ephemeral"
let default_backoff_seconds = 1.0
let max_backoff_seconds = 3600.0
let default_nsqd_port = 4150
let default_lookupd_port = 4161
let network_buffer_size = 16 * 1024
let recalculate_rdy_interval = 60.0

module Milliseconds = struct
  type t = Milliseconds of int64

  let of_int64 i =
    Milliseconds i

  let value (Milliseconds i) = i
end

let default_requeue_delay = Milliseconds.of_int64 5000L

module MessageID = struct
  type t =
      MessageID of bytes

  let of_bytes b =
    MessageID b

  let to_string = function
    | MessageID id -> Bytes.to_string id
end

let ephemeral s =
  s ^ ephemeral_suffix

module Topic = struct
  type t =
    | Topic of string
    | TopicEphemeral of string

  let to_string = function
    | Topic s -> s
    | TopicEphemeral s -> ephemeral s
end

module Channel = struct 
  type t =
    | Channel of string
    | ChannelEphemeral of string

  let to_string = function
    | Channel s -> s
    | ChannelEphemeral s -> ephemeral s
end

module FrameType = struct
  type t =
    | FrameResponse
    | FrameError
    | FrameMessage
    | FrameUnknown of int32

  let of_int32 = function
    | 0l -> FrameResponse
    | 1l -> FrameError
    | 2l -> FrameMessage
    | i -> FrameUnknown i
end

type raw_frame = {
  frame_type: int32;
  data: Bytes.t; 
}

type raw_message = {
  timestamp: int64;
  attempts: Unsigned.UInt16.t;
  id: MessageID.t;
  body: Bytes.t; 
}

module Address = struct
  module T = struct
    type t =
      | Host of string
      | HostPort of string * int
    [@@deriving sexp_of, compare]
  end
  include T
  include Comparable.Make(T)

  let host s =
    Host s

  let host_port a p =
    HostPort (a, p)

  let to_string a =
    match a with
    | Host a -> a
    | HostPort (a, p) -> Printf.sprintf "%s:%d" a p
end

module IdentifyConfig = struct
  (* This will be encoded to JSON and sent with the IDENTIFY command *)
  type t = {
    heartbeat_interval : int; (* In milliseconds  *)
    client_id : string;
    hostname : string;
    user_agent : string;
    output_buffer_size : int; (* buffer size on bytes the server should use  *)
    output_buffer_timeout : int;
    sample_rate : int;
  } [@@deriving yojson { strict = false }]
end

type command =
  | IDENTIFY of IdentifyConfig.t
  | Magic
  | SUB of Topic.t * Channel.t
  | PUB of Topic.t * Bytes.t
  | MPUB of Topic.t * Bytes.t list
  | REQ of MessageID.t * Milliseconds.t
  | FIN of MessageID.t
  | TOUCH of MessageID.t
  | RDY of int
  | AUTH of string
  | CLS
  | NOP

module ServerMessage = struct
  type t =
    | ResponseOk
    | Heartbeat
    | ErrorInvalid of string
    | ErrorBadTopic of string
    | ErrorBadChannel of string
    | ErrorFINFailed of string
    | ErrorREQFailed of string
    | ErrorTOUCHFailed of string
    | Message of raw_message

  let to_string = function
    | ResponseOk -> "OK"
    | Heartbeat -> "Heartbeat"
    | ErrorInvalid s -> Printf.sprintf "ErrorInvalid: %s" s
    | ErrorBadTopic s -> Printf.sprintf "ErrorBadTopic: %s" s
    | ErrorBadChannel s -> Printf.sprintf "ErrorBadChannel: %s" s
    | ErrorFINFailed s -> Printf.sprintf "ErrorFINFailed: %s" s
    | ErrorREQFailed s -> Printf.sprintf "ErrorREQFailed: %s" s
    | ErrorTOUCHFailed s -> Printf.sprintf "ErrTouchFailed: %s" s
    | Message m -> Printf.sprintf "Message with ID %s" (MessageID.to_string m.id)
end

type handler_result =
  | HandlerOK
  | HandlerRequeue

type lookup_producer = {
  remote_address: string;
  hostname: string;
  broadcast_address: string;
  tcp_port: int;
  http_port: int;
  version: string;
} [@@deriving yojson { strict = false }]

type lookup_response = {
  channels: string list;
  producers: lookup_producer list;
} [@@deriving yojson { strict = false }]

let producer_addresses lr =
  List.map ~f:(fun p -> Address.host_port p.broadcast_address p.tcp_port) lr.producers

let guard_str f =
  match Result.try_with f with
  | Ok _ as x -> x
  | Error e -> Error (Exn.to_string e)

let lookup_response_from_string s =
  let open Result in
  guard_str (fun () -> Yojson.Safe.from_string s) >>=
  lookup_response_of_yojson

let log_and_return prefix r =
  match r with
  | Ok _ as ok -> return ok
  | Error s as error ->
    Logs_lwt.err (fun l -> l "%s: %s" prefix s) >>= fun () ->
    return error

let query_nsqlookupd ~topic a =
  let host, port = match a with
    | Address.Host h -> h, default_lookupd_port
    | Address.HostPort (h, p) -> (h, p)
  in
  let topic_string = Topic.to_string topic in
  let uri = 
    Uri.make 
      ~scheme:"http" 
      ~host 
      ~port 
      ~path:"lookup"
      ~query:([("topic", [topic_string])])
      ()
  in
  let open Cohttp in
  catch
    begin
      fun () ->
        Cohttp_lwt_unix.Client.get uri >>= fun (resp, body) ->
        match Response.status resp with
        | `OK ->
          Cohttp_lwt.Body.to_string body >>= fun body ->
          log_and_return "Error parsing lookup response" @@ lookup_response_from_string body
        | status ->
          return_error (Printf.sprintf "Expected %s, got %s" (Code.string_of_status `OK) (Code.string_of_status status))
    end
    begin
      fun e ->
        let s = Exn.to_string e in
        log_and_return "Querying lookupd" (Error s)
    end

let bytes_of_pub topic data =
  let buf = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 buf 0 (Int32.of_int_exn @@ Bytes.length data);
  Printf.sprintf "PUB %s\n%s%s" (Topic.to_string topic) (Bytes.to_string buf) (Bytes.to_string data)
  |> Bytes.of_string

(** 
   MPUB <topic_name>\n
   [ 4-byte body size ]
   [ 4-byte num messages ]
   [ 4-byte message #1 size ][ N-byte binary data ]
      ... (repeated <num_messages> times)

   <topic_name> - a valid string (optionally having #ephemeral suffix)
*)
let bytes_of_mpub topic bodies =
  let body_count = List.length bodies in
  let data_size = List.fold_left ~f:(fun a b -> a + Bytes.length b) ~init:0 bodies in
  let buf = Bytes.create (4 + 4 + (4*body_count) + data_size) in
  EndianBytes.BigEndian.set_int32 buf 0 (Int32.of_int_exn @@ data_size);
  EndianBytes.BigEndian.set_int32 buf 4 (Int32.of_int_exn body_count);
  let index = ref 8 in
  List.iter ~f:(
    fun b -> 
      EndianBytes.BigEndian.set_int32 buf !index (Int32.of_int_exn @@ Bytes.length b);
      index := !index + 4;
      Bytes.blit b 0 buf !index (Bytes.length b);
      index := !index + Bytes.length b;
  ) bodies;
  Printf.sprintf "MPUB %s\n%s" (Topic.to_string topic) (Bytes.to_string buf)
  |> Bytes.of_string

(**
   IDENTIFY\n
   [ 4-byte size in bytes ][ N-byte JSON data ]
*)
let bytes_of_identify c =
  let buf = Bytes.create 4 in
  let data = IdentifyConfig.to_yojson c |> Yojson.Safe.to_string in
  let length = String.length data in
  EndianBytes.BigEndian.set_int32 buf 0 (Int32.of_int_exn length);
  Printf.sprintf "IDENTIFY\n%s%s" (Bytes.to_string buf) data
  |> Bytes.of_string

let bytes_of_command = function
  | Magic -> Bytes.of_string "  V2" 
  | IDENTIFY c -> bytes_of_identify c
  | NOP -> Bytes.of_string "NOP\n"
  | RDY i -> Printf.sprintf "RDY %i\n" i |> Bytes.of_string
  | FIN id -> Printf.sprintf "FIN %s\n" (MessageID.to_string id) |> Bytes.of_string
  | TOUCH id -> Printf.sprintf "TOUCH %s\n" (MessageID.to_string id) |> Bytes.of_string
  | SUB (t, c) -> Printf.sprintf "SUB %s %s\n" (Topic.to_string t) (Channel.to_string c) |> Bytes.of_string
  | REQ (id, delay) -> Printf.sprintf "REQ %s %Li\n" (MessageID.to_string id) (Milliseconds.value delay) |> Bytes.of_string
  | PUB (t, data) -> bytes_of_pub t data
  | MPUB (t, data) -> bytes_of_mpub t data
  | CLS -> Bytes.of_string "CLS\n"
  | AUTH secret -> 
    let buf = Bytes.create 4 in
    let length = String.length secret in
    EndianBytes.BigEndian.set_int32 buf 0 (Int32.of_int_exn length);
    Printf.sprintf "AUTH\n%s%s" (Bytes.to_string buf) secret
    |> Bytes.of_string

(* Timeout will only apply if > 0.0 *)
let maybe_timeout ~timeout f =
  if Float.(timeout <= 0.0)
  then f ()
  else Lwt_unix.with_timeout timeout f

let send ~timeout ~conn command =
  let oc = snd conn in
  let data = bytes_of_command command |> Bytes.to_string in
  maybe_timeout ~timeout (fun () -> Lwt_io.write oc data)

let catch_result promise =
  try_bind 
    promise
    (fun x -> return_ok x)
    (fun e -> return_error e)

let catch_result1 f x =
  catch_result (fun () -> f x)

let connect host timeout =
  let (host, port) = match host with
    | Address.Host h          -> (h, default_nsqd_port)
    | Address.HostPort (h, p) -> (h, p)
  in
  Lwt_unix.gethostbyname host >>= fun info ->
  try
    let host = Array.get info.h_addr_list 0 in
    let addr = Unix.ADDR_INET(host, port) in
    maybe_timeout ~timeout (fun () ->
        Lwt_io.open_connection
          ~in_buffer:(Lwt_bytes.create network_buffer_size)
          ~out_buffer:(Lwt_bytes.create network_buffer_size)
          addr
      )
  with
    _ -> fail_with "Host lookup failed"

let frame_from_bytes bytes =
  let frame_type = EndianBytes.BigEndian.get_int32 bytes 0 in
  let to_read = (Bytes.length bytes) - 4 in
  let data = Bytes.sub bytes 4 to_read in
  {frame_type; data}

let read_raw_frame ~timeout (ic, _) =
  Lwt_io.BE.read_int32 ic >>= fun size ->
  let size = Int32.to_int_exn size in
  let bytes = Bytes.create size in
  maybe_timeout ~timeout (fun () -> Lwt_io.read_into_exactly ic bytes 0 size) >>= fun () ->
  return (frame_from_bytes bytes)

let parse_response_body body =
  let body = Bytes.to_string body in
  let open ServerMessage in
  match body with
  | "OK" -> Ok ResponseOk
  | "_heartbeat_" -> Ok Heartbeat
  | _ -> Error (Printf.sprintf "Unknown response: %s" body)

let parse_message_body_exn body =
  let timestamp = EndianBytes.BigEndian.get_int64 body 0 in
  let attempts = EndianBytes.BigEndian.get_uint16 body 8 |> Unsigned.UInt16.of_int in
  let length = Bytes.length body in
  let id = MessageID.of_bytes (Bytes.sub body 10 16) in
  let body = Bytes.sub body 26 (length-26) in
  ServerMessage.Message { timestamp; attempts; id; body; }

let parse_message_body body =
  guard_str @@ fun () -> parse_message_body_exn body

let parse_error_body body =
  match String.split ~on:' ' (Bytes.to_string body) with
  | [code; detail] ->
    begin 
      let open ServerMessage in
      match code with 
      | "E_INVALID" -> Result.return @@ ErrorInvalid detail
      | "E_BAD_TOPIC" -> Result.return @@ ErrorBadTopic detail
      | "E_BAD_CHANNEL" -> Result.return @@ ErrorBadChannel detail
      | "E_FIN_FAILED" -> Result.return @@ ErrorFINFailed detail
      | _ -> Error (Printf.sprintf "Unknown error code: %s. %s" code detail)
    end
  | _ -> Error (Printf.sprintf "Malformed error code: %s" (Bytes.to_string body))

let server_message_of_frame raw =
  match (FrameType.of_int32 raw.frame_type) with
  | FrameResponse -> parse_response_body raw.data
  | FrameMessage -> parse_message_body raw.data
  | FrameError -> parse_error_body raw.data
  | FrameUnknown i -> Result.Error (Printf.sprintf "Unknown frame type: %li" i)

let send_expect_ok ~read_timeout ~write_timeout ~conn cmd =
  send ~timeout:write_timeout ~conn cmd >>= fun () ->
  read_raw_frame ~timeout:read_timeout conn >>= fun raw ->
  match server_message_of_frame raw with
  | Ok ResponseOk -> return_unit
  | Ok sm -> fail_with @@ Printf.sprintf "Expected OK, got %s" (ServerMessage.to_string sm)
  | Error e -> fail_with (Printf.sprintf "Expected OK, got %s" e)

let subscribe ~read_timeout ~write_timeout ~conn topic channel =
  send_expect_ok ~read_timeout ~write_timeout ~conn (SUB (topic, channel))

let identify ~read_timeout ~write_timeout ~conn ic =
  send_expect_ok ~read_timeout ~write_timeout ~conn (IDENTIFY ic)

let handle_message handler msg max_attempts =
  let requeue_delay attempts =
    let d = Milliseconds.value default_requeue_delay in
    let attempts = Int64.of_int_exn attempts in
    Milliseconds.of_int64 Int64.(d * attempts)
  in
  catch_result1 handler msg.body >>=
  begin
    function
    | Ok r -> return r
    | Error e ->
      Logs_lwt.err (fun l -> l "Handler error: %s" (Exn.to_string e)) >>= fun () ->
      return HandlerRequeue
  end
  >>= function
  | HandlerOK -> return (FIN msg.id)
  | HandlerRequeue -> 
    let attempts = Unsigned.UInt16.to_int msg.attempts in
    if attempts >= max_attempts
    then 
      (Logs_lwt.warn (fun l -> l "Discarding message %s as reached max attempts, %d" (MessageID.to_string msg.id) attempts) >>= fun () ->
       return (FIN msg.id))
    else
      let delay = requeue_delay attempts in
      return (REQ (msg.id, delay))

let handle_server_message server_message handler max_attempts =
  let warn_return_none name msg = 
    Logs_lwt.warn (fun l -> l "%s: %s" name msg) >>= fun () -> return_none 
  in
  let open ServerMessage in
  match server_message with
  | ResponseOk -> return_none
  | Heartbeat -> Logs_lwt.debug (fun l -> l "Received heartbeat") >>= fun () -> return_some NOP
  | ErrorInvalid s ->  warn_return_none "ErrorInvalid" s
  | ErrorBadTopic s -> warn_return_none "ErrorBadTopic" s 
  | ErrorBadChannel s -> warn_return_none "ErrorBadChannel" s 
  | ErrorFINFailed s -> warn_return_none "ErrorFINFailed" s 
  | ErrorREQFailed s -> warn_return_none "ErrorREQFailed" s 
  | ErrorTOUCHFailed s -> warn_return_none "ErrorTOUCHFailed" s 
  | Message msg -> handle_message handler msg max_attempts >>= fun cmd -> return_some cmd

module Consumer = struct

  module VInt = struct 
    let validate_positive value name c =
      if Int.is_positive value
      then Ok c
      else Error (Printf.sprintf "%s must be greater than 0" name)

    let validate_between ~low ~high value name c =
      if Int.between ~low ~high value
      then Ok c
      else Error (Printf.sprintf "%s must be between %s and %s, got %s" name (Int.to_string low) (Int.to_string high) (Int.to_string value))
  end

  module VFloat = struct 
    let validate_positive value name c =
      if Float.is_positive value
      then Ok c
      else Error (Printf.sprintf "%s must be greater than 0" name)

    let validate_between ~low ~high value name c =
      if Float.between ~low ~high value
      then Ok c
      else Error (Printf.sprintf "%s must be between %s and %s, got %s" name (Float.to_string low) (Float.to_string high) (Float.to_string value))
  end


  let validate_not_blank s name c =
    if String.is_empty s
    then Error (Printf.sprintf "%s can't be blank" name)
    else Ok c

  type config = {
    (* The total number of messages allowed in flight for all connections of this consumer *)
    max_in_flight : int;
    max_attempts : int;
    backoff_multiplier : float;

    (* network timeouts in seconds *)
    dial_timeout : float;
    read_timeout : float;
    write_timeout : float;
    lookupd_poll_interval : float;
    lookupd_poll_jitter : float;
    max_requeue_delay : float;
    default_requeue_delay : float;

    (* The fields below are used in IdentifyConfig.t *)
    heartbeat_interval : float;
    client_id : string;
    hostname : string;
    user_agent : string;
    output_buffer_size : int; (* buffer size on bytes the server should use  *)
    output_buffer_timeout : float;
    sample_rate : int; (* Between 0 and 99 *)
  }

  let validate_config c =
    let open Result in
    VInt.validate_positive c.max_in_flight "max_in_flight" c
    >>= VInt.validate_between ~low:0 ~high:65535 c.max_attempts "max_attempts"
    >>= VFloat.validate_positive c.backoff_multiplier "backoff_multiplier"
    >>= VFloat.validate_between ~low:0.0 ~high:300.0 c.dial_timeout "dial_timeout"
    >>= VFloat.validate_between ~low:0.1 ~high:300.0 c.read_timeout "read_timeout"
    >>= VFloat.validate_between ~low:0.1 ~high:300.0 c.write_timeout "write_timeout"
    >>= VFloat.validate_between ~low:0.1 ~high:300.0 c.lookupd_poll_interval "lookupd_poll_interval"
    >>= VFloat.validate_between ~low:0.0 ~high:1.0 c.lookupd_poll_jitter "lookupd_poll_jitter"
    >>= VFloat.validate_between ~low:0.0 ~high:(3600.) c.default_requeue_delay "default_requeue_delay"
    >>= VFloat.validate_between ~low:0.0 ~high:(3600.) c.max_requeue_delay "max_requeue_delay"
    >>= VFloat.validate_between ~low:0.01 ~high:300.0 c.output_buffer_timeout "output_buffer_timeout"
    >>= VInt.validate_between ~low:64 ~high:(5 * 1025 * 1000) c.output_buffer_size "output_buffer_size"
    >>= VInt.validate_between ~low:0 ~high:99 c.sample_rate "sample_rate"
    >>= VFloat.validate_between ~low:1.0 ~high:300.0 c.heartbeat_interval "heartbeat_interval"
    >>= validate_not_blank c.client_id "client_id"
    >>= validate_not_blank c.hostname "hostname"
    >>= validate_not_blank c.user_agent "user_agent"

  let create_config 
      ?(max_in_flight=1) 
      ?(max_attempts=5)
      ?(backoff_multiplier=0.5)
      ?(dial_timeout=1.0)
      ?(read_timeout = 60.0)
      ?(write_timeout = 1.0)
      ?(lookupd_poll_interval = 60.0)
      ?(lookupd_poll_jitter = 0.3)
      ?(heartbeat_interval = 60.0)
      ?(max_requeue_delay = (15.0 *. 60.0))
      ?(default_requeue_delay = 90.0)
      ?(client_id = (Unix.gethostname ()))
      ?(hostname = (Unix.gethostname ()))
      ?(user_agent = "nsq-ocaml/0.2")
      ?(output_buffer_size = 16 * 1024)
      ?(output_buffer_timeout = 0.25)
      ?(sample_rate = 0)
      ()
    = 
    validate_config {
      max_in_flight;
      max_attempts;
      backoff_multiplier;
      dial_timeout;
      read_timeout;
      write_timeout;
      lookupd_poll_interval;
      lookupd_poll_jitter;
      max_requeue_delay;
      default_requeue_delay;
      heartbeat_interval;
      client_id;
      hostname;
      user_agent;
      output_buffer_size;
      output_buffer_timeout;
      sample_rate;
    }

  let extract_identify_config c = 
    let to_ms f = Float.to_int (f *. 1000.0) in
    {
      IdentifyConfig.heartbeat_interval = to_ms c.heartbeat_interval;
      IdentifyConfig.client_id = c.client_id;
      IdentifyConfig.hostname = c.hostname;
      IdentifyConfig.user_agent = c.user_agent;
      IdentifyConfig.output_buffer_size = c.output_buffer_size;
      IdentifyConfig.output_buffer_timeout = to_ms c.output_buffer_timeout;
      IdentifyConfig.sample_rate = c.sample_rate;
    }

  type breaker_position =
    | Closed
    | HalfOpen
    | Open

  type breaker_state = {
    position : breaker_position;
    error_count : int;
  }

  let backoff_duration backoff_multiplier error_count =
    let bo = (backoff_multiplier *. (Float.of_int error_count)) in
    Float.min bo max_backoff_seconds

  type mode =
    | ModeNsqd
    | ModeLookupd

  type t = {
    addresses : Address.t list;
    (* The number of open NSQD connections *)
    mutable nsqd_connections : int;
    topic : Topic.t;
    channel : Channel.t;
    handler : (bytes -> handler_result Lwt.t);
    config : config;
    mode : mode;
    log_prefix : string;
  }

  let create ?(mode=ModeNsqd) ?config addresses topic channel handler =
    let config = match config with
      (* We're assuming create_config with defaults always returns valid config *)
      | None -> create_config () |> Result.ok_or_failwith
      | Some c -> c
    in
    { 
      addresses;
      nsqd_connections = 0;
      topic; 
      channel; 
      handler;
      config;
      mode;
      log_prefix = Printf.sprintf "%s/%s" (Topic.to_string topic) (Channel.to_string channel);
    }

  let rdy_per_connection c =
    if c.nsqd_connections = 0 || (c.config.max_in_flight < c.nsqd_connections)
    then 1
    else c.config.max_in_flight / c.nsqd_connections

  let do_after duration f =
    Logs_lwt.debug (fun l -> l "Sleeping for %f seconds" duration) >>= fun () ->
    Lwt_unix.sleep duration >>= f

  type loop_message =
    | RawFrame of raw_frame
    | Command of command
    | TrialBreaker
    | ConnectionError of exn
    | RecalcRDY

  let rec read_loop ~timeout conn mbox =
    let put_async m = Lwt.async @@ fun () -> Lwt_mvar.put mbox m in
    catch_result1 (read_raw_frame ~timeout) conn >>= function
    | Ok raw ->
      put_async @@ RawFrame raw;
      read_loop ~timeout conn mbox
    | Error e ->
      put_async @@ ConnectionError e;
      return_unit

  let open_breaker c conn mbox bs =
    (* Send RDY 0 and send retry trial command after a delay  *)
    Logs_lwt.debug (fun l -> l "Breaker open, sending RDY 0") >>= fun () ->
    send ~timeout:c.config.dial_timeout ~conn (RDY 0) >|= fun () ->
    let bs = { error_count = bs.error_count + 1; position = Open } in
    let duration = backoff_duration c.config.backoff_multiplier bs.error_count in
    async (fun () -> do_after duration (fun () -> Lwt_mvar.put mbox TrialBreaker));
    bs

  let update_breaker_state c conn open_breaker cmd bs =
    match cmd with
    | NOP -> return bs (* Receiving a NOP should not alter our state  *)
    | _ ->
      let is_error = match cmd with
        | REQ _ -> true
        | _ -> false
      in
      match is_error, bs.position with
      | true, Closed ->
        open_breaker bs
      | true, Open -> return bs
      | true, HalfOpen ->
        (* Failed test *)
        open_breaker bs
      | false, Closed -> return bs
      | false, Open -> return bs
      | false, HalfOpen ->
        (* Passed test  *)
        let rdy = rdy_per_connection c in
        Logs_lwt.debug (fun l -> l "%s Trial passed, sending RDY %d" c.log_prefix rdy) >>= fun () ->
        send ~timeout:c.config.write_timeout ~conn (RDY rdy) >>= fun () ->
        return { position = Closed; error_count = 0; }

  let consume c conn mbox =
    let open_breaker = open_breaker c conn mbox in
    let update_breaker_state = update_breaker_state c conn open_breaker in
    let send = send ~timeout:c.config.write_timeout ~conn in
    let rec mbox_loop bs =
      Lwt_mvar.take mbox >>= function
      | RawFrame raw ->
        begin 
          match server_message_of_frame raw with
          | Ok server_message ->
            Lwt.async 
              begin
                fun () -> 
                  handle_server_message server_message c.handler c.config.max_attempts >>= function
                  | Some c -> Lwt_mvar.put mbox (Command c)
                  | None -> return_unit
              end;
            mbox_loop bs
          | Error s -> 
            Logs_lwt.err (fun l -> l "%s Error parsing response: %s" c.log_prefix s) >>= fun () ->
            mbox_loop bs
        end
      | Command cmd ->
        send cmd >>= fun () ->
        update_breaker_state cmd bs >>= mbox_loop
      | TrialBreaker ->
        Logs_lwt.debug (fun l -> l "%s Breaker trial, sending RDY 1 (Error count: %i)" c.log_prefix  bs.error_count) >>= fun () ->
        let bs = { bs with position = HalfOpen } in
        send (RDY 1) >>=  fun () ->
        mbox_loop bs
      | ConnectionError e ->
        fail e 
      | RecalcRDY ->
        (* Only recalc and send if breaker is closed  *)
        match bs.position with
        | Open -> mbox_loop bs
        | HalfOpen -> mbox_loop bs
        | Closed -> 
          let rdy = rdy_per_connection c in
          Logs_lwt.debug (fun l -> l "%s Sending recalculated RDY %d" c.log_prefix rdy) >>= fun () ->
          send (RDY rdy) >>= fun () ->
          mbox_loop bs
    in
    send Magic >>= fun () ->
    let ic = extract_identify_config c.config in
    identify ~read_timeout:c.config.read_timeout ~write_timeout:c.config.write_timeout ~conn ic >>= fun () ->
    subscribe ~read_timeout:c.config.read_timeout ~write_timeout:c.config.write_timeout ~conn c.topic c.channel >>= fun () ->
    (* Start cautiously by sending RDY 1 *) 
    Logs_lwt.debug (fun l -> l "%s Sending initial RDY 1" c.log_prefix) >>= fun () ->
    send (RDY 1) >>= fun () ->
    (* Start background reader *)
    Lwt.async (fun () -> read_loop ~timeout:c.config.read_timeout conn mbox);
    let initial_state = { position = HalfOpen; error_count = 0; } in
    mbox_loop initial_state

  let rec main_loop c address mbox =
    catch_result (fun () -> connect address c.config.dial_timeout) >>= function
    | Ok conn ->
      let handle_ex e = 
        Lwt.join [
          Logs_lwt.err (fun l -> l "Consumer connection error: %s" (Exn.to_string e));
          (Lwt_io.close (fst conn));
          (Lwt_io.close (snd conn));
          (Lwt_unix.sleep default_backoff_seconds)]
      in
      c.nsqd_connections <- c.nsqd_connections + 1;
      Logs_lwt.debug ( fun l -> l "%s %d connections" c.log_prefix c.nsqd_connections) >>= fun () ->
      catch (fun () -> consume c conn mbox) handle_ex >>= fun () ->
      (* If we get here it means that something failed and we need to reconnect *)
      c.nsqd_connections <- (max 0 (c.nsqd_connections - 1));
      Logs_lwt.debug ( fun l -> l "%s %d connections" c.log_prefix c.nsqd_connections) >>= fun () ->
      main_loop c address mbox
    | Error e ->
      Logs_lwt.err (fun l -> l "%s Connecting to consumer '%s': %s" c.log_prefix (Address.to_string address) (Exn.to_string e)) >>= fun () ->
      Lwt_unix.sleep default_backoff_seconds >>= fun () ->
      main_loop c address mbox

  let async_exception_hook e =
    Logs.err (fun l -> l "Async exception: %s" (Exn.to_string e))

  let start_nsqd_consumer c address =
    let mbox = Lwt_mvar.create_empty () in
    async 
      (** 
         Start an async thread to update RDY count occasionaly.
         The number of open connections can change as we add new consumers due to lookupd
         discovering producers or we have connection failures. Each time a new connection 
         is opened or closed the consumer.nsqd_connections field is updated.
         We therefore need to occasionaly update our RDY count as this may have changed so that
         it is spread evenly across connections.
      *)
      begin
        fun () ->
          let jitter = Random.float (recalculate_rdy_interval /. 10.0) in
          let interval = recalculate_rdy_interval +. jitter in
          let rec loop () =
            Lwt_unix.sleep interval >>= fun () ->
            Logs_lwt.debug ( fun l -> l "%s recalculating RDY" c.log_prefix) >>= fun () ->
            Lwt_mvar.put mbox RecalcRDY >>= loop
          in
          loop ()
      end;
    main_loop c address mbox

  let start_polling_lookupd c lookup_addresses =
    let poll_interval = Float.((1.0 + (Random.float c.config.lookupd_poll_jitter)) * c.config.lookupd_poll_interval) in
    let rec check_for_producers running =
      Logs_lwt.debug (fun l -> l "Querying %d lookupd hosts" (List.length lookup_addresses)) >>= fun () ->
      Lwt_list.map_p (query_nsqlookupd ~topic:c.topic) lookup_addresses >>= fun results ->
      let discovered_producers =
        List.filter_map ~f:Result.ok results
        |> List.map ~f:producer_addresses
        |> List.join
        |> Set.of_list (module Address)
      in
      let new_producers = Set.diff discovered_producers running in
      Logs_lwt.debug (fun l -> l "Found %d new producers" (Set.length new_producers)) >>= fun () ->
      Set.iter 
        ~f:(fun a -> 
            async (fun () -> 
                Logs_lwt.debug (fun l -> l "Starting consumer for: %s" (Address.to_string a)) >>= fun () ->
                start_nsqd_consumer c a)
          ) 
        new_producers;
      let running = Set.union running new_producers in
      Lwt_unix.sleep poll_interval >>= fun () ->
      check_for_producers running
    in
    check_for_producers (Set.empty (module Address))

  let run c =
    Lwt.async_exception_hook := async_exception_hook;
    match c.mode with
    | ModeLookupd ->
      Logs_lwt.debug (fun l -> l "Starting lookupd poll") >>= fun () ->
      start_polling_lookupd c c.addresses
    | ModeNsqd ->
      let consumers = List.map ~f:(fun a -> start_nsqd_consumer c a) c.addresses in
      Lwt.join consumers

end

module Producer = struct
  type connection = {
    conn : (Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel);
    last_send : float ref
  }

  type t = { 
    address : Address.t;
    pool : connection Lwt_pool.t;
  }

  let default_pool_size = 5
  let default_dial_timeout = 15.0
  let default_write_timeout = 15.0
  let default_read_timeout = 15.0

  (** Throw away connections that are idle for this long
       Note that NSQ expects hearbeats to be answered every 30 seconds
       and if two are missed it closes the connection.
  *)
  let ttl_seconds = 50.0 

  let create_pool address size =
    let validate c =
      let now = Unix.time () in
      let diff = now -. !(c.last_send) in
      return (Float.(diff < ttl_seconds))
    in
    (* Always return false so that we throw away connections where we encountered an error *)
    let check _ is_ok = is_ok false in
    let dispose c =
      Logs_lwt.warn (fun l -> l "Error publishing, closing connection") >>= fun () ->
      Lwt.join [(Lwt_io.close (fst c.conn));(Lwt_io.close (snd c.conn))]
    in
    Lwt_pool.create size ~validate ~check ~dispose
      begin
        fun () -> 
          connect address default_dial_timeout >>= fun conn ->
          send ~timeout:default_write_timeout ~conn Magic >>= fun () ->
          let last_send = ref (Unix.time ()) in
          return { conn; last_send }
      end

  let create ?(pool_size=default_pool_size) address = 
    if pool_size <= 0
    then Error "Pool size must be >= 1"
    else Ok { address; pool = create_pool address pool_size }

  let publish_cmd t cmd =
    let with_conn c =
      let rec read_until_ok () =
        read_raw_frame ~timeout:default_read_timeout c.conn >>= fun frame ->
        match server_message_of_frame frame with
        | Ok ResponseOk -> return_ok ()
        | Ok Heartbeat -> 
          send ~timeout:default_write_timeout ~conn:c.conn  NOP >>= fun () ->
          c.last_send := Unix.time ();
          read_until_ok ()
        | Ok _ -> return_error "Expected OK or Heartbeat, got another message"
        | Error e -> return_error (Printf.sprintf "Received error: %s" e)
      in
      send ~timeout:default_write_timeout ~conn:c.conn cmd >>= fun () ->
      c.last_send := Unix.time ();
      read_until_ok ()
    in
    let try_publish () = Lwt_pool.use t.pool with_conn in
    let handle_ex e =
      let message = Printf.sprintf "Publishing to `%s`: %s" (Address.to_string t.address) (Exn.to_string e) in
      return_error message
    in
    catch try_publish handle_ex

  let publish t topic message =
    let cmd = (PUB (topic, message)) in
    publish_cmd t cmd

  let publish_multi t topic messages =
    let cmd = (MPUB (topic, messages)) in
    publish_cmd t cmd

end
