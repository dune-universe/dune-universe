open Base
open Lwt

let client_version = "0.4"
let ephemeral_suffix = "#ephemeral"
let default_backoff_seconds = 1.0
let max_backoff_seconds = 3600.0
let default_nsqd_port = 4150
let default_lookupd_port = 4161
let network_buffer_size = 16 * 1024
let recalculate_rdy_interval = 60.0

module Seconds : sig
  type t

  val of_float : float -> t
  val value : t -> float
end = struct
  type t = float [@@deriving yojson]

  let of_float f = f
  let value s = s
end

module Milliseconds : sig
  type t [@@deriving yojson]

  val of_int64 : int64 -> t
  val value : t -> int64
  val of_seconds : Seconds.t -> t
end = struct
  type t = int64 [@@deriving yojson]

  let of_int64 i = i
  let value i = i

  let of_seconds s =
    (Seconds.value s) *. 1000.0 |> Int64.of_float |> of_int64
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
    heartbeat_interval : Milliseconds.t;
    client_id : string;
    hostname : string;
    user_agent : string;
    output_buffer_size : int; (* buffer size on bytes the server should use  *)
    output_buffer_timeout : Milliseconds.t;
    sample_rate : int;
  } [@@deriving yojson { strict = false }]
end

type command =
  | IDENTIFY of IdentifyConfig.t
  | MAGIC
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

let try_with_string f =
  match Result.try_with f with
  | Ok _ as x -> x
  | Error e -> Error (Exn.to_string e)

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

  let parse_response_body body =
    let body = Bytes.to_string body in
    match body with
    | "OK" -> Ok ResponseOk
    | "_heartbeat_" -> Ok Heartbeat
    | _ -> Error (Printf.sprintf "Unknown response: %s" body)

  let parse_message_body_exn body =
    let timestamp = EndianBytes.BigEndian.get_int64 body 0 in
    let attempts = EndianBytes.BigEndian.get_uint16 body 8 |> Unsigned.UInt16.of_int in
    let length = Bytes.length body in
    let id = MessageID.of_bytes (Bytes.sub ~pos:10 ~len:16 body) in
    let body = Bytes.sub ~pos:26 ~len:(length-26) body in
    Message { timestamp; attempts; id; body; }

  let parse_message_body body =
    try_with_string @@ fun () -> parse_message_body_exn body

  let parse_error_body body =
    match String.split ~on:' ' (Bytes.to_string body) with
    | [code; detail] ->
      begin 
        match code with 
        | "E_INVALID" -> Result.return @@ ErrorInvalid detail
        | "E_BAD_TOPIC" -> Result.return @@ ErrorBadTopic detail
        | "E_BAD_CHANNEL" -> Result.return @@ ErrorBadChannel detail
        | "E_FIN_FAILED" -> Result.return @@ ErrorFINFailed detail
        | _ -> Error (Printf.sprintf "Unknown error code: %s. %s" code detail)
      end
    | _ -> Error (Printf.sprintf "Malformed error code: %s" (Bytes.to_string body))

  let of_raw_frame raw =
    match (FrameType.of_int32 raw.frame_type) with
    | FrameResponse -> parse_response_body raw.data
    | FrameMessage -> parse_message_body raw.data
    | FrameError -> parse_error_body raw.data
    | FrameUnknown i -> Result.Error (Printf.sprintf "Unknown frame type: %li" i)

end

module Lookup = struct
  type producer = {
    remote_address: string;
    hostname: string;
    broadcast_address: string;
    tcp_port: int;
    http_port: int;
    version: string;
  } [@@deriving yojson { strict = false }]

  type response = {
    channels: string list;
    producers: producer list;
  } [@@deriving yojson { strict = false }]

  let response_of_string s =
    let open Result in
    try_with_string (fun () -> Yojson.Safe.from_string s) >>=
    response_of_yojson

  let producer_addresses lr =
    List.map ~f:(fun p -> Address.host_port p.broadcast_address p.tcp_port) lr.producers
end

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
          log_and_return "Error parsing lookup response" @@ Lookup.response_of_string body
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
      Bytes.blit ~src:b ~src_pos:0 ~dst:buf ~dst_pos:!index ~len:(Bytes.length b);
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
  | MAGIC -> Bytes.of_string "  V2"
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
  let timeout = Seconds.value timeout in
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
    (fun e -> return_error (Exn.to_string e))

let catch_result1 f x =
  catch_result (fun () -> f x)

let connect host timeout =
  let (host, port) = match host with
    | Address.Host h          -> (h, default_nsqd_port)
    | Address.HostPort (h, p) -> (h, p)
  in
  Lwt_unix.gethostbyname host >>= fun info ->
  let host = Array.random_element_exn info.h_addr_list in
  let addr = Unix.ADDR_INET(host, port) in
  maybe_timeout ~timeout (fun () ->
      Lwt_io.open_connection
        ~in_buffer:(Lwt_bytes.create network_buffer_size)
        ~out_buffer:(Lwt_bytes.create network_buffer_size)
        addr
    )

let frame_from_bytes bytes =
  let frame_type = EndianBytes.BigEndian.get_int32 bytes 0 in
  let to_read = (Bytes.length bytes) - 4 in
  let data = Bytes.sub ~pos:4 ~len:to_read bytes in
  {frame_type; data}

let read_raw_frame ~timeout (ic, _) =
  Lwt_io.BE.read_int32 ic >>= fun size ->
  let size = Int32.to_int_exn size in
  let bytes = Bytes.create size in
  maybe_timeout ~timeout (fun () -> Lwt_io.read_into_exactly ic bytes 0 size) >>= fun () ->
  return (frame_from_bytes bytes)

let send_expect_ok ~read_timeout ~write_timeout ~conn cmd =
  send ~timeout:write_timeout ~conn cmd >>= fun () ->
  read_raw_frame ~timeout:read_timeout conn >>= fun raw ->
  match ServerMessage.of_raw_frame raw with
  | Ok ResponseOk -> return_unit
  | Ok sm -> fail_with @@ Printf.sprintf "Expected OK, got %s" (ServerMessage.to_string sm)
  | Error e -> fail_with (Printf.sprintf "Expected OK, got %s" e)

let subscribe ~read_timeout ~write_timeout ~conn topic channel =
  send_expect_ok ~read_timeout ~write_timeout ~conn (SUB (topic, channel))

let identify ~read_timeout ~write_timeout ~conn ic =
  send_expect_ok ~read_timeout ~write_timeout ~conn (IDENTIFY ic)


module TestHelper = struct
  let result_printer i r = 
    let r = match r with
      | Ok _ -> "Ok"
      | Error s -> s
    in
    Stdio.print_endline (Printf.sprintf "%d: %s" i r)
end

module VInt = struct 
  let validate_positive value name c =
    if Int.is_positive value
    then Ok c
    else Error (Printf.sprintf "%s must be greater than 0" name)

  let validate_between ~low ~high value name c =
    if Int.between ~low ~high value
    then Ok c
    else Error (Printf.sprintf "%s must be between %d and %d, got %d" name low high value)

  let result_printer i r = 
    let r = match r with
      | Ok _ -> "Ok"
      | Error s -> s
    in
    Stdio.print_endline (Printf.sprintf "%d: %s" i r)

  let%expect_test "validate_positive" =
    [
      ( 1, "Field", () ) ;
      ( 0, "Field", () ) ;
      ( -1, "Field", () ) ;
    ]
    |> List.map ~f:(fun (v, name, c) -> validate_positive v name c)
    |> List.iteri ~f:result_printer;
    [%expect{|
        0: Ok
        1: Field must be greater than 0
        2: Field must be greater than 0
      |}]

  let%expect_test "validate_between" =
    [
      ( 0, 0, 1, "Field", () );
      ( 0, 10, 1, "Field", () );
      ( 0, 10, 11, "Field", () );
    ]
    |> List.map ~f:(fun (low, high, v, name, c) -> validate_between ~low ~high v name c)
    |> List.iteri ~f:result_printer;
    [%expect{|
        0: Field must be between 0 and 0, got 1
        1: Ok
        2: Field must be between 0 and 10, got 11
      |}]

end

module VFloat = struct 
  let validate_positive value name c =
    if Float.is_positive value
    then Ok c
    else Error (Printf.sprintf "%s must be greater than 0" name)

  let validate_between ~low ~high value name c =
    if Float.between ~low ~high value
    then Ok c
    else Error (Printf.sprintf "%s must be between %f and %f, got %f" name low high value)

  let%expect_test "validate_positive" =
    [
      ( 1.0, "Field", () ) ;
      ( 0.0, "Field", () ) ;
      ( -1.0, "Field", () ) ;
    ]
    |> List.map ~f:(fun (v, name, c) -> validate_positive v name c)
    |> List.iteri ~f:TestHelper.result_printer;
    [%expect{|
        0: Ok
        1: Field must be greater than 0
        2: Field must be greater than 0
      |}]

  let%expect_test "validate_between" =
    [
      ( 0.0, 0.0, 1.0, "Field", () );
      ( 0.0, 10.0, 1.0, "Field", () );
      ( 0.0, 10.0, 11.0, "Field", () );
    ]
    |> List.map ~f:(fun (low, high, v, name, c) -> validate_between ~low ~high v name c)
    |> List.iteri ~f:TestHelper.result_printer;
    [%expect{|
        0: Field must be between 0.000000 and 0.000000, got 1.000000
        1: Ok
        2: Field must be between 0.000000 and 10.000000, got 11.000000
      |}]

end

let validate_not_blank s name c =
  if String.is_empty s
  then Error (Printf.sprintf "%s can't be blank" name)
  else Ok c

let%expect_test "validate_not_blank" =
  [
    ( "", "Field", () );
    ( "X", "Field", () );
  ]
  |> List.map ~f:(fun (v, name, c) -> validate_not_blank v name c)
  |> List.iteri ~f:TestHelper.result_printer;
  [%expect{|
        0: Field can't be blank
        1: Ok
      |}]

module Consumer = struct
  module Config = struct
    type t = {
      (* The total number of messages allowed in flight for all connections of this consumer *)
      max_in_flight : int;
      max_attempts : int;
      backoff_multiplier : float;
      error_threshold : int; (* After how many errors do we send RDY 0 and back off *)

      (* network timeouts in seconds *)
      dial_timeout : Seconds.t;
      read_timeout : Seconds.t;
      write_timeout : Seconds.t;
      lookupd_poll_interval : Seconds.t;
      lookupd_poll_jitter : float;
      max_requeue_delay : Seconds.t;
      default_requeue_delay : Seconds.t;

      (* The fields below are used in IdentifyConfig.t *)
      heartbeat_interval : Seconds.t;
      client_id : string;
      hostname : string;
      user_agent : string;
      output_buffer_size : int; (* buffer size on bytes the server should use  *)
      output_buffer_timeout : Seconds.t;
      sample_rate : int; (* Between 0 and 99 *)
    }

    let validate c =
      let open Result in
      VInt.validate_positive c.max_in_flight "max_in_flight" c
      >>= VInt.validate_between ~low:0 ~high:65535 c.max_attempts "max_attempts"
      >>= VFloat.validate_positive c.backoff_multiplier "backoff_multiplier"
      >>= VFloat.validate_between ~low:0.0 ~high:300.0 (Seconds.value c.dial_timeout) "dial_timeout"
      >>= VFloat.validate_between ~low:0.1 ~high:300.0 (Seconds.value c.read_timeout) "read_timeout"
      >>= VFloat.validate_between ~low:0.1 ~high:300.0 (Seconds.value c.write_timeout) "write_timeout"
      >>= VFloat.validate_between ~low:0.1 ~high:300.0 (Seconds.value c.lookupd_poll_interval) "lookupd_poll_interval"
      >>= VFloat.validate_between ~low:0.0 ~high:1.0 c.lookupd_poll_jitter "lookupd_poll_jitter"
      >>= VFloat.validate_between ~low:0.0 ~high:(3600.) (Seconds.value c.default_requeue_delay) "default_requeue_delay"
      >>= VFloat.validate_between ~low:0.0 ~high:(3600.) (Seconds.value c.max_requeue_delay) "max_requeue_delay"
      >>= VFloat.validate_between ~low:0.01 ~high:300.0 (Seconds.value c.output_buffer_timeout) "output_buffer_timeout"
      >>= VInt.validate_between ~low:64 ~high:(5 * 1025 * 1000) c.output_buffer_size "output_buffer_size"
      >>= VInt.validate_between ~low:0 ~high:99 c.sample_rate "sample_rate"
      >>= VFloat.validate_between ~low:1.0 ~high:300.0 (Seconds.value c.heartbeat_interval) "heartbeat_interval"
      >>= validate_not_blank c.client_id "client_id"
      >>= validate_not_blank c.hostname "hostname"
      >>= validate_not_blank c.user_agent "user_agent"

    let create 
        ?(max_in_flight = 1)
        ?(max_attempts = 5)
        ?(backoff_multiplier = 0.5)
        ?(error_threshold = 1)
        ?(dial_timeout = Seconds.of_float 1.0)
        ?(read_timeout = Seconds.of_float 60.0)
        ?(write_timeout = Seconds.of_float 1.0)
        ?(lookupd_poll_interval = Seconds.of_float 60.0)
        ?(lookupd_poll_jitter = 0.3)
        ?(heartbeat_interval = Seconds.of_float 60.0)
        ?(max_requeue_delay = Seconds.of_float (15.0 *. 60.0))
        ?(default_requeue_delay = Seconds.of_float 90.0)
        ?(client_id = (Unix.gethostname ()))
        ?(hostname = (Unix.gethostname ()))
        ?(user_agent = Printf.sprintf "nsq-ocaml/%s" client_version)
        ?(output_buffer_size = 16 * 1024)
        ?(output_buffer_timeout = Seconds.of_float 0.25)
        ?(sample_rate = 0)
        ()
      = 
      validate {
        max_in_flight;
        max_attempts;
        backoff_multiplier;
        error_threshold;
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

    let to_identity_config c = 
      {
        IdentifyConfig.heartbeat_interval = (Milliseconds.of_seconds c.heartbeat_interval);
        IdentifyConfig.client_id = c.client_id;
        IdentifyConfig.hostname = c.hostname;
        IdentifyConfig.user_agent = c.user_agent;
        IdentifyConfig.output_buffer_size = c.output_buffer_size;
        IdentifyConfig.output_buffer_timeout = (Milliseconds.of_seconds c.output_buffer_timeout);
        IdentifyConfig.sample_rate = c.sample_rate;
      }

  end

  type mode =
    | ModeNsqd
    | ModeLookupd

  type handler_result =
    | HandlerOK
    | HandlerRequeue

  type handler_func = bytes -> handler_result Lwt.t

  type t = {
    addresses : Address.t list;
    (* The number of open NSQD connections *)
    mutable open_connections : int;
    topic : Topic.t;
    channel : Channel.t;
    handler : handler_func;
    config : Config.t;
    mode : mode;
    log_prefix : string;
  }

  type breaker_position =
    | Closed
    | HalfOpen
    | Open

  type breaker_state = {
    position : breaker_position;
    error_count : int;
  }

  let backoff_duration ~multiplier ~error_count =
    let bo = (multiplier *. (Float.of_int error_count)) in
    Float.min bo max_backoff_seconds
    |> Seconds.of_float

  let%expect_test "backoff_duration" =
    let test_cases = [
      ( 1.0, 1 ) ;
      ( 1.0, 2 );
      ( 2.0, 4000);
    ]
    in
    List.iteri ~f:(fun i (multiplier, error_count) ->
        let r = backoff_duration ~multiplier ~error_count in
        Stdio.print_endline (Printf.sprintf "%d %f" i (Seconds.value r))
      ) test_cases;
    [%expect{|
      0 1.000000
      1 2.000000
      2 3600.000000 |}]

  let create ?(mode=ModeNsqd) ?config addresses topic channel handler =
    let config = match config with
      (* We're assuming create_config with defaults always returns valid config *)
      | None -> Config.create () |> Result.ok_or_failwith
      | Some c -> c
    in
    { 
      addresses;
      open_connections = 0;
      topic; 
      channel; 
      handler;
      config;
      mode;
      log_prefix = Printf.sprintf "%s/%s" (Topic.to_string topic) (Channel.to_string channel);
    }

  let rdy_per_connection c =
    if c.open_connections = 0 || (c.config.max_in_flight < c.open_connections)
    then 1
    else c.config.max_in_flight / c.open_connections

  let do_after_async ~duration f =
    async
      begin fun () ->
        Logs_lwt.debug (fun l -> l "Sleeping for %f seconds" (Seconds.value duration)) >>= fun () ->
        Lwt_unix.sleep (Seconds.value duration) >>= f
      end

  type loop_message =
    | RawFrame of raw_frame
    | Command of command
    | TrialBreaker
    | ConnectionError of string
    | RecalcRDY

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
      | Error s ->
        Logs_lwt.err (fun l -> l "Handler error: %s" s) >>= fun () ->
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

  let rec read_loop ~timeout conn mbox =
    let put_async m = async @@ fun () -> Lwt_mvar.put mbox m in
    catch_result1 (read_raw_frame ~timeout) conn >>= function
    | Ok raw ->
      put_async @@ RawFrame raw;
      read_loop ~timeout conn mbox
    | Error s ->
      put_async @@ ConnectionError s;
      return_unit

  let maybe_open_breaker c conn mbox bs =
    let bs = { bs with error_count = bs.error_count + 1 } in
    Logs_lwt.warn (fun l -> l "Handler returned error, count %d" bs.error_count) >>= fun () ->
    if bs.error_count < c.config.error_threshold
    then
      return bs
    else
      Logs_lwt.warn (fun l -> l "Error threshold exceeded (%d of %d)" bs.error_count c.config.error_threshold) >>= fun () ->
      (* Send RDY 0 and send retry trial command after a delay  *)
      Logs_lwt.debug (fun l -> l "Breaker open, sending RDY 0") >>= fun () ->
      send ~timeout:c.config.write_timeout ~conn (RDY 0) >>= fun () ->
      let bs = { bs with position = Open } in
      let duration = backoff_duration ~multiplier:c.config.backoff_multiplier ~error_count:bs.error_count in
      do_after_async ~duration (fun () -> Lwt_mvar.put mbox TrialBreaker);
      return bs

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
    let maybe_open_breaker = maybe_open_breaker c conn mbox in
    let update_breaker_state = update_breaker_state c conn maybe_open_breaker in
    let send = send ~timeout:c.config.write_timeout ~conn in
    let rec mbox_loop bs =
      Lwt_mvar.take mbox >>= function
      | RawFrame raw ->
        begin 
          match ServerMessage.of_raw_frame raw with
          | Ok server_message ->
            async 
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
      | ConnectionError s ->
        fail_with s
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
    send MAGIC >>= fun () ->
    let ic = Config.to_identity_config c.config in
    identify ~read_timeout:c.config.read_timeout ~write_timeout:c.config.write_timeout ~conn ic >>= fun () ->
    subscribe ~read_timeout:c.config.read_timeout ~write_timeout:c.config.write_timeout ~conn c.topic c.channel >>= fun () ->
    (* Start cautiously by sending RDY 1 *) 
    Logs_lwt.debug (fun l -> l "%s Sending initial RDY 1" c.log_prefix) >>= fun () ->
    send (RDY 1) >>= fun () ->
    (* Start background reader *)
    async (fun () -> read_loop ~timeout:c.config.read_timeout conn mbox);
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
      c.open_connections <- c.open_connections + 1;
      Logs_lwt.debug ( fun l -> l "%s %d connections" c.log_prefix c.open_connections) >>= fun () ->
      catch (fun () -> consume c conn mbox) handle_ex >>= fun () ->
      (* If we get here it means that something failed and we need to reconnect *)
      c.open_connections <- (max 0 (c.open_connections - 1));
      Logs_lwt.debug ( fun l -> l "%s %d connections" c.log_prefix c.open_connections) >>= fun () ->
      main_loop c address mbox
    | Error e ->
      Logs_lwt.err (fun l -> l "%s Connecting to consumer '%s': %s" c.log_prefix (Address.to_string address) e) >>= fun () ->
      Lwt_unix.sleep default_backoff_seconds >>= fun () ->
      main_loop c address mbox

  let async_exception_hook e =
    Logs.err (fun l -> l "Async exception: %s" (Exn.to_string e))

  (** 
           Start an async thread to update RDY count occasionaly.
           The number of open connections can change as we add new consumers due to lookupd
           discovering producers or we have connection failures. Each time a new connection 
           is opened or closed the consumer.nsqd_connections field is updated.
           We therefore need to occasionaly update our RDY count as this may have changed so that
           it is spread evenly across connections.
  *)
  let start_background_ready_calculator c mbox =
    let jitter = Random.float (recalculate_rdy_interval /. 10.0) in
    let interval = recalculate_rdy_interval +. jitter in
    let rec loop () =
      Lwt_unix.sleep interval >>= fun () ->
      Logs_lwt.debug ( fun l -> l "%s recalculating RDY" c.log_prefix) >>= fun () ->
      Lwt_mvar.put mbox RecalcRDY >>= loop
    in
    async loop

  let start_nsqd_consumer c address =
    let mbox = Lwt_mvar.create_empty () in
    start_background_ready_calculator c mbox;
    main_loop c address mbox

  let start_polling_lookupd c lookup_addresses =
    let poll_interval = Float.((1.0 + (Random.float c.config.lookupd_poll_jitter)) * (Seconds.value c.config.lookupd_poll_interval)) in
    let rec check_for_producers running =
      catch
        begin
          fun () ->
            Logs_lwt.debug (fun l -> l "Querying %d lookupd hosts" (List.length lookup_addresses)) >>= fun () ->
            Lwt_list.map_p (query_nsqlookupd ~topic:c.topic) lookup_addresses >>= fun results ->
            let discovered_producers =
              List.filter_map ~f:Result.ok results
              |> List.map ~f:Lookup.producer_addresses
              |> List.join
              |> Set.of_list (module Address)
            in
            let new_producers = Set.diff discovered_producers running in
            Logs_lwt.debug (fun l -> l "Found %d new producers" (Set.length new_producers)) >>= fun () ->
            Set.iter
              ~f:(
                fun a ->
                  async (fun () ->
                      Logs_lwt.debug (fun l -> l "Starting consumer for: %s" (Address.to_string a)) >>= fun () ->
                      start_nsqd_consumer c a)
              )
              new_producers;
            let running = Set.union running new_producers in
            Lwt_unix.sleep poll_interval >>= fun () ->
            check_for_producers running
        end
        begin
          fun e ->
            Logs_lwt.err (fun l -> l "Error polling lookupd: %s" (Exn.to_string e)) >>= fun () ->
            check_for_producers running
        end
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
      join consumers

end

module Producer = struct
  type connection = {
    conn : (Lwt_io.input Lwt_io.channel * Lwt_io.output Lwt_io.channel);
    last_write : float ref
  }

  type t = { 
    address : Address.t;
    pool : connection Lwt_pool.t;
  }

  let default_pool_size = 5
  let default_dial_timeout = Seconds.of_float 15.0
  let default_write_timeout = Seconds.of_float 15.0
  let default_read_timeout = Seconds.of_float 15.0

  (** Throw away connections that are idle for this long
       Note that NSQ expects hearbeats to be answered every 30 seconds
       and if two are missed it closes the connection.
  *)
  let ttl_seconds = 50.0 

  let create_pool address size =
    let validate c =
      let now = Unix.time () in
      let diff = now -. !(c.last_write) in
      return (Float.(diff < ttl_seconds))
    in
    (* Always return false so that we throw away connections where we encountered an error *)
    let check _ is_ok = is_ok false in
    let dispose c =
      Logs_lwt.warn (fun l -> l "Error publishing, closing connection") >>= fun () ->
      join [(Lwt_io.close (fst c.conn));(Lwt_io.close (snd c.conn))]
    in
    Lwt_pool.create size ~validate ~check ~dispose
      begin
        fun () -> 
          connect address default_dial_timeout >>= fun conn ->
          send ~timeout:default_write_timeout ~conn MAGIC >>= fun () ->
          let last_write = ref (Unix.time ()) in
          return { conn; last_write }
      end

  let create ?(pool_size=default_pool_size) address = 
    if pool_size < 1
    then Error "Pool size must be >= 1"
    else Ok { address; pool = create_pool address pool_size }

  let publish_cmd t cmd =
    let with_conn c =
      let rec read_until_ok () =
        read_raw_frame ~timeout:default_read_timeout c.conn >>= fun frame ->
        match ServerMessage.of_raw_frame frame with
        | Ok ResponseOk -> return_ok ()
        | Ok Heartbeat -> 
          send ~timeout:default_write_timeout ~conn:c.conn NOP >>= fun () ->
          c.last_write := Unix.time ();
          read_until_ok ()
        | Ok _ -> return_error "Expected OK or Heartbeat, got another message"
        | Error e -> return_error (Printf.sprintf "Received error: %s" e)
      in
      send ~timeout:default_write_timeout ~conn:c.conn cmd >>= fun () ->
      c.last_write := Unix.time ();
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
