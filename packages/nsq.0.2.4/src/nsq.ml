open Containers
open Lwt

let ephemeral_suffix = "#ephemeral"
let default_backoff_seconds = 1.0
let max_backoff_seconds = 3600.0
let default_nsqd_port = 4150
let default_lookupd_port = 4161
let default_lookupd_poll_seconds = 60.
let network_buffer_size = 16 * 1024

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
  data: bytes; 
}

type raw_message = {
  timestamp: int64;
  attempts: Unsigned.UInt16.t;
  id: MessageID.t;
  body: bytes; 
}

module Address = struct
  type t =
    | Host of string
    | HostPort of string * int

  let host s =
    Host s

  let host_port a p =
    HostPort (a, p)

  let to_string a =
    match a with
    | Host a -> a
    | HostPort (a, p) -> Format.sprintf "%s:%d" a p

  let compare a b =
    String.compare (to_string a) (to_string b)

  let equal a b =
    String.equal (to_string a) (to_string b)
end

type command =
  (* TODO: Identify *)
  | Magic
  | SUB of Topic.t * Channel.t
  | PUB of Topic.t * bytes
  | MPUB of Topic.t * bytes list
  | REQ of MessageID.t * Milliseconds.t
  | FIN of MessageID.t
  | TOUCH of MessageID.t
  | RDY of int
  | AUTH of string
  | CLS
  | NOP

type server_message =
  | ResponseOk
  | Heartbeat
  | ErrorInvalid of string
  | ErrorBadTopic of string
  | ErrorBadChannel of string
  | ErrorFINFailed of string
  | ErrorREQFailed of string
  | ErrorTOUCHFailed of string
  | Message of raw_message

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
} [@@deriving yojson]

type lookup_response = {
  channels: string list;
  producers: lookup_producer list;
} [@@deriving yojson]

let producer_addresses lr =
  List.map (fun p -> Address.host_port p.broadcast_address p.tcp_port) lr.producers

let lookup_response_from_string s =
  let open Result in
  guard_str (fun () -> Yojson.Safe.from_string s) >>=
  lookup_response_of_yojson

let log_and_return prefix r =
  match r with
  | Result.Ok _ as ok -> return ok
  | Result.Error s as error ->
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
          return_error (Format.sprintf "Expected %s, got %s" (Code.string_of_status `OK) (Code.string_of_status status))
    end
    begin
      fun e ->
        let s = Printexc.to_string e in
        Logs_lwt.err (fun l -> l "Querying lookupd `%s`: %s" (Address.to_string a) s) >>= fun () ->
        return_error s
    end

let bytes_of_pub topic data =
  let buf = Bytes.create 4 in
  EndianBytes.BigEndian.set_int32 buf 0 (Int32.of_int @@ Bytes.length data);
  Format.sprintf "PUB %s\n%s%s" (Topic.to_string topic) (Bytes.to_string buf) (Bytes.to_string data)
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
  let data_size = List.fold_left (fun a b -> a + Bytes.length b) 0 bodies in
  let buf = Bytes.create (4 + 4 + (4*body_count) + data_size) in
  EndianBytes.BigEndian.set_int32 buf 0 (Int32.of_int @@ data_size);
  EndianBytes.BigEndian.set_int32 buf 4 (Int32.of_int body_count);
  let index = ref 8 in
  List.iter (
    fun b -> 
      EndianBytes.BigEndian.set_int32 buf !index (Int32.of_int @@ Bytes.length b);
      index := !index + 4;
      Bytes.blit b 0 buf !index (Bytes.length b);
      index := !index + Bytes.length b;
  ) bodies;
  Format.sprintf "MPUB %s\n%s" (Topic.to_string topic) (Bytes.to_string buf)
  |> Bytes.of_string

let bytes_of_command = function
  | Magic -> Bytes.of_string "  V2" 
  | NOP -> Bytes.of_string "NOP\n"
  | RDY i -> Format.sprintf "RDY %i\n" i |> Bytes.of_string
  | FIN id -> Format.sprintf "FIN %s\n" (MessageID.to_string id) |> Bytes.of_string
  | TOUCH id -> Format.sprintf "TOUCH %s\n" (MessageID.to_string id) |> Bytes.of_string
  | SUB (t, c) -> Format.sprintf "SUB %s %s\n" (Topic.to_string t) (Channel.to_string c) |> Bytes.of_string
  | REQ (id, delay) -> Format.sprintf "REQ %s %Li\n" (MessageID.to_string id) (Milliseconds.value delay) |> Bytes.of_string
  | PUB (t, data) -> bytes_of_pub t data
  | MPUB (t, data) -> bytes_of_mpub t data
  | CLS -> Bytes.of_string "CLS\n"
  | AUTH secret -> 
    let buf = Bytes.create 4 in
    let length = String.length secret in
    EndianBytes.BigEndian.set_int32 buf 0 (Int32.of_int length);
    Format.sprintf "AUTH\n%s%s" (Bytes.to_string buf) secret
    |> Bytes.of_string

let send command (_, oc) =
  bytes_of_command command 
  |> Bytes.to_string
  |> Lwt_io.write oc

let catch_result promise =
  try_bind 
    promise
    (fun x -> return_ok x)
    (fun e -> return_error e)

let catch_result1 f x =
  catch_result (fun () -> f x)

let connect host =
  let (host, port) = match host with
    | Address.Host h          -> (h, default_nsqd_port)
    | Address.HostPort (h, p) -> (h, p)
  in
  Lwt_unix.gethostbyname host >>= fun info ->
  match Array.get_safe info.h_addr_list 0 with
  | None -> fail_with "Host lookup failed"
  | Some host ->
    let addr = Unix.ADDR_INET(host, port) in
    Lwt_io.open_connection 
      ~in_buffer:(Lwt_bytes.create network_buffer_size)
      ~out_buffer:(Lwt_bytes.create network_buffer_size)
      addr

let frame_from_bytes bytes =
  let frame_type = EndianBytes.BigEndian.get_int32 bytes 0 in
  let to_read = (Bytes.length bytes) - 4 in
  let data = Bytes.sub bytes 4 to_read in
  {frame_type; data}

let read_raw_frame (ic, _) =
  Lwt_io.BE.read_int32 ic >>= fun size ->
  let size = Int32.to_int size in
  let bytes = Bytes.create size in
  Lwt_io.read_into_exactly ic bytes 0 size >|= fun () ->
  frame_from_bytes bytes

let parse_response_body body =
  let body = Bytes.to_string body in
  match body with
  | "OK" -> Result.Ok ResponseOk
  | "_heartbeat_" -> Result.Ok Heartbeat
  | _ -> Result.Error (Format.sprintf "Unknown response: %s" body)

let parse_message_body_exn body =
  let timestamp = EndianBytes.BigEndian.get_int64 body 0 in
  let attempts = EndianBytes.BigEndian.get_uint16 body 8 |> Unsigned.UInt16.of_int in
  let length = Bytes.length body in
  let id = MessageID.of_bytes (Bytes.sub body 10 16) in
  let body = Bytes.sub body 26 (length-26) in
  Message { timestamp; attempts; id; body; }

let parse_message_body body =
  Result.guard_str @@ fun () -> parse_message_body_exn body

let parse_error_body body =
  match String.Split.left ~by:" " (Bytes.to_string body) with
  | None -> Result.Error (Format.sprintf "Malformed error code: %s" (Bytes.to_string body))
  | Some (code, detail) ->
    match code with
    | "E_INVALID" -> Result.return @@ ErrorInvalid detail
    | "E_BAD_TOPIC" -> Result.return @@ ErrorBadTopic detail
    | "E_BAD_CHANNEL" -> Result.return @@ ErrorBadChannel detail
    | "E_FIN_FAILED" -> Result.return @@ ErrorFINFailed detail
    | _ -> Result.Error (Format.sprintf "Unknown error code: %s. %s" code detail)

let server_message_of_frame raw =
  match (FrameType.of_int32 raw.frame_type) with
  | FrameResponse -> parse_response_body raw.data
  | FrameMessage -> parse_message_body raw.data
  | FrameError -> parse_error_body raw.data
  | FrameUnknown i -> Result.Error (Format.sprintf "Unknown frame type: %li" i)

let subscribe topic channel conn =
  send (SUB (topic, channel)) conn >>= fun () ->
  read_raw_frame conn >>= fun raw ->
  match server_message_of_frame raw with
  | Result.Ok ResponseOk -> return_unit
  | _ -> fail_with "Unexpected SUB response"

let requeue_delay attempts =
  let d = Milliseconds.value default_requeue_delay in
  let attempts = Int64.of_int_exn attempts in
  Milliseconds.of_int64 Int64.(d * attempts)

let handle_message handler msg max_attempts =
  catch_result1 handler msg.body >>=
  begin
    function
    | Result.Ok r -> return r
    | Result.Error e ->
      Logs_lwt.err (fun l -> l "Handler error: %s" (Printexc.to_string e)) >>= fun () ->
      return HandlerRequeue
  end
  >>= function
  | HandlerOK -> return (FIN msg.id)
  | HandlerRequeue -> 
    let attempts = Unsigned.UInt16.to_int msg.attempts in
    if attempts >= max_attempts
    then 
      (Logs_lwt.warn (fun l -> l "Discarding message as reached max attempts, %d" attempts) >>= fun () ->
       return (FIN msg.id))
    else
      let delay = requeue_delay attempts in
      return (REQ (msg.id, delay))

let handle_frame frame handler max_attempts =
  let warn_return_none name msg = 
    Logs_lwt.warn (fun l -> l "%s: %s" name msg) >>= fun () -> return_none 
  in
  match frame with
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
  type config = {
    max_in_flight : int;
    max_attempts : int;
    backoff_multiplier : float;
  }

  let validate_positive c value name =
    if value > 0
    then Result.Ok c
    else Result.Error (Format.sprintf "%s must be greater than 0" name)

  let validate_between min max value name c =
    if value >= min && value <= max
    then Result.Ok c
    else Result.Error (Format.sprintf "%s must be between %d and %d, got %d" name min max value)

  let max_in_flight_positive c =
    validate_positive c c.max_in_flight "max_in_flight"

  let max_attempts_between min max c =
    validate_between min max c.max_attempts "max_attempts" c

  let validate_config c =
    let open Result in
    max_in_flight_positive c
    >>= max_attempts_between 0 65535

  let default_config_value = {
    max_in_flight = 1;
    max_attempts = 5;
    backoff_multiplier = 0.5;
  }

  let default_config () =
    default_config_value

  type breaker_position =
    | Closed
    | HalfOpen
    | Open

  type breaker_state = {
    position : breaker_position;
    error_count : int;
  }

  let backoff_duration backoff_multiplier error_count =
    let bo = (backoff_multiplier *. (float_of_int error_count)) in
    Float.min bo max_backoff_seconds

  type mode =
    | ModeNsqd
    | ModeLookupd

  type t = {
    addresses : Address.t list;
    desired_rdy : int;
    topic : Topic.t;
    channel : Channel.t;
    handler : (bytes -> handler_result Lwt.t);
    config : config;
    mode : mode;
  }

  let create ?(mode=ModeNsqd) ?(config=default_config_value) addresses topic channel handler =
    let num_addresses = List.length addresses in
    match validate_config config with
    | Result.Error s -> Result.Error (Format.sprintf "Invalid config: %s" s)
    | Result.Ok config ->
      let desired_rdy = max 1 (config.max_in_flight / num_addresses) in
      Result.return { 
        addresses;
        desired_rdy;
        topic; 
        channel; 
        handler;
        config;
        mode;
      }

  let do_after duration f =
    Logs_lwt.debug (fun l -> l "Sleeping for %f seconds" duration) >>= fun () ->
    Lwt_unix.sleep duration >>= f

  type loop_message =
    | RawFrame of raw_frame
    | Command of command
    | TrialBreaker
    | ConnectionError of exn

  let rec read_loop conn mbox =
    let put_async m = Lwt.async @@ fun () -> Lwt_mvar.put mbox m in
    catch_result1 read_raw_frame conn >>= function
    | Result.Ok raw ->
      put_async @@ RawFrame raw;
      read_loop conn mbox
    | Result.Error e ->
      put_async @@ ConnectionError e;
      return_unit

  let open_breaker c conn mbox bs =
    (* Send RDY 0 and send retry trial command after a delay  *)
    Logs_lwt.debug (fun l -> l "Breaker open, sending RDY 0") >>= fun () ->
    send (RDY 0) conn >|= fun () ->
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
        Logs_lwt.debug (fun l -> l "Trial passed, sending RDY %d" c.desired_rdy) >>= fun () ->
        send (RDY c.desired_rdy) conn >>= fun () ->
        return { position = Closed; error_count = 0; }

  let consume c conn mbox =
    let open_breaker = open_breaker c conn mbox in
    let update_breaker_state = update_breaker_state c conn open_breaker in
    let rec mbox_loop bs =
      Lwt_mvar.take mbox >>= function
      | RawFrame raw ->
        begin 
          match server_message_of_frame raw with
          | Result.Ok frame ->
            Lwt.async 
              begin
                fun () -> 
                  handle_frame frame c.handler c.config.max_attempts >>= function
                  | Some c -> Lwt_mvar.put mbox (Command c)
                  | None -> return_unit
              end;
            mbox_loop bs
          | Result.Error s -> 
            Logs_lwt.err (fun l -> l "Error parsing response: %s" s) >>= fun () ->
            mbox_loop bs
        end
      | Command cmd ->
        send cmd conn >>= fun () ->
        update_breaker_state cmd bs >>= mbox_loop
      | TrialBreaker ->
        Logs_lwt.debug (fun l -> l "Breaker trial, sending RDY 1 (Error count: %i)" bs.error_count) >>= fun () ->
        let bs = { bs with position = HalfOpen } in
        send (RDY 1) conn >>=  fun () ->
        mbox_loop bs
      | ConnectionError e ->
        fail e 
    in
    send Magic conn >>= fun () ->
    subscribe c.topic c.channel conn >>= fun () ->
    (* Start cautiously by sending RDY 1 *)
    Logs_lwt.debug (fun l -> l "Sending initial RDY 1") >>= fun () ->
    send (RDY 1) conn >>= fun () ->
    (* Start background reader *)
    Lwt.async (fun () -> read_loop conn mbox);
    let initial_state = { position = HalfOpen; error_count = 0; } in
    mbox_loop initial_state

  let rec main_loop c address mbox =
    catch_result (fun () -> connect address) >>= function
    | Result.Ok conn ->
      let handle_ex e = 
        Lwt.join [
          Logs_lwt.err (fun l -> l "Reader failed: %s" (Printexc.to_string e));
          (Lwt_io.close (fst conn));
          (Lwt_io.close (snd conn));
          (Lwt_unix.sleep default_backoff_seconds)]
      in
      catch (fun () -> consume c conn mbox) handle_ex >>= fun () ->
      main_loop c address mbox
    | Result.Error e ->
      Logs_lwt.err (fun l -> l "Connecting to consumer '%s': %s" (Address.to_string address) (Printexc.to_string e)) >>= fun () ->
      Lwt_unix.sleep default_backoff_seconds >>= fun () ->
      main_loop c address mbox

  let async_exception_hook e =
    Logs.err (fun l -> l "Async exception: %s" (Printexc.to_string e))

  let start_nsqd_consumer c address =
    let mbox = Lwt_mvar.create_empty () in
    main_loop c address mbox

  (* Return only succesful results, but log the errors found *)
  let start_polling_lookupd c lookup_addresses =
    let rec internal running =
      Logs_lwt.debug (fun l -> l "Querying %d lookupd hosts" (List.length lookup_addresses)) >>= fun () ->
      Lwt_list.map_p (query_nsqlookupd ~topic:c.topic) lookup_addresses >>= fun results ->
      let new_producers =
        List.keep_ok results
        |> List.flat_map producer_addresses
        |> List.sort_uniq ~cmp:Address.compare
        |> List.filter (fun a -> not @@ List.mem ~eq:Address.equal a running)
      in
      Logs_lwt.debug (fun l -> l "Found %d new producers" (List.length new_producers)) >>= fun () ->
      List.iter 
        (fun a -> 
           async (fun () -> 
               Logs_lwt.debug (fun l -> l "Starting consumer for: %s" (Address.to_string a)) >>= fun () ->
               start_nsqd_consumer c a)
        ) 
        new_producers;
      let running = List.union ~eq:Address.equal running new_producers in
      Lwt_unix.sleep default_lookupd_poll_seconds >>= fun () ->
      internal running
    in
    internal []

  let run c =
    Lwt.async_exception_hook := async_exception_hook;
    match c.mode with
    | ModeLookupd ->
      Logs_lwt.debug (fun l -> l "Starting lookupd poll") >>= fun () ->
      start_polling_lookupd c c.addresses
    | ModeNsqd ->
      let consumers = List.map (fun a -> start_nsqd_consumer c a) c.addresses in
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
          connect address >>= fun conn ->
          send Magic conn >>= fun () ->
          let last_send = ref (Unix.time ()) in
          return { conn; last_send }
      end

  let create ?(pool_size=default_pool_size) address = 
    if pool_size <= 0
    then Result.Error "Pool size must be >= 1"
    else Result.Ok { address; pool = create_pool address pool_size }

  let publish_cmd t cmd =
    let with_conn c =
      let rec read_until_ok () =
        read_raw_frame c.conn >>= fun frame ->
        match server_message_of_frame frame with
        | Result.Ok ResponseOk -> return_ok ()
        | Result.Ok Heartbeat -> 
          send NOP c.conn >>= fun () -> 
          c.last_send := Unix.time ();
          read_until_ok ()
        | Result.Ok _ -> return_error "Expected OK or Heartbeat, got another message"
        | Result.Error e -> return_error (Format.sprintf "Received error: %s" e)
      in
      send cmd c.conn >>= fun () ->
      c.last_send := Unix.time ();
      read_until_ok ()
    in
    let try_publish () = Lwt_pool.use t.pool with_conn in
    let handle_ex e =
      let message = Format.sprintf "Publishing to `%s`: %s" (Address.to_string t.address) (Printexc.to_string e) in
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
