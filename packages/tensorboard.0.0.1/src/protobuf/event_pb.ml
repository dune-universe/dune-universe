[@@@ocaml.warning "-27-30-39"]

type log_message_mutable = {
  mutable level : Event_types.log_message_level;
  mutable message : string;
}

let default_log_message_mutable () : log_message_mutable = {
  level = Event_types.default_log_message_level ();
  message = "";
}

type session_log_mutable = {
  mutable status : Event_types.session_log_session_status;
  mutable checkpoint_path : string;
  mutable msg : string;
}

let default_session_log_mutable () : session_log_mutable = {
  status = Event_types.default_session_log_session_status ();
  checkpoint_path = "";
  msg = "";
}

type tagged_run_metadata_mutable = {
  mutable tag : string;
  mutable run_metadata : bytes;
}

let default_tagged_run_metadata_mutable () : tagged_run_metadata_mutable = {
  tag = "";
  run_metadata = Bytes.create 0;
}

type event_mutable = {
  mutable wall_time : float;
  mutable step : int64;
  mutable what : Event_types.event_what;
}

let default_event_mutable () : event_mutable = {
  wall_time = 0.;
  step = 0L;
  what = Event_types.File_version ("");
}

type watchdog_config_mutable = {
  mutable timeout_ms : int64;
}

let default_watchdog_config_mutable () : watchdog_config_mutable = {
  timeout_ms = 0L;
}

type worker_heartbeat_request_mutable = {
  mutable shutdown_mode : Event_types.worker_shutdown_mode;
  mutable watchdog_config : Event_types.watchdog_config option;
}

let default_worker_heartbeat_request_mutable () : worker_heartbeat_request_mutable = {
  shutdown_mode = Event_types.default_worker_shutdown_mode ();
  watchdog_config = None;
}

type worker_heartbeat_response_mutable = {
  mutable health_status : Event_types.worker_health;
  mutable worker_log : Event_types.event list;
  mutable hostname : string;
}

let default_worker_heartbeat_response_mutable () : worker_heartbeat_response_mutable = {
  health_status = Event_types.default_worker_health ();
  worker_log = [];
  hostname = "";
}


let rec decode_log_message_level d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Event_types.Unknown:Event_types.log_message_level)
  | 10 -> (Event_types.Debugging:Event_types.log_message_level)
  | 20 -> (Event_types.Info:Event_types.log_message_level)
  | 30 -> (Event_types.Warn:Event_types.log_message_level)
  | 40 -> (Event_types.Error:Event_types.log_message_level)
  | 50 -> (Event_types.Fatal:Event_types.log_message_level)
  | _ -> Pbrt.Decoder.malformed_variant "log_message_level"

let rec decode_log_message d =
  let v = default_log_message_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.level <- decode_log_message_level d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_message), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.message <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(log_message), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Event_types.level = v.level;
    Event_types.message = v.message;
  } : Event_types.log_message)

let rec decode_session_log_session_status d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Event_types.Status_unspecified:Event_types.session_log_session_status)
  | 1 -> (Event_types.Start:Event_types.session_log_session_status)
  | 2 -> (Event_types.Stop:Event_types.session_log_session_status)
  | 3 -> (Event_types.Checkpoint:Event_types.session_log_session_status)
  | _ -> Pbrt.Decoder.malformed_variant "session_log_session_status"

let rec decode_session_log d =
  let v = default_session_log_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.status <- decode_session_log_session_status d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(session_log), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.checkpoint_path <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(session_log), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.msg <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(session_log), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Event_types.status = v.status;
    Event_types.checkpoint_path = v.checkpoint_path;
    Event_types.msg = v.msg;
  } : Event_types.session_log)

let rec decode_tagged_run_metadata d =
  let v = default_tagged_run_metadata_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.tag <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tagged_run_metadata), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.run_metadata <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tagged_run_metadata), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Event_types.tag = v.tag;
    Event_types.run_metadata = v.run_metadata;
  } : Event_types.tagged_run_metadata)

let rec decode_event_what d = 
  let rec loop () = 
    let ret:Event_types.event_what = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "event_what"
      | Some (3, _) -> Event_types.File_version (Pbrt.Decoder.string d)
      | Some (4, _) -> Event_types.Graph_def (Pbrt.Decoder.bytes d)
      | Some (5, _) -> Event_types.Summary (Summary_pb.decode_summary (Pbrt.Decoder.nested d))
      | Some (6, _) -> Event_types.Log_message (decode_log_message (Pbrt.Decoder.nested d))
      | Some (7, _) -> Event_types.Session_log (decode_session_log (Pbrt.Decoder.nested d))
      | Some (8, _) -> Event_types.Tagged_run_metadata (decode_tagged_run_metadata (Pbrt.Decoder.nested d))
      | Some (9, _) -> Event_types.Meta_graph_def (Pbrt.Decoder.bytes d)
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_event d =
  let v = default_event_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      v.wall_time <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.step <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.what <- Event_types.File_version (Pbrt.Decoder.string d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.what <- Event_types.Graph_def (Pbrt.Decoder.bytes d);
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.what <- Event_types.Summary (Summary_pb.decode_summary (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.what <- Event_types.Log_message (decode_log_message (Pbrt.Decoder.nested d));
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.what <- Event_types.Session_log (decode_session_log (Pbrt.Decoder.nested d));
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.what <- Event_types.Tagged_run_metadata (decode_tagged_run_metadata (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.what <- Event_types.Meta_graph_def (Pbrt.Decoder.bytes d);
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(event), field(9)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Event_types.wall_time = v.wall_time;
    Event_types.step = v.step;
    Event_types.what = v.what;
  } : Event_types.event)

let rec decode_worker_health d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Event_types.Ok:Event_types.worker_health)
  | 1 -> (Event_types.Received_shutdown_signal:Event_types.worker_health)
  | 2 -> (Event_types.Internal_error:Event_types.worker_health)
  | _ -> Pbrt.Decoder.malformed_variant "worker_health"

let rec decode_worker_shutdown_mode d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Event_types.Default:Event_types.worker_shutdown_mode)
  | 1 -> (Event_types.Shutdown_immediately:Event_types.worker_shutdown_mode)
  | 2 -> (Event_types.Wait_for_coordinator:Event_types.worker_shutdown_mode)
  | _ -> Pbrt.Decoder.malformed_variant "worker_shutdown_mode"

let rec decode_watchdog_config d =
  let v = default_watchdog_config_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.timeout_ms <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(watchdog_config), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Event_types.timeout_ms = v.timeout_ms;
  } : Event_types.watchdog_config)

let rec decode_worker_heartbeat_request d =
  let v = default_worker_heartbeat_request_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.shutdown_mode <- decode_worker_shutdown_mode d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(worker_heartbeat_request), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.watchdog_config <- Some (decode_watchdog_config (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(worker_heartbeat_request), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Event_types.shutdown_mode = v.shutdown_mode;
    Event_types.watchdog_config = v.watchdog_config;
  } : Event_types.worker_heartbeat_request)

let rec decode_worker_heartbeat_response d =
  let v = default_worker_heartbeat_response_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.worker_log <- List.rev v.worker_log;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.health_status <- decode_worker_health d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(worker_heartbeat_response), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.worker_log <- (decode_event (Pbrt.Decoder.nested d)) :: v.worker_log;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(worker_heartbeat_response), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.hostname <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(worker_heartbeat_response), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Event_types.health_status = v.health_status;
    Event_types.worker_log = v.worker_log;
    Event_types.hostname = v.hostname;
  } : Event_types.worker_heartbeat_response)

let rec encode_log_message_level (v:Event_types.log_message_level) encoder =
  match v with
  | Event_types.Unknown -> Pbrt.Encoder.int_as_varint (0) encoder
  | Event_types.Debugging -> Pbrt.Encoder.int_as_varint 10 encoder
  | Event_types.Info -> Pbrt.Encoder.int_as_varint 20 encoder
  | Event_types.Warn -> Pbrt.Encoder.int_as_varint 30 encoder
  | Event_types.Error -> Pbrt.Encoder.int_as_varint 40 encoder
  | Event_types.Fatal -> Pbrt.Encoder.int_as_varint 50 encoder

let rec encode_log_message (v:Event_types.log_message) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_log_message_level v.Event_types.level encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Event_types.message encoder;
  ()

let rec encode_session_log_session_status (v:Event_types.session_log_session_status) encoder =
  match v with
  | Event_types.Status_unspecified -> Pbrt.Encoder.int_as_varint (0) encoder
  | Event_types.Start -> Pbrt.Encoder.int_as_varint 1 encoder
  | Event_types.Stop -> Pbrt.Encoder.int_as_varint 2 encoder
  | Event_types.Checkpoint -> Pbrt.Encoder.int_as_varint 3 encoder

let rec encode_session_log (v:Event_types.session_log) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_session_log_session_status v.Event_types.status encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Event_types.checkpoint_path encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Event_types.msg encoder;
  ()

let rec encode_tagged_run_metadata (v:Event_types.tagged_run_metadata) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Event_types.tag encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Event_types.run_metadata encoder;
  ()

let rec encode_event_what (v:Event_types.event_what) encoder = 
  begin match v with
  | Event_types.File_version x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Event_types.Graph_def x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  | Event_types.Summary x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Summary_pb.encode_summary x) encoder;
  | Event_types.Log_message x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_message x) encoder;
  | Event_types.Session_log x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_session_log x) encoder;
  | Event_types.Tagged_run_metadata x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tagged_run_metadata x) encoder;
  | Event_types.Meta_graph_def x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  end

and encode_event (v:Event_types.event) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Event_types.wall_time encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Event_types.step encoder;
  begin match v.Event_types.what with
  | Event_types.File_version x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.string x encoder;
  | Event_types.Graph_def x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  | Event_types.Summary x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Summary_pb.encode_summary x) encoder;
  | Event_types.Log_message x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_log_message x) encoder;
  | Event_types.Session_log x ->
    Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_session_log x) encoder;
  | Event_types.Tagged_run_metadata x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tagged_run_metadata x) encoder;
  | Event_types.Meta_graph_def x ->
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  end;
  ()

let rec encode_worker_health (v:Event_types.worker_health) encoder =
  match v with
  | Event_types.Ok -> Pbrt.Encoder.int_as_varint (0) encoder
  | Event_types.Received_shutdown_signal -> Pbrt.Encoder.int_as_varint 1 encoder
  | Event_types.Internal_error -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_worker_shutdown_mode (v:Event_types.worker_shutdown_mode) encoder =
  match v with
  | Event_types.Default -> Pbrt.Encoder.int_as_varint (0) encoder
  | Event_types.Shutdown_immediately -> Pbrt.Encoder.int_as_varint 1 encoder
  | Event_types.Wait_for_coordinator -> Pbrt.Encoder.int_as_varint 2 encoder

let rec encode_watchdog_config (v:Event_types.watchdog_config) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Event_types.timeout_ms encoder;
  ()

let rec encode_worker_heartbeat_request (v:Event_types.worker_heartbeat_request) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_worker_shutdown_mode v.Event_types.shutdown_mode encoder;
  begin match v.Event_types.watchdog_config with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_watchdog_config x) encoder;
  | None -> ();
  end;
  ()

let rec encode_worker_heartbeat_response (v:Event_types.worker_heartbeat_response) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  encode_worker_health v.Event_types.health_status encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_event x) encoder;
  ) v.Event_types.worker_log;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Event_types.hostname encoder;
  ()
