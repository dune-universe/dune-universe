[@@@ocaml.warning "-27-30-39"]

let rec pp_log_message_level fmt (v:Event_types.log_message_level) =
  match v with
  | Event_types.Unknown -> Format.fprintf fmt "Unknown"
  | Event_types.Debugging -> Format.fprintf fmt "Debugging"
  | Event_types.Info -> Format.fprintf fmt "Info"
  | Event_types.Warn -> Format.fprintf fmt "Warn"
  | Event_types.Error -> Format.fprintf fmt "Error"
  | Event_types.Fatal -> Format.fprintf fmt "Fatal"

let rec pp_log_message fmt (v:Event_types.log_message) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "level" pp_log_message_level fmt v.Event_types.level;
    Pbrt.Pp.pp_record_field "message" Pbrt.Pp.pp_string fmt v.Event_types.message;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_session_log_session_status fmt (v:Event_types.session_log_session_status) =
  match v with
  | Event_types.Status_unspecified -> Format.fprintf fmt "Status_unspecified"
  | Event_types.Start -> Format.fprintf fmt "Start"
  | Event_types.Stop -> Format.fprintf fmt "Stop"
  | Event_types.Checkpoint -> Format.fprintf fmt "Checkpoint"

let rec pp_session_log fmt (v:Event_types.session_log) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "status" pp_session_log_session_status fmt v.Event_types.status;
    Pbrt.Pp.pp_record_field "checkpoint_path" Pbrt.Pp.pp_string fmt v.Event_types.checkpoint_path;
    Pbrt.Pp.pp_record_field "msg" Pbrt.Pp.pp_string fmt v.Event_types.msg;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_tagged_run_metadata fmt (v:Event_types.tagged_run_metadata) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "tag" Pbrt.Pp.pp_string fmt v.Event_types.tag;
    Pbrt.Pp.pp_record_field "run_metadata" Pbrt.Pp.pp_bytes fmt v.Event_types.run_metadata;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_event_what fmt (v:Event_types.event_what) =
  match v with
  | Event_types.File_version x -> Format.fprintf fmt "@[File_version(%a)@]" Pbrt.Pp.pp_string x
  | Event_types.Graph_def x -> Format.fprintf fmt "@[Graph_def(%a)@]" Pbrt.Pp.pp_bytes x
  | Event_types.Summary x -> Format.fprintf fmt "@[Summary(%a)@]" Summary_pp.pp_summary x
  | Event_types.Log_message x -> Format.fprintf fmt "@[Log_message(%a)@]" pp_log_message x
  | Event_types.Session_log x -> Format.fprintf fmt "@[Session_log(%a)@]" pp_session_log x
  | Event_types.Tagged_run_metadata x -> Format.fprintf fmt "@[Tagged_run_metadata(%a)@]" pp_tagged_run_metadata x
  | Event_types.Meta_graph_def x -> Format.fprintf fmt "@[Meta_graph_def(%a)@]" Pbrt.Pp.pp_bytes x

and pp_event fmt (v:Event_types.event) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "wall_time" Pbrt.Pp.pp_float fmt v.Event_types.wall_time;
    Pbrt.Pp.pp_record_field "step" Pbrt.Pp.pp_int64 fmt v.Event_types.step;
    Pbrt.Pp.pp_record_field "what" pp_event_what fmt v.Event_types.what;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_worker_health fmt (v:Event_types.worker_health) =
  match v with
  | Event_types.Ok -> Format.fprintf fmt "Ok"
  | Event_types.Received_shutdown_signal -> Format.fprintf fmt "Received_shutdown_signal"
  | Event_types.Internal_error -> Format.fprintf fmt "Internal_error"

let rec pp_worker_shutdown_mode fmt (v:Event_types.worker_shutdown_mode) =
  match v with
  | Event_types.Default -> Format.fprintf fmt "Default"
  | Event_types.Shutdown_immediately -> Format.fprintf fmt "Shutdown_immediately"
  | Event_types.Wait_for_coordinator -> Format.fprintf fmt "Wait_for_coordinator"

let rec pp_watchdog_config fmt (v:Event_types.watchdog_config) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "timeout_ms" Pbrt.Pp.pp_int64 fmt v.Event_types.timeout_ms;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_worker_heartbeat_request fmt (v:Event_types.worker_heartbeat_request) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "shutdown_mode" pp_worker_shutdown_mode fmt v.Event_types.shutdown_mode;
    Pbrt.Pp.pp_record_field "watchdog_config" (Pbrt.Pp.pp_option pp_watchdog_config) fmt v.Event_types.watchdog_config;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_worker_heartbeat_response fmt (v:Event_types.worker_heartbeat_response) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "health_status" pp_worker_health fmt v.Event_types.health_status;
    Pbrt.Pp.pp_record_field "worker_log" (Pbrt.Pp.pp_list pp_event) fmt v.Event_types.worker_log;
    Pbrt.Pp.pp_record_field "hostname" Pbrt.Pp.pp_string fmt v.Event_types.hostname;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
