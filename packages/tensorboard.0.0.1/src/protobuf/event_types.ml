[@@@ocaml.warning "-27-30-39"]


type log_message_level =
  | Unknown 
  | Debugging 
  | Info 
  | Warn 
  | Error 
  | Fatal 

type log_message = {
  level : log_message_level;
  message : string;
}

type session_log_session_status =
  | Status_unspecified 
  | Start 
  | Stop 
  | Checkpoint 

type session_log = {
  status : session_log_session_status;
  checkpoint_path : string;
  msg : string;
}

type tagged_run_metadata = {
  tag : string;
  run_metadata : bytes;
}

type event_what =
  | File_version of string
  | Graph_def of bytes
  | Summary of Summary_types.summary
  | Log_message of log_message
  | Session_log of session_log
  | Tagged_run_metadata of tagged_run_metadata
  | Meta_graph_def of bytes

and event = {
  wall_time : float;
  step : int64;
  what : event_what;
}

type worker_health =
  | Ok 
  | Received_shutdown_signal 
  | Internal_error 

type worker_shutdown_mode =
  | Default 
  | Shutdown_immediately 
  | Wait_for_coordinator 

type watchdog_config = {
  timeout_ms : int64;
}

type worker_heartbeat_request = {
  shutdown_mode : worker_shutdown_mode;
  watchdog_config : watchdog_config option;
}

type worker_heartbeat_response = {
  health_status : worker_health;
  worker_log : event list;
  hostname : string;
}

let rec default_log_message_level () = (Unknown:log_message_level)

let rec default_log_message 
  ?level:((level:log_message_level) = default_log_message_level ())
  ?message:((message:string) = "")
  () : log_message  = {
  level;
  message;
}

let rec default_session_log_session_status () = (Status_unspecified:session_log_session_status)

let rec default_session_log 
  ?status:((status:session_log_session_status) = default_session_log_session_status ())
  ?checkpoint_path:((checkpoint_path:string) = "")
  ?msg:((msg:string) = "")
  () : session_log  = {
  status;
  checkpoint_path;
  msg;
}

let rec default_tagged_run_metadata 
  ?tag:((tag:string) = "")
  ?run_metadata:((run_metadata:bytes) = Bytes.create 0)
  () : tagged_run_metadata  = {
  tag;
  run_metadata;
}

let rec default_event_what () : event_what = File_version ("")

and default_event 
  ?wall_time:((wall_time:float) = 0.)
  ?step:((step:int64) = 0L)
  ?what:((what:event_what) = File_version (""))
  () : event  = {
  wall_time;
  step;
  what;
}

let rec default_worker_health () = (Ok:worker_health)

let rec default_worker_shutdown_mode () = (Default:worker_shutdown_mode)

let rec default_watchdog_config 
  ?timeout_ms:((timeout_ms:int64) = 0L)
  () : watchdog_config  = {
  timeout_ms;
}

let rec default_worker_heartbeat_request 
  ?shutdown_mode:((shutdown_mode:worker_shutdown_mode) = default_worker_shutdown_mode ())
  ?watchdog_config:((watchdog_config:watchdog_config option) = None)
  () : worker_heartbeat_request  = {
  shutdown_mode;
  watchdog_config;
}

let rec default_worker_heartbeat_response 
  ?health_status:((health_status:worker_health) = default_worker_health ())
  ?worker_log:((worker_log:event list) = [])
  ?hostname:((hostname:string) = "")
  () : worker_heartbeat_response  = {
  health_status;
  worker_log;
  hostname;
}
