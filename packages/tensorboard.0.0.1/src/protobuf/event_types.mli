(** event.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_log_message_level : unit -> log_message_level
(** [default_log_message_level ()] is the default value for type [log_message_level] *)

val default_log_message : 
  ?level:log_message_level ->
  ?message:string ->
  unit ->
  log_message
(** [default_log_message ()] is the default value for type [log_message] *)

val default_session_log_session_status : unit -> session_log_session_status
(** [default_session_log_session_status ()] is the default value for type [session_log_session_status] *)

val default_session_log : 
  ?status:session_log_session_status ->
  ?checkpoint_path:string ->
  ?msg:string ->
  unit ->
  session_log
(** [default_session_log ()] is the default value for type [session_log] *)

val default_tagged_run_metadata : 
  ?tag:string ->
  ?run_metadata:bytes ->
  unit ->
  tagged_run_metadata
(** [default_tagged_run_metadata ()] is the default value for type [tagged_run_metadata] *)

val default_event_what : unit -> event_what
(** [default_event_what ()] is the default value for type [event_what] *)

val default_event : 
  ?wall_time:float ->
  ?step:int64 ->
  ?what:event_what ->
  unit ->
  event
(** [default_event ()] is the default value for type [event] *)

val default_worker_health : unit -> worker_health
(** [default_worker_health ()] is the default value for type [worker_health] *)

val default_worker_shutdown_mode : unit -> worker_shutdown_mode
(** [default_worker_shutdown_mode ()] is the default value for type [worker_shutdown_mode] *)

val default_watchdog_config : 
  ?timeout_ms:int64 ->
  unit ->
  watchdog_config
(** [default_watchdog_config ()] is the default value for type [watchdog_config] *)

val default_worker_heartbeat_request : 
  ?shutdown_mode:worker_shutdown_mode ->
  ?watchdog_config:watchdog_config option ->
  unit ->
  worker_heartbeat_request
(** [default_worker_heartbeat_request ()] is the default value for type [worker_heartbeat_request] *)

val default_worker_heartbeat_response : 
  ?health_status:worker_health ->
  ?worker_log:event list ->
  ?hostname:string ->
  unit ->
  worker_heartbeat_response
(** [default_worker_heartbeat_response ()] is the default value for type [worker_heartbeat_response] *)
