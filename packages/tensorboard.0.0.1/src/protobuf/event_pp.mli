(** event.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_log_message_level : Format.formatter -> Event_types.log_message_level -> unit 
(** [pp_log_message_level v] formats v *)

val pp_log_message : Format.formatter -> Event_types.log_message -> unit 
(** [pp_log_message v] formats v *)

val pp_session_log_session_status : Format.formatter -> Event_types.session_log_session_status -> unit 
(** [pp_session_log_session_status v] formats v *)

val pp_session_log : Format.formatter -> Event_types.session_log -> unit 
(** [pp_session_log v] formats v *)

val pp_tagged_run_metadata : Format.formatter -> Event_types.tagged_run_metadata -> unit 
(** [pp_tagged_run_metadata v] formats v *)

val pp_event_what : Format.formatter -> Event_types.event_what -> unit 
(** [pp_event_what v] formats v *)

val pp_event : Format.formatter -> Event_types.event -> unit 
(** [pp_event v] formats v *)

val pp_worker_health : Format.formatter -> Event_types.worker_health -> unit 
(** [pp_worker_health v] formats v *)

val pp_worker_shutdown_mode : Format.formatter -> Event_types.worker_shutdown_mode -> unit 
(** [pp_worker_shutdown_mode v] formats v *)

val pp_watchdog_config : Format.formatter -> Event_types.watchdog_config -> unit 
(** [pp_watchdog_config v] formats v *)

val pp_worker_heartbeat_request : Format.formatter -> Event_types.worker_heartbeat_request -> unit 
(** [pp_worker_heartbeat_request v] formats v *)

val pp_worker_heartbeat_response : Format.formatter -> Event_types.worker_heartbeat_response -> unit 
(** [pp_worker_heartbeat_response v] formats v *)
