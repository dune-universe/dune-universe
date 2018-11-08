(** event.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_log_message_level : Event_types.log_message_level -> Pbrt.Encoder.t -> unit
(** [encode_log_message_level v encoder] encodes [v] with the given [encoder] *)

val encode_log_message : Event_types.log_message -> Pbrt.Encoder.t -> unit
(** [encode_log_message v encoder] encodes [v] with the given [encoder] *)

val encode_session_log_session_status : Event_types.session_log_session_status -> Pbrt.Encoder.t -> unit
(** [encode_session_log_session_status v encoder] encodes [v] with the given [encoder] *)

val encode_session_log : Event_types.session_log -> Pbrt.Encoder.t -> unit
(** [encode_session_log v encoder] encodes [v] with the given [encoder] *)

val encode_tagged_run_metadata : Event_types.tagged_run_metadata -> Pbrt.Encoder.t -> unit
(** [encode_tagged_run_metadata v encoder] encodes [v] with the given [encoder] *)

val encode_event_what : Event_types.event_what -> Pbrt.Encoder.t -> unit
(** [encode_event_what v encoder] encodes [v] with the given [encoder] *)

val encode_event : Event_types.event -> Pbrt.Encoder.t -> unit
(** [encode_event v encoder] encodes [v] with the given [encoder] *)

val encode_worker_health : Event_types.worker_health -> Pbrt.Encoder.t -> unit
(** [encode_worker_health v encoder] encodes [v] with the given [encoder] *)

val encode_worker_shutdown_mode : Event_types.worker_shutdown_mode -> Pbrt.Encoder.t -> unit
(** [encode_worker_shutdown_mode v encoder] encodes [v] with the given [encoder] *)

val encode_watchdog_config : Event_types.watchdog_config -> Pbrt.Encoder.t -> unit
(** [encode_watchdog_config v encoder] encodes [v] with the given [encoder] *)

val encode_worker_heartbeat_request : Event_types.worker_heartbeat_request -> Pbrt.Encoder.t -> unit
(** [encode_worker_heartbeat_request v encoder] encodes [v] with the given [encoder] *)

val encode_worker_heartbeat_response : Event_types.worker_heartbeat_response -> Pbrt.Encoder.t -> unit
(** [encode_worker_heartbeat_response v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_log_message_level : Pbrt.Decoder.t -> Event_types.log_message_level
(** [decode_log_message_level decoder] decodes a [log_message_level] value from [decoder] *)

val decode_log_message : Pbrt.Decoder.t -> Event_types.log_message
(** [decode_log_message decoder] decodes a [log_message] value from [decoder] *)

val decode_session_log_session_status : Pbrt.Decoder.t -> Event_types.session_log_session_status
(** [decode_session_log_session_status decoder] decodes a [session_log_session_status] value from [decoder] *)

val decode_session_log : Pbrt.Decoder.t -> Event_types.session_log
(** [decode_session_log decoder] decodes a [session_log] value from [decoder] *)

val decode_tagged_run_metadata : Pbrt.Decoder.t -> Event_types.tagged_run_metadata
(** [decode_tagged_run_metadata decoder] decodes a [tagged_run_metadata] value from [decoder] *)

val decode_event_what : Pbrt.Decoder.t -> Event_types.event_what
(** [decode_event_what decoder] decodes a [event_what] value from [decoder] *)

val decode_event : Pbrt.Decoder.t -> Event_types.event
(** [decode_event decoder] decodes a [event] value from [decoder] *)

val decode_worker_health : Pbrt.Decoder.t -> Event_types.worker_health
(** [decode_worker_health decoder] decodes a [worker_health] value from [decoder] *)

val decode_worker_shutdown_mode : Pbrt.Decoder.t -> Event_types.worker_shutdown_mode
(** [decode_worker_shutdown_mode decoder] decodes a [worker_shutdown_mode] value from [decoder] *)

val decode_watchdog_config : Pbrt.Decoder.t -> Event_types.watchdog_config
(** [decode_watchdog_config decoder] decodes a [watchdog_config] value from [decoder] *)

val decode_worker_heartbeat_request : Pbrt.Decoder.t -> Event_types.worker_heartbeat_request
(** [decode_worker_heartbeat_request decoder] decodes a [worker_heartbeat_request] value from [decoder] *)

val decode_worker_heartbeat_response : Pbrt.Decoder.t -> Event_types.worker_heartbeat_response
(** [decode_worker_heartbeat_response decoder] decodes a [worker_heartbeat_response] value from [decoder] *)
