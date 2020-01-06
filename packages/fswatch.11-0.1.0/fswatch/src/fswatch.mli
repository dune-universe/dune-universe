module Status : sig
  type t =
    | FSW_OK
    | FSW_ERR_UNKNOWN_ERROR
    | FSW_ERR_SESSION_UNKNOWN
    | FSW_ERR_MONITOR_ALREADY_EXISTS
    | FSW_ERR_MEMORY
    | FSW_ERR_UNKNOWN_MONITOR_TYPE
    | FSW_ERR_CALLBACK_NOT_SET
    | FSW_ERR_PATHS_NOT_SET
    | FSW_ERR_MISSING_CONTEXT
    | FSW_ERR_INVALID_PATH
    | FSW_ERR_INVALID_CALLBACK
    | FSW_ERR_INVALID_LATENCY
    | FSW_ERR_INVALID_REGEX
    | FSW_ERR_MONITOR_ALREADY_RUNNING
    | FSW_ERR_UNKNOWN_VALUE
    | FSW_ERR_INVALID_PROPERTY

  val t_to_string : t -> string
  val t_of_string : string -> t
end

module Event : sig
  type flag =
    | NoOp
    | PlatformSpecific
    | Created
    | Updated
    | Removed
    | Renamed
    | OwnerModified
    | AttributeModified
    | MovedFrom
    | MovedTo
    | IsFile
    | IsDir
    | IsSymLink
    | Link
    | Overflow

  type t = {
    path : string;
    time : float;
    flags : flag array;
  }

  val t_to_string : t -> string

  type callback = t array -> unit
end

module Monitor : sig
  type t =
    | System_default
    | Fsevents
    | Kqueue
    | Inotify
    | Windows
    | Poll
    | Fen
end

module Filter : sig
  type filter_type = Include | Exclude

  type monitor_filter = {
    text : string;
    filter_type : filter_type;
    case_sensitive : bool;
    extended : bool;
  }

  type event_type_filter = {
    flag : Event.flag;
  }
end

type handle

val init_library : unit -> Status.t
val init_session : Monitor.t -> Event.callback -> handle
val add_path : handle -> string -> unit
val add_property : handle -> name:string -> value:string -> unit
val set_allow_overflow : handle -> bool -> unit
val set_recursive : handle -> bool -> unit
val set_directory_only : handle -> bool -> unit
val set_follow_symlinks : handle -> bool -> unit
val add_event_type_filter : handle -> Filter.event_type_filter -> unit
val add_filter : handle -> Filter.monitor_filter -> unit
val start_monitor : handle -> unit
val start_monitor_thread : handle -> Thread.t
val stop_monitor : handle -> unit
val is_running : handle -> bool
val destroy_session : handle -> unit
val last_status : handle -> Status.t

val last_error : unit -> Status.t
val is_verbose : unit -> bool
val set_verbose : bool -> unit

