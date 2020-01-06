(*
 * stub.ml
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of ocaml-fswatch.
 *)

module Status = struct
  type t=
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

  let t_to_int= function
    | FSW_OK                          -> 0
    | FSW_ERR_UNKNOWN_ERROR           -> 1 lsl 0
    | FSW_ERR_SESSION_UNKNOWN         -> 1 lsl 1
    | FSW_ERR_MONITOR_ALREADY_EXISTS  -> 1 lsl 2
    | FSW_ERR_MEMORY                  -> 1 lsl 3
    | FSW_ERR_UNKNOWN_MONITOR_TYPE    -> 1 lsl 4
    | FSW_ERR_CALLBACK_NOT_SET        -> 1 lsl 5
    | FSW_ERR_PATHS_NOT_SET           -> 1 lsl 6
    | FSW_ERR_MISSING_CONTEXT         -> 1 lsl 7
    | FSW_ERR_INVALID_PATH            -> 1 lsl 8
    | FSW_ERR_INVALID_CALLBACK        -> 1 lsl 9
    | FSW_ERR_INVALID_LATENCY         -> 1 lsl 10
    | FSW_ERR_INVALID_REGEX           -> 1 lsl 11
    | FSW_ERR_MONITOR_ALREADY_RUNNING -> 1 lsl 12
    | FSW_ERR_UNKNOWN_VALUE           -> 1 lsl 13
    | FSW_ERR_INVALID_PROPERTY        -> 1 lsl 14

  let t_of_int n=
    if n = 0        then FSW_OK                          else
    if n = 1 lsl 0  then FSW_ERR_UNKNOWN_ERROR           else
    if n = 1 lsl 1  then FSW_ERR_SESSION_UNKNOWN         else
    if n = 1 lsl 2  then FSW_ERR_MONITOR_ALREADY_EXISTS  else
    if n = 1 lsl 3  then FSW_ERR_MEMORY                  else
    if n = 1 lsl 4  then FSW_ERR_UNKNOWN_MONITOR_TYPE    else
    if n = 1 lsl 5  then FSW_ERR_CALLBACK_NOT_SET        else
    if n = 1 lsl 6  then FSW_ERR_PATHS_NOT_SET           else
    if n = 1 lsl 7  then FSW_ERR_MISSING_CONTEXT         else
    if n = 1 lsl 8  then FSW_ERR_INVALID_PATH            else
    if n = 1 lsl 9  then FSW_ERR_INVALID_CALLBACK        else
    if n = 1 lsl 10 then FSW_ERR_INVALID_LATENCY         else
    if n = 1 lsl 11 then FSW_ERR_INVALID_REGEX           else
    if n = 1 lsl 12 then FSW_ERR_MONITOR_ALREADY_RUNNING else
    if n = 1 lsl 13 then FSW_ERR_UNKNOWN_VALUE           else
    if n = 1 lsl 14 then FSW_ERR_INVALID_PROPERTY        else
    failwith "status_of_int"

  let t_to_string= function
    | FSW_OK-> "FSW_OK"
    | FSW_ERR_UNKNOWN_ERROR-> "FSW_ERR_UNKNOWN_ERROR"
    | FSW_ERR_SESSION_UNKNOWN-> "FSW_ERR_SESSION_UNKNOWN"
    | FSW_ERR_MONITOR_ALREADY_EXISTS-> "FSW_ERR_MONITOR_ALREADY_EXISTS"
    | FSW_ERR_MEMORY-> "FSW_ERR_MEMORY"
    | FSW_ERR_UNKNOWN_MONITOR_TYPE-> "FSW_ERR_UNKNOWN_MONITOR_TYPE"
    | FSW_ERR_CALLBACK_NOT_SET-> "FSW_ERR_CALLBACK_NOT_SET"
    | FSW_ERR_PATHS_NOT_SET-> "FSW_ERR_PATHS_NOT_SET"
    | FSW_ERR_MISSING_CONTEXT-> "FSW_ERR_MISSING_CONTEXT"
    | FSW_ERR_INVALID_PATH-> "FSW_ERR_INVALID_PATH"
    | FSW_ERR_INVALID_CALLBACK-> "FSW_ERR_INVALID_CALLBACK"
    | FSW_ERR_INVALID_LATENCY-> "FSW_ERR_INVALID_LATENCY"
    | FSW_ERR_INVALID_REGEX-> "FSW_ERR_INVALID_REGEX"
    | FSW_ERR_MONITOR_ALREADY_RUNNING-> "FSW_ERR_MONITOR_ALREADY_RUNNING"
    | FSW_ERR_UNKNOWN_VALUE-> "FSW_ERR_UNKNOWN_VALUE"
    | FSW_ERR_INVALID_PROPERTY-> "FSW_ERR_INVALID_PROPERTY"

  let t_of_string str=
    match String.lowercase_ascii str with
    | "fsw_ok"-> FSW_OK
    | "fsw_err_unknown_error"-> FSW_ERR_UNKNOWN_ERROR
    | "fsw_err_session_unknown"-> FSW_ERR_SESSION_UNKNOWN
    | "fsw_err_monitor_already_exists"-> FSW_ERR_MONITOR_ALREADY_EXISTS
    | "fsw_err_memory"-> FSW_ERR_MEMORY
    | "fsw_err_unknown_monitor_type"-> FSW_ERR_UNKNOWN_MONITOR_TYPE
    | "fsw_err_callback_not_set"-> FSW_ERR_CALLBACK_NOT_SET
    | "fsw_err_paths_not_set"-> FSW_ERR_PATHS_NOT_SET
    | "fsw_err_missing_context"-> FSW_ERR_MISSING_CONTEXT
    | "fsw_err_invalid_path"-> FSW_ERR_INVALID_PATH
    | "fsw_err_invalid_callback"-> FSW_ERR_INVALID_CALLBACK
    | "fsw_err_invalid_latency"-> FSW_ERR_INVALID_LATENCY
    | "fsw_err_invalid_regex"-> FSW_ERR_INVALID_REGEX
    | "fsw_err_monitor_already_running"-> FSW_ERR_MONITOR_ALREADY_RUNNING
    | "fsw_err_unknown_value"-> FSW_ERR_UNKNOWN_VALUE
    | "fsw_err_invalid_property"-> FSW_ERR_INVALID_PROPERTY
    | _-> failwith "t_of_string"
end

module Event = struct
  type flag=
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

  let flag_to_int= function
    | NoOp              -> 0
    | PlatformSpecific  -> 1 lsl 0
    | Created           -> 1 lsl 1
    | Updated           -> 1 lsl 2
    | Removed           -> 1 lsl 3
    | Renamed           -> 1 lsl 4
    | OwnerModified     -> 1 lsl 5
    | AttributeModified -> 1 lsl 6
    | MovedFrom         -> 1 lsl 7
    | MovedTo           -> 1 lsl 8
    | IsFile            -> 1 lsl 9
    | IsDir             -> 1 lsl 10
    | IsSymLink         -> 1 lsl 11
    | Link              -> 1 lsl 12
    | Overflow          -> 1 lsl 13

  let flag_of_int n=
    if n = 0        then NoOp              else
    if n = 1 lsl 0  then PlatformSpecific  else
    if n = 1 lsl 1  then Created           else
    if n = 1 lsl 2  then Updated           else
    if n = 1 lsl 3  then Removed           else
    if n = 1 lsl 4  then Renamed           else
    if n = 1 lsl 5  then OwnerModified     else
    if n = 1 lsl 6  then AttributeModified else
    if n = 1 lsl 7  then MovedFrom         else
    if n = 1 lsl 8  then MovedTo           else
    if n = 1 lsl 9  then IsFile            else
    if n = 1 lsl 10 then IsDir             else
    if n = 1 lsl 11 then IsSymLink         else
    if n = 1 lsl 12 then Link              else
    if n = 1 lsl 13 then Overflow          else
      failwith "flag_of_int"

  let flag_to_string= function
    | NoOp-> "NoOp"
    | PlatformSpecific-> "PlatformSpecific"
    | Created-> "Created"
    | Updated-> "Updated"
    | Removed-> "Removed"
    | Renamed-> "Renamed"
    | OwnerModified-> "OwnerModified"
    | AttributeModified-> "AttributeModified"
    | MovedFrom-> "MovedFrom"
    | MovedTo-> "MovedTo"
    | IsFile-> "IsFile"
    | IsDir-> "IsDir"
    | IsSymLink-> "IsSymLink"
    | Link-> "Link"
    | Overflow-> "Overflow"

  let flag_of_string str=
    match String.lowercase_ascii str with
    | "noop"-> NoOp
    | "platformspecific"-> PlatformSpecific
    | "created"-> Created
    | "updated"-> Updated
    | "removed"-> Removed
    | "renamed"-> Renamed
    | "ownermodified"-> OwnerModified
    | "attributemodified"-> AttributeModified
    | "movedfrom"-> MovedFrom
    | "movedto"-> MovedTo
    | "isfile"-> IsFile
    | "isdir"-> IsDir
    | "issymlink"-> IsSymLink
    | "link"-> Link
    | "overflow"-> Overflow
    | _-> failwith "flag_of_string"

  type raw= {
    path: string;
    time: float; (* the semantic is as the same as [Unix.time ()] *)
    flags: int array;
  }

  type t= {
    path: string;
    time: float; (* the semantic is as the same as [Unix.time ()] *)
    flags: flag array;
  }

  let t_of_raw (raw:raw)=
    { path= raw.path;
      time= raw.time;
      flags= Array.map flag_of_int raw.flags
    }

  let t_to_string t= Printf.sprintf
  "{
  path= %s;
  time= %f;
  flags= [|%s|];\n}"
  t.path
  t.time
  (t.flags
    |> Array.map flag_to_string
    |> Array.to_list
    |> String.concat "; ")

  type callback= t array -> unit
end

module Monitor = struct
  type t=
    | System_default
    | Fsevents
    | Kqueue
    | Inotify
    | Windows
    | Poll
    | Fen

  let t_to_int= function
    | System_default -> 0
    | Fsevents       -> 1
    | Kqueue         -> 2
    | Inotify        -> 3
    | Windows        -> 4
    | Poll           -> 5
    | Fen            -> 6

  let t_of_int= function
    | 0-> System_default
    | 1-> Fsevents
    | 2-> Kqueue
    | 3-> Inotify
    | 4-> Windows
    | 5-> Poll
    | 6-> Fen
    | _-> failwith "t_of_int"
end

module Filter = struct
  type filter_type=
    | Include
    | Exclude

  type cmonitor_filter= {
    text: string;
    filter_type: filter_type;
    case_sensitive: bool;
    extended: bool;
  }

  type event_type_filter_raw= {
    flag: int;
  }

  type event_type_filter= {
    flag: Event.flag;
  }

  let event_type_filter_to_raw filter= ({
    flag=  Event.flag_to_int filter.flag;
  }:event_type_filter_raw)
end

module FSW = struct
  type handle= nativeint
  type status= int

  external init_library: unit -> status= "fsw_init_library_stub"
  external init_session: Monitor.t -> handle= "fsw_init_session_stub"
  external add_path: handle -> string -> status= "fsw_add_path_stub" 
  external add_property: handle -> name:string -> value:string -> status = "fsw_add_property_stub"
  external set_allow_overflow: handle -> bool -> status= "fsw_set_allow_overflow_stub"
(*   external set_callback: handle -> Event.callback -> status= "fsw_set_callback_stub" *)
  external set_latency: handle -> float -> status= "fsw_set_latency_stub"
  external set_recursive: handle -> bool -> status= "fsw_set_recursive_stub"
  external set_directory_only: handle -> bool -> status= "fsw_set_directory_only_stub"
  external set_follow_symlinks: handle-> bool -> status= "fsw_set_follow_symlinks_stub"
  external add_event_type_filter: handle -> Filter.event_type_filter_raw -> status= "fsw_add_event_type_filter_stub"
  external add_filter: handle -> Filter.cmonitor_filter -> status= "fsw_add_filter_stub"
  external start_monitor: handle -> status= "fsw_start_monitor_stub"
  external stop_monitor: handle -> status= "fsw_stop_monitor_stub"
  external is_running: handle -> bool= "fsw_is_running_stub"
  external destroy_session: handle -> status= "fsw_destroy_session_stub"
  external last_error: unit -> status= "fsw_last_error_stub"
  external is_verbose: unit -> bool= "fsw_is_verbose_stub"
  external set_verbose: bool -> unit= "fsw_set_verbose_stub"
end

