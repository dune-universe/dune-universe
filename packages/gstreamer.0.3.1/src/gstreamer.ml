exception Error of string
exception Stopped
exception Timeout
exception Failed
exception End_of_stream

let () =
  Callback.register_exception "gstreamer_exn_error" (Error "");
  Callback.register_exception "gstreamer_exn_stopped" Stopped;
  Callback.register_exception "gstreamer_exn_timeout" Timeout;
  Callback.register_exception "gstreamer_exn_failed" Failed;
  Callback.register_exception "gstreamer_exn_eos" End_of_stream

external init : string array option -> unit = "ocaml_gstreamer_init"

let init ?argv () = init argv

external deinit : unit -> unit = "ocaml_gstreamer_deinit"
external version : unit -> int * int * int * int = "ocaml_gstreamer_version"
external version_string : unit -> string = "ocaml_gstreamer_version_string"

type data =
  (int, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module Format = struct
  type t = Undefined | Default | Bytes | Time | Buffers | Percent

  external to_string : t -> string = "ocaml_gstreamer_format_to_string"
end

module Event = struct
  type seek_flag =
    | Seek_flag_none
    | Seek_flag_flush
    | Seek_flag_accurate
    | Seek_flag_key_unit
    | Seek_flag_segment
    | Seek_flag_skip
    | Seek_flag_snap_before
    | Seek_flag_snap_after
    | Seek_flag_snap_nearest
end

module Element = struct
  type t

  external set_property_string : t -> string -> string -> unit
    = "ocaml_gstreamer_element_set_property_string"

  external set_property_int : t -> string -> string -> unit
    = "ocaml_gstreamer_element_set_property_int"

  external set_property_bool : t -> string -> string -> unit
    = "ocaml_gstreamer_element_set_property_bool"

  type state =
    | State_void_pending
    | State_null
    | State_ready
    | State_paused
    | State_playing

  external string_of_state : state -> string
    = "ocaml_gstreamer_element_string_of_state"

  type state_change =
    | State_change_success
    | State_change_async
    | State_change_no_preroll

  external set_state : t -> state -> state_change
    = "ocaml_gstreamer_element_set_state"

  external get_state : t -> state_change * state * state
    = "ocaml_gstreamer_element_get_state"

  external link : t -> t -> unit = "ocaml_gstreamer_element_link"

  let link_many ee =
    let e, ee = (List.hd ee, List.tl ee) in
    ignore
      (List.fold_left
         (fun e e' ->
           link e e';
           e')
         e ee)

  external position : t -> Format.t -> Int64.t
    = "ocaml_gstreamer_element_position"

  external duration : t -> Format.t -> Int64.t
    = "ocaml_gstreamer_element_duration"

  external seek_simple :
    t -> Format.t -> Event.seek_flag array -> Int64.t -> unit
    = "ocaml_gstreamer_element_seek_simple"

  let seek_simple e fmt flags n = seek_simple e fmt (Array.of_list flags) n
end

module Element_factory = struct
  type t = Element.t

  external make : string -> string -> t = "ocaml_gstreamer_element_factory_make"
end

module Message = struct
  type message_type =
    | Unknown
    | End_of_stream
    | Error
    | Warning
    | Info
    | Tag
    | Buffering
    | State_changed
    | State_dirty
    | Step_done
    | Clock_provide
    | Clock_lost
    | New_clock
    | Structure_change
    | Stream_status
    | Application
    | Element
    | Segment_start
    | Segment_done
    | Duration_changed
    | Latency
    | Async_start
    | Async_done
    | Request_state
    | Step_start
    | Qos
    | Progress
    | Toc
    | Reset_time
    | Stream_start
    | Need_context
    | Have_context
    | Any

  type t

  type msg =
    [ `Unknown
    | `End_of_stream
    | `Error of string
    | `Warning of string
    | `Info of string
    | `Tag of (string * string list) list
    | `Buffering of int
    | `State_changed of Element.state * Element.state * Element.state
    | `State_dirty
    | `Step_done
    | `Clock_provide
    | `Clock_lost
    | `New_clock
    | `Structure_change
    | `Stream_status
    | `Application
    | `Element
    | `Segment_start
    | `Segment_done
    | `Duration_changed
    | `Latency
    | `Async_start
    | `Async_done
    | `Request_state
    | `Step_start
    | `Qos
    | `Progress
    | `Toc
    | `Reset_time
    | `Stream_start
    | `Need_context
    | `Have_context ]

  external message_type : t -> message_type = "ocaml_gstreamer_message_type"
  external source_name : t -> string = "ocaml_gstreamer_message_source_name"

  external parse_tag : t -> (string * string array) array
    = "ocaml_gstreamer_message_parse_tag"

  let parse_tag msg =
    let tags = parse_tag msg in
    let tags = Array.map (fun (l, v) -> (l, Array.to_list v)) tags in
    Array.to_list tags

  external parse_error : t -> string = "ocaml_gstreamer_message_parse_error"

  external parse_state_changed :
    t -> Element.state * Element.state * Element.state
    = "ocaml_gstreamer_message_parse_state_changed"

  external parse_buffering : t -> int
    = "ocaml_gstreamer_message_parse_buffering"

  let message_of_msg msg =
    match message_type msg with
      | Unknown -> `Unknown
      | End_of_stream -> `End_of_stream
      | Error -> `Error (parse_error msg)
      | Warning -> `Warning (parse_error msg)
      | Info -> `Info (parse_error msg)
      | Tag -> `Tag (parse_tag msg)
      | Buffering -> `Buffering (parse_buffering msg)
      | State_changed -> `State_changed (parse_state_changed msg)
      | State_dirty -> `State_dirty
      | Step_done -> `Step_done
      | Clock_provide -> `Clock_provide
      | Clock_lost -> `Clock_lost
      | New_clock -> `New_clock
      | Structure_change -> `Structure_change
      | Stream_status -> `Stream_status
      | Application -> `Application
      | Element -> `Element
      | Segment_start -> `Segment_start
      | Segment_done -> `Segment_done
      | Duration_changed -> `Duration_changed
      | Latency -> `Latency
      | Async_start -> `Async_start
      | Async_done -> `Async_done
      | Request_state -> `Request_state
      | Step_start -> `Step_start
      | Qos -> `Qos
      | Progress -> `Progress
      | Toc -> `Toc
      | Reset_time -> `Reset_time
      | Stream_start -> `Stream_start
      | Need_context -> `Need_context
      | Have_context -> `Have_context
      | Any -> assert false
end

module Loop = struct
  type t

  external create : unit -> t = "ocaml_gstreamer_loop_create"
  external run : t -> unit = "ocaml_gstreamer_loop_run"
  external quit : t -> unit = "ocaml_gstreamer_loop_quit"
end

module Bus = struct
  type message_payload = Message.msg
  type message = { source : string; payload : message_payload }
  type t

  type message_type =
    [ `Unknown
    | `End_of_stream
    | `Error
    | `Warning
    | `Info
    | `Tag
    | `Buffering
    | `State_changed
    | `State_dirty
    | `Step_done
    | `Clock_provide
    | `Clock_lost
    | `New_clock
    | `Structure_change
    | `Stream_status
    | `Application
    | `Element
    | `Segment_start
    | `Segment_done
    | `Duration_changed
    | `Latency
    | `Async_start
    | `Async_done
    | `Request_state
    | `Step_start
    | `Qos
    | `Progress
    | `Toc
    | `Reset_time
    | `Stream_start
    | `Need_context
    | `Have_context
    | `Any ]

  external of_element : Element.t -> t = "ocaml_gstreamer_bus_of_element"

  let type_of_mesage_type = function
    | `Unknown -> Message.Unknown
    | `End_of_stream -> Message.End_of_stream
    | `Error -> Message.Error
    | `Warning -> Message.Warning
    | `Info -> Message.Info
    | `Tag -> Message.Tag
    | `Buffering -> Message.Buffering
    | `State_changed -> Message.State_changed
    | `State_dirty -> Message.State_dirty
    | `Step_done -> Message.Step_done
    | `Clock_provide -> Message.Clock_provide
    | `Clock_lost -> Message.Clock_lost
    | `New_clock -> Message.New_clock
    | `Structure_change -> Message.Structure_change
    | `Stream_status -> Message.Stream_status
    | `Application -> Message.Application
    | `Element -> Message.Element
    | `Segment_start -> Message.Segment_start
    | `Segment_done -> Message.Segment_done
    | `Duration_changed -> Message.Duration_changed
    | `Latency -> Message.Latency
    | `Async_start -> Message.Async_start
    | `Async_done -> Message.Async_done
    | `Request_state -> Message.Request_state
    | `Step_start -> Message.Step_start
    | `Qos -> Message.Qos
    | `Progress -> Message.Progress
    | `Toc -> Message.Toc
    | `Reset_time -> Message.Reset_time
    | `Stream_start -> Message.Stream_start
    | `Need_context -> Message.Need_context
    | `Have_context -> Message.Have_context
    | `Any -> Message.Any

  let parse_msg msg =
    { source = Message.source_name msg; payload = Message.message_of_msg msg }

  let any_message = function None -> None | Some msg -> Some (parse_msg msg)

  external pop_filtered : t -> Message.message_type array -> Message.t option
    = "ocaml_gstreamer_bus_pop_filtered"

  let pop_filtered bus filter =
    any_message
      (pop_filtered bus (Array.of_list (List.map type_of_mesage_type filter)))

  external timed_pop_filtered :
    t -> ?timeout:Int64.t -> Message.message_type array -> Message.t
    = "ocaml_gstreamer_bus_timed_pop_filtered"

  let timed_pop_filtered bus ?timeout filter =
    parse_msg
      (timed_pop_filtered bus ?timeout
         (Array.of_list (List.map type_of_mesage_type filter)))
end

module Bin = struct
  type t = Element.t

  let of_element e = e

  external add : t -> Element.t -> unit = "ocaml_gstreamer_bin_add"

  let add_many bin e = List.iter (add bin) e

  external get_by_name : t -> string -> Element.t
    = "ocaml_gstreamer_bin_get_by_name"
end

module Pipeline = struct
  type t = Element.t

  external create : string -> t = "ocaml_gstreamer_pipeline_create"
  external parse_launch : string -> t = "ocaml_gstreamer_pipeline_parse_launch"
end

module Buffer = struct
  type t

  (* Not allowing the two below for now because they are quite unsafe. *)
  (* external create : int -> t = "ocaml_gstreamer_buffer_create" *)
  (* external set_data : t -> int -> data -> int -> int -> unit = "ocaml_gstreamer_buffer_set_data" *)

  external of_string : string -> int -> int -> t
    = "ocaml_gstreamer_buffer_of_string"

  external of_data : data -> int -> int -> t = "ocaml_gstreamer_buffer_of_data"

  external of_data_list : (data * int * int) list -> t
    = "ocaml_gstreamer_buffer_of_data_list"

  external to_data : t -> data = "ocaml_gstreamer_buffer_to_data"
  external to_string : t -> string = "ocaml_gstreamer_buffer_to_string"

  external set_presentation_time : t -> Int64.t -> unit
    = "ocaml_gstreamer_buffer_set_presentation_time"

  external set_decoding_time : t -> Int64.t -> unit
    = "ocaml_gstreamer_buffer_set_decoding_time"

  external set_duration : t -> Int64.t -> unit
    = "ocaml_gstreamer_buffer_set_duration"
end

module App_src = struct
  type t

  external of_element : Element.t -> t = "ocaml_gstreamer_appsrc_of_element"
  external to_element : t -> Element.t = "ocaml_gstreamer_appsrc_to_element"

  external push_buffer : t -> Buffer.t -> unit
    = "ocaml_gstreamer_appsrc_push_buffer"

  external push_buffer_bytes :
    t -> Int64.t -> Int64.t -> bytes -> int -> int -> unit
    = "ocaml_gstreamer_appsrc_push_buffer_bytes_b" "ocaml_gstreamer_appsrc_push_buffer_bytes_n"

  let push_buffer_bytes src ?(presentation_time = Int64.minus_one)
      ?(duration = Int64.minus_one) data ofs len =
    push_buffer_bytes src presentation_time duration data ofs len

  external push_buffer_data :
    t -> Int64.t -> Int64.t -> data -> int -> int -> unit
    = "ocaml_gstreamer_appsrc_push_buffer_data_b" "ocaml_gstreamer_appsrc_push_buffer_data_n"

  let push_buffer_data src ?(presentation_time = Int64.minus_one)
      ?(duration = Int64.minus_one) data ofs len =
    push_buffer_data src presentation_time duration data ofs len

  external on_need_data : t -> (int -> unit) -> unit
    = "ocaml_gstreamer_appsrc_connect_need_data"

  external end_of_stream : t -> unit = "ocaml_gstreamer_appsrc_end_of_stream"

  external set_format : t -> Format.t -> unit
    = "ocaml_gstreamer_appsrc_set_format"
end

module App_sink = struct
  type t

  external of_element : Element.t -> t = "ocaml_gstreamer_appsink_of_element"
  external pull_buffer : t -> Buffer.t = "ocaml_gstreamer_appsink_pull_buffer"

  let pull_buffer_data sink = Buffer.to_data (pull_buffer sink)
  let pull_buffer_string sink = Buffer.to_string (pull_buffer sink)

  external emit_signals : t -> unit = "ocaml_gstreamer_appsink_emit_signals"
  external is_eos : t -> bool = "ocaml_gstreamer_appsink_is_eos"

  external on_new_sample : t -> (unit -> unit) -> unit
    = "ocaml_gstreamer_appsink_connect_new_sample"

  external set_max_buffers : t -> int -> unit
    = "ocaml_gstreamer_appsink_set_max_buffers"
end

module Caps = struct
  type t

  external to_string : t -> string = "ocaml_gstreamer_caps_to_string"
end

module Type_find_element = struct
  type t

  external of_element : Element.t -> t
    = "ocaml_gstreamer_typefind_element_of_element"

  external on_have_type : t -> (int -> Caps.t -> unit) -> unit
    = "ocaml_gstreamer_typefind_element_connect_have_type"
end

module Tag_setter = struct
  type t = Element.t

  type merge_mode =
    | Undefined
    | Replace_all
    | Replace
    | Append
    | Prepend
    | Keep
    | Keep_all
    | Count

  let of_element e = e

  external add_tag : t -> merge_mode -> string -> string -> unit
    = "ocaml_gstreamer_tag_setter_add_tag"
end
