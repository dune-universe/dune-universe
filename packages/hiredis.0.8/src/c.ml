type t = Hiredis_value.t

type status =
    | OK
    | ERR of string option

type context
external redis_context_errstr : context -> string option = "redis_context_errstr"
external redis_context_to_fd : context -> Unix.file_descr = "redis_context_to_fd"
external redis_context_of_fd : Unix.file_descr -> context = "redis_context_of_fd"
external redis_context_of_fd_i : int -> context = "redis_context_of_fd"
external redis_context_free_keep_fd : context -> unit = "redis_context_free_keep_fd"
external redis_context_connect : string -> int -> bool -> context = "redis_context_connect"
external redis_context_connect_unix : string -> bool -> context = "redis_context_connect_unix"
external redis_context_reconnect : context -> status = "redis_context_reconnect"
external redis_context_set_timeout : context -> int -> int -> status = "redis_context_set_timeout"
external redis_context_enable_keepalive : context -> status = "redis_context_enable_keepalive"
external redis_context_command : context -> string array -> t = "redis_context_command"
external redis_context_append_command : context -> string array -> status = "redis_context_append_command"
external redis_context_append_formatted : context -> string -> status = "redis_context_append_formatted"
external redis_context_free : context -> unit = "redis_context_free"
external redis_context_flush_buffer : context -> status = "redis_context_flush_buffer"
external redis_context_read_buffer : context -> status = "redis_context_read_buffer"
external redis_context_get_reply : context -> t option = "redis_context_get_reply"

external redis_format_command : string array -> string option = "redis_format_command"

type reader
external redis_reader_create : unit -> reader = "redis_reader_create"
external redis_reader_free : reader -> unit = "redis_reader_free"
external redis_reader_feed : reader -> string -> status = "redis_reader_feed"
external redis_reader_get_reply : reader -> t option = "redis_reader_get_reply"

