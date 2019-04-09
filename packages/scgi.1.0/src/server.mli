(** The SCGI server *)
type server_name = string

type inet_addr = string

type port = int

type socket_filename = string

val handler_inet :
     ?read_timeout:float (* default: 60. seconds *)
  -> ?processing_timeout:float (* default: 300. seconds *)
  -> ?write_timeout:float (* default: 60. seconds *)
  -> ?write_error_handler:(exn -> unit Lwt.t)
  -> ?fd:Lwt_unix.file_descr
  -> ?buffer_size:int
  -> ?backlog:int
  -> ?no_close:bool
  -> inet_addr
  -> port
  -> (Request.t -> Response.t Lwt.t)
  -> Lwt_io.server Lwt.t
(** Launch an SCGI server listening on an Internet socket. *)

val handler_sock :
     ?read_timeout:float (* default: 60. seconds *)
  -> ?processing_timeout:float (* default: 300. seconds *)
  -> ?write_timeout:float (* default: 60. seconds *)
  -> ?write_error_handler:(exn -> unit Lwt.t)
  -> ?fd:Lwt_unix.file_descr
  -> ?buffer_size:int
  -> ?backlog:int
  -> ?no_close:bool
  -> socket_filename
  -> (Request.t -> Response.t Lwt.t)
  -> Lwt_io.server Lwt.t
(** Launch an SCGI server listening on a Unix-domain socket. You may want to
    block the SIGPIPE signal with
    [Sys.set_signal Sys.sigpipe Sys.Signal_ignore] in order to avoid process
    termination with exit code 141 (on Linux at least) when a client closes its
    connection too early. *)
