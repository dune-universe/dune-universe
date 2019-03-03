(** Logs reporter via syslog, using Lwt

    Please read {!Logs_syslog} first. *)

(** [udp_reporter ~hostname remote_ip ~port ~truncate ()] is [reporter], which
    sends syslog message using the given [hostname] to [remote_ip, remote_port]
    via UDP.  Each message is truncated to [truncate] bytes (defaults to 65535).
    The [hostname] default to [Lwt_unix.gethostname ()], [port] defaults to
    514. [facility] is the default syslog facility (see
    {!Logs_syslog.message}). *)
val udp_reporter :
  ?hostname:string -> Lwt_unix.inet_addr -> ?port:int -> ?truncate:int ->
  ?facility:Syslog_message.facility -> unit ->
  Logs.reporter Lwt.t

(** [tcp_reporter ~hostname remote_ip ~port ~truncate ~framing ()] is
    [Ok reporter] or [Error msg].  The [reporter] sends each log message to
    [remote_ip, port] via TCP.  If the initial TCP connection to the [remote_ip]
    fails, an [Error msg] is returned instead.  If the TCP connection fails, the
    log message is reported to standard error, and attempts are made to
    re-establish the TCP connection.  Each syslog message is truncated to
    [truncate] bytes (defaults to 0, thus no truncation).  Each syslog message
    is framed (using [framing]), the default strategy is to append a single byte
    containing 0.  The [hostname] default to [Lwt_unix.gethostname ()], [port]
    to 514. [facility] is the default syslog facility (see
    {!Logs_syslog.message}). *)
val tcp_reporter : ?hostname:string -> Lwt_unix.inet_addr -> ?port:int ->
  ?truncate:int ->
  ?framing:Logs_syslog.framing ->
  ?facility:Syslog_message.facility -> unit ->
  (Logs.reporter, string) result Lwt.t

(** [unix_reporter ~socket ~truncate ~framing ()] is [Ok reporter] or
    [Error msg]. The [reporter] sends each log message via syslog to [socket]
    (which defaults to ["/dev/log"]). If the initial connection to the socket
    fails, the log message is reported to standard error, and attempts are made
    to re-establish the connection. A syslog message is truncated to [truncate]
    bytes and is framed according to the given [framing]. The default for
    [truncate] is [65536] if [framing] is not provided and [0] otherwise. If
    [framing] is not provided, then the socket used is a datagram socket (as
    for {!udp_reporter}) otherwise a stream socket is used (as for
    {!tcp_reporter}). [facility] is the default syslog facility (see
    {!Logs_syslog.message}). *)
val unix_reporter : ?socket:string ->
  ?truncate:int ->
  ?framing:Logs_syslog.framing ->
  ?facility:Syslog_message.facility -> unit ->
  (Logs.reporter, string) result Lwt.t

(** {2:lwt_example Example usage}

    To install a Lwt syslog reporter, sending via UDP to localhost, use the
    following snippet:
{[
let install_logger () =
  udp_reporter (Unix.inet_addr_of_string "127.0.0.1") () >|= fun r ->
  Logs.set_reporter r

let _ = Lwt_main.run (install_logger ())
]}

    And via TCP:
{[
let install_logger () =
  tcp_reporter (Unix.inet_addr_of_string "127.0.0.1") () >|= function
    | Ok r -> Logs.set_reporter r
    | Error e -> print_endline e

let _ = Lwt_main.run (install_logger ())
]}

*)
