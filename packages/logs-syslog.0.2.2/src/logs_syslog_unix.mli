(** Logs reporter via syslog using Unix

    Please read {!Logs_syslog} first. *)

(** [udp_reporter ~hostname remote_ip ~port ~truncate ()] is [reporter], which
    sends log message to [remote_ip, port] via UDP.  Each message is truncated
    to [truncate] bytes (defaults to 65535).  The [hostname] is part of each
    syslog message, and defaults to [Unix.gethostname ()], the [port] defaults
    to 514. [facility] is the default syslog facility (see
    {!Logs_syslog.message}). *)
val udp_reporter :
  ?hostname:string -> Unix.inet_addr -> ?port:int -> ?truncate:int ->
  ?facility:Syslog_message.facility -> unit ->
  Logs.reporter

(** [tcp_reporter ~hostname remote_ip ~port ~truncate ~framing ()] is
    [Ok reporter] or [Error msg].  The [reporter] sends each log message via syslog
    to [remote_ip, port] via TCP.  If the initial TCP connection to the
    [remote_ip] fails, an [Error msg] is returned instead.  If the TCP
    connection fails, the log message is reported to standard error, and
    attempts are made to re-establish the TCP connection.  A syslog message is
    truncated to [truncate] bytes (by default no truncation happens). Each
    syslog message is framed according to the given [framing] (defaults to a
    single zero byte).  The [hostname] defaults to [Unix.gethostname ()], [port]
    to 514, [framing] to append a zero byte. [facility] is the default syslog
    facility (see {!Logs_syslog.message}). *)
val tcp_reporter : ?hostname:string -> Unix.inet_addr -> ?port:int ->
  ?truncate:int ->
  ?framing:Logs_syslog.framing ->
  ?facility:Syslog_message.facility -> unit ->
  (Logs.reporter, string) result

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
  (Logs.reporter, string) result

(** {2:unix_example Example usage}

    To install a Unix syslog reporter. sending via UDP to localhost, use the
    following snippet:

{[
Logs.set_reporter (udp_reporter (Unix.inet_addr_of_string "127.0.0.1") ())
]}

    To install a reporter using TCP, use the following snippet:
{[
let () =
  match tcp_reporter (Unix.inet_addr_of_string "127.0.0.1") () with
  | Error e -> print_endline e
  | Ok r -> Logs.set_reporter r
]}

*)
