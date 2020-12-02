(** Logs reporter via syslog using Lwt and TLS

    Please read {!Logs_syslog} first. *)

(** [tcp_tls_reporter ~hostname remote_ip ~port ~cacert ~cert ~priv_key ~truncate ~framing ()]
    is [Ok reporter] or [Error msg].  The TLS connection
    validates the certificate of the log server, it must be signed by [cacert].
    The reporters credentials are its public [cert], and its [priv_key].  The
    [reporter] sends each log message to [remote_ip, port] via TLS.  If the
    initial TLS connection to the [remote_ip] fails, an [Error msg] is returned
    instead.  If the TLS connection fails, the log message is reported to
    standard error, and an attempt is made to re-establish the TLS connection.
    Each message can be truncated after [truncate] bytes (defaults to no
    truncation).  Each message is framed: by default a single byte containing 0
    is appended, depending on [framing], its length could be prepended, as
    specified in {{:https://tools.ietf.org/html/rfc5125}RFC 5125}.  The default
    value for [hostname] is [Lwt_unix.gethostname ()], the default value for
    [port] is 6514. [facility] is the default syslog facility (see
    {!Logs_syslog.message}). *)
val tcp_tls_reporter : ?hostname:string -> Lwt_unix.inet_addr -> ?port:int ->
  cacert:string -> cert:string -> priv_key:string ->
  ?truncate:int ->
  ?framing:Logs_syslog.framing ->
  ?facility:Syslog_message.facility -> unit ->
  (Logs.reporter, string) result Lwt.t

(** {2:lwt_tls_example Example usage}

    To install a Lwt syslog reporter, sending via TLS to localhost, use the
    following snippet (assuming you already have certificates, and the common
    name of the collector is "log server"):
{[
let install_logger () =
  tls_reporter (Unix.inet_addr_of_string "127.0.0.1")
    ~cacert:"ca.pem" ~cert:"log.pem" ~priv_key:"log.key"
    () >|= function
  | Ok r -> Logs.set_reporter r
  | Error e -> print_endline e

let _ = Lwt_main.run (install_logger ())
]}

*)
