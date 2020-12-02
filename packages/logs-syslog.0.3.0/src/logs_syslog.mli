(** Logs reporter using syslog

    The {{:http://erratique.ch/software/logs/doc/Logs.html}logs} library
    provides basic logging support, each log source has an independent logging
    level, and reporting is decoupled from logging.

    This library implements log reporters via syslog, using
    {{:http://verbosemo.de/syslog-message/}syslog-message}.

    A variety of transport mechanisms are implemented:
    {ul
    {- {{:https://tools.ietf.org/html/rfc3164}RFC 3164} specifies the original
       BSD syslog protocol over UDP on port 514 (also
       {{:https://tools.ietf.org/html/rfc5426}RFC 5426}).}
    {- {{:https://tools.ietf.org/html/rfc6587}RFC 6587} specifies the historic
       syslog over TCP on port 514.}
    {- {{:https://tools.ietf.org/html/rfc5425}RFC 5425} specifies syslog over
       TLS on TCP port 6514.}}

    The UDP transport sends each log message to the remote log host using
    [sendto].  If [sendto] raises an [Unix.Unix_error], this error is printed
    together with the log message on standard error.

    When using a stream transport, TCP or TLS, the creation of a reporter
    attempts to establish a connection to the log host, and only results in [Ok]
    {!Logs.reporter} on success, otherwise the [Error msg] is returned.  At
    runtime when the connection failed, the message is printed on standard
    error, and a re-establishment of the connection is attempted.

    Syslog messages need to be framed when transported over TCP and TLS streams.
    In the historical {{:https://tools.ietf.org/html/rfc6587}RFC 6587} this is
    defined to be either non-transparent (which lets you stream the message), by
    terminating each syslog message with a line feed (0x0A), a null byte (0x00),
    a CR-LF sequence, or any custom byte sequence.  The alternative is octet
    counting, also defined in {{:https://tools.ietf.org/html/rfc5425}RFC 5425},
    which prepends the message with the message length, ASCII-encoded as decimal
    number, followed by a whitespace (0x20).  We support all three popular
    methods, and also a [`Custom] one appending any byte sequence at the end of
    each message.  There are defaults (TCP: null byte, TLS: octet counting)
    passed to the individual reporter constructors (see the {!framing} type
    below).

    Every time a library logs a message which reaches the reporter (depending on
    log level), the function {!message} is evaluated with the [hostname]
    provided while creating the reporter, the log level is mapped to a syslog
    level, and the current timestamp is added.  The log message is prepended
    with the log source name.

    This module contains the pure fragments shared between the effectful
    implementation for {{!Logs_syslog_unix}Unix}, {{!Logs_syslog_lwt}Lwt}, and
    {{!Logs_syslog_mirage}MirageOS}.  TLS support is available for
    {{!Logs_syslog_lwt_tls}Lwt} and {{!Logs_syslog_mirage_tls}MirageOS}.

    Not implemented is the reliable transport for syslog (see
    {{:https://tools.ietf.org/html/rfc3195}RFC 3195}) (using port 601), which is
    an alternative transport of syslog messages over TCP.

    {e v0.3.0 - {{:https://github.com/hannesm/logs-syslog }homepage}} *)

(** [message ~facility ~host ~source ~tags ~header level now msg] is [message],
    a syslog message with the given values.  The default [facility] is
    [Syslog_message.System_Daemons].  *)
val message :
  ?facility:Syslog_message.facility ->
  host:string ->
  source:string ->
  tags:Logs.Tag.set ->
  ?header:string ->
  Logs.level ->
  Ptime.t ->
  string ->
  Syslog_message.t

(** Different framing methods used in the wild, as described in
    {{:https://tools.ietf.org/html/rfc6587}RFC 6587} *)
type framing = [
  | `LineFeed
  | `Null
  | `Custom of string
  | `Count
]

(** [frame_msg msg framing] is [framed_message], where the [framing] is
    applied. *)
val frame_message : string -> framing -> string

(** [ppf] is a formatter *)
val ppf : Format.formatter

(** [flush ()] flushes the formatter, and return the [text] *)
val flush : unit -> string

(** [facility] is a {!Logs.Tag.def} tag to give a message a different syslog
    facility from the reporter's default. *)
val facility : Syslog_message.facility Logs.Tag.def
