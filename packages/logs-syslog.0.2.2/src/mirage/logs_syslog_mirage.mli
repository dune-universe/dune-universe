(** Logs reporter via syslog using MirageOS

    Please read {!Logs_syslog} first. *)

(** UDP syslog *)
module Udp (C : Mirage_console.S) (CLOCK : Mirage_clock.PCLOCK) (STACK : Mirage_stack.V4) : sig
  (** [create c udp ~hostname ip ~port ~truncate ()] is [reporter], which
      sends log messages to [ip, port] via UDP.  Upon failure, a message is
      emitted to the console [c].  Each message can be truncated: [truncate]
      defaults to 65535 bytes.  The [hostname] is part of each syslog message.
      The [port] defaults to 514. [facility] is the default syslog facility (see
      {!Logs_syslog.message}). *)
  val create : C.t -> STACK.t -> hostname:string ->
    STACK.IPV4.ipaddr -> ?port:int -> ?truncate:int ->
    ?facility:Syslog_message.facility -> unit -> Logs.reporter
end

(** TCP syslog *)
module Tcp (C : Mirage_console.S) (CLOCK : Mirage_clock.PCLOCK) (STACK : Mirage_stack.V4) : sig
  (** [create c tcp ~hostname ip ~port ~truncate ~framing ()] is
      [Ok reporter] or [Error msg].  The [reporter] sends log messages to [ip, port]
      via TCP.  If the initial TCP connection to the [remote_ip] fails, an
      [Error msg] is returned instead.  If the TCP connection fails, an error is
      logged to the console [c] and attempts are made to re-establish the TCP
      connection.  Each syslog message can be truncated, depending on [truncate]
      (defaults to no truncating).  The [hostname] is part of each syslog
      message.  The default value of [port] is 514, the default behaviour of
      [framing] is to append a zero byte. [facility] is the default syslog
      facility (see {!Logs_syslog.message}). *)
  val create : C.t -> STACK.t -> hostname:string ->
    STACK.IPV4.ipaddr -> ?port:int ->
    ?truncate:int ->
    ?framing:Logs_syslog.framing ->
    ?facility:Syslog_message.facility -> unit ->
    (Logs.reporter, string) result Lwt.t
end

(** {2:mirage_example Example usage}

    To install a Mirage syslog reporter, sending via UDP to localhost, use the
    following snippet:
{[
module Main (C : Mirage_console.S) (S : Mirage_stack.V4) (CLOCK : Mirage_clock.PCLOCK)
  module LU = Logs_syslog_mirage.Udp(C)(CLOCK)(S)

  let start c s _ =
    let ip = Ipaddr.V4.of_string_exn "127.0.0.1" in
    let r = LU.create c s ip ~hostname:"MirageOS.example" () in
    Logs.set_reporter r ;
    Lwt.return_unit
end
]}

    The TCP transport is very similar:
{[
module Main (C : Mirage_console.S) (S : Mirage_stack.V4) (CLOCK : Mirage_clock.PCLOCK)
  module LT = Logs_syslog_mirage.Tcp(C)(CLOCK)(S)

  let start c s _ =
    let ip = Ipaddr.V4.of_string_exn "127.0.0.1" in
    LT.create c s ip ~hostname:"MirageOS.example" () >>= function
      | Ok r -> Logs.set_reporter r ; Lwt.return_unit
      | Error e -> Lwt.fail_invalid_arg e
end
]}

*)
