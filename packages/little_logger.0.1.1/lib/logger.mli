(** A tiny, little logger <3

    {1 Overview}

    {2 Set up}

    The default log level is [Level.Warning] and the default printer is
    [prerr_endline].

    To change these, use the {!set_log_level} and {!set_printer} functions.

    {2 Messages}

    Logging functions come in two flavors: those that accept a [message] (e.g.,
    [info], [error]), and those that accept strings directly (e.g., [sinfo],
    [serror]).

    A [message] is simply a [unit -> string]
    {{:https://en.wikipedia.org/wiki/Thunk} thunk}. Because [message] is a
    thunk, its evaluation won't happen unless the message actually needs to be
    printed. The type [message] is just a name given to the thunk to clarify the
    function signatures.

    The functions that take a string directly are prefixed with an [s] (for
    string).

    {2 Printers}

    A [printer] is a function with the following signature [string -> unit]. The
    name [printer] is simply used for clarity.

    Common choices for a [printer] would be [prerr_endline] to print to
    [stderr], or [Async.prerr_endline] to asynchronously print to [stderr] using
    [Async.Writer] (e.g., if you are using Jane Street's [Async] library.

    {2 Message Levels}

    Message levels are hierarchical: given the log level threshold
    ({!get_log_level}), all messages of equal or higher priority will be logged.

    In other words, if a message level is greater than or equal to the logging
    threshold, it will be printed.

    For example, [Level.Trace] would print all messages, [Level.Silent] would
    print no messages, [Level.Error] would print [Level.Error], [Level.Fatal],
    and [Level.Unknown] messages.

    The default log level is [Level.Warning]. All messages of equal or higher
    priority than this will be logged.

    {1:examples Examples}

    Note: the examples assume you have [open Little_logger] at the top of your
    file.

    {2 Logging messages}

    {[
      let () = Logger.error (fun () -> "This is an error")
      let () = Logger.serror "This is an error, but using `serror` instead"

      let () =
        Logger.info (fun () ->
            sprintf "I can use %s strings like %s" "format" "this")
    ]}

    Using the thunk accepting functions let you avoid potentially expensive
    calls in cases where the log level would prevent a message from being
    printed.

    {[
      let () = Logger.set_log_level Logger.Level.Error

      (* This won't run and the expensive thing won't take up extra time. *) let
      () = Logger.debug (fun () -> (* ...Call some expensive log message
      generating function here... *) )
    ]}

    {2 Changing the logging level}

    The default level is [Warning]. Here we lower it to [Debug].

    {[ let () = Logger.set_log_level Logger.Level.Debug ]}

    {2 Changing the printer}

    The default printer is [prerr_endline], which prints to [stderr]. We can
    change to printing to [stdout] like this.

    {[ let () = Logger.set_printer print_endline ]}

    If you're using the [Async] library, just change to an async printer.

    {[ let () = Logger.set_printer Async.prerr_endline ]}

    {3 Writing directly to a file}

    If you want to get crazy you could do something like this to log directly to
    a file, but I don't know if it is the best idea :)

    {[
      let log_fname = "silly_file.txt"
      let log_chan = Out_channel.create "silly_file.txt"

      let printer msg = Out_channel.output_string log_chan (msg ^ "\n")
      let () = Logger.set_printer printer

      let () = Logger.info (fun () -> "Hi file!")

      let () = Out_channel.close log_chan
    ]} *)

(** {1 API} *)

open! Core

(** {2 Level} *)

module Level : sig
  (** Logging levels *)

  (** Logging levels ordered from most messages printed to fewest. Or from
      lowest severity/priority to highest severity/priority.

      If a message level is greater than or equal to the logging threshold, it
      will be printed.

      E.g.,

      - [Trace] messages are only printed at log level threshold of [Trace].
      - [Info] messages are printed at log level threshold of [Info] and below
        ([Trace], [Debug], and [Info]).
      - [Error] messages are printed at log level threshold of [Error] and
        below.

      If the log level is set to [Silent] no messages will be printed
      (regardless of their level).

      ([Trace
      < Debug < Info < Warning < Error < Fatal < Unknown < Silent]) *)
  type t = Trace | Debug | Info | Warning | Error | Fatal | Unknown | Silent

  val of_string : string -> t Or_error.t
  (** [of_string level] attempts to create a [t] from its case-insensitive
      string representation (e.g., for creating from command line arguments). *)

  val to_string : t -> string
  (** [to_string t] converts the [t] to its string representation (e.g., for
      printing). *)
end

(** {2 Type aliases} *)

type printer = string -> unit
(** Type alias for printer functions *)

type message = unit -> string
(** Type alias for message thunks *)

(** {2 Getters and setters} *)

val get_log_level : unit -> Level.t

val set_log_level : Level.t -> unit

val set_printer : printer -> unit

(** {2:message_accepting Message accepting log functions} *)

val unknown : message -> unit
(** [unknown msg] logs an unknown message. Unknown messages are printed when log
    level is [Level.Unknown] or below. *)

val fatal : message -> unit
(** [fatal msg] logs an fatal message. Fatal messages are printed when log level
    is [Level.Fatal] or below. *)

val error : message -> unit
(** [error msg] logs an error message. Error messages are printed when log level
    is [Level.Error] or below. *)

val warning : message -> unit
(** [warning msg] logs an warning message. Warning messages are printed when log
    level is [Level.Warning] or below. *)

val info : message -> unit
(** [info msg] logs an info message. Info messages are printed when log level is
    [Level.Info] or below. *)

val debug : message -> unit
(** [debug msg] logs an debug message. Debug messages are printed when log level
    is [Level.Debug] or below. *)

val trace : message -> unit
(** [trace msg] logs an trace message. Trace messages are printed when log level
    is [Level.Trace] or below. *)

(** {2 String accepting log functions}

    These are analogous to the {{!message_accepting} message accepting} log
    functions. *)

val sunknown : string -> unit
val sfatal : string -> unit
val serror : string -> unit
val swarning : string -> unit
val sinfo : string -> unit
val sdebug : string -> unit
val strace : string -> unit
