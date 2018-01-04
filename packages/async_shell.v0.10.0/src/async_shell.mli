open! Core
open Core_extended.Std
open! Async
(** The functions in here are straightforward in_thread wrappers of
    core_extended.Shell functions. *)

module Process : sig
  type t = Core_extended.Shell.Process.t

  type status = [ `Timeout of Time.Span.t
                | `Exited of int
                | `Signaled of Signal.t ]

  type result = Core_extended.Shell.Process.result = {
    command : t;
    status  : status;
    stdout  : string;
    stderr  : string
  }

  exception Failed of result

  val status_to_string : status -> string
  val format_failed : result -> string
end

type 'a with_process_flags = 'a Core_extended.Shell.with_process_flags
type 'a with_run_flags     = 'a Core_extended.Shell.with_run_flags
type 'a with_test_flags    = 'a Core_extended.Shell.with_test_flags
type 'a with_ssh_flags     = 'a Core_extended.Shell.with_ssh_flags
type 'a cmd                = 'a Core_extended.Shell.cmd
type ('a,'ret) sh_cmd      = ('a,'ret) Core_extended.Shell.sh_cmd

val test     :      bool Deferred.t     cmd with_test_flags
val sh_test  : ('a, bool Deferred.t) sh_cmd with_test_flags
val ssh_test : ('a, bool Deferred.t) sh_cmd with_test_flags with_ssh_flags

val run         : unit Deferred.t cmd with_run_flags
val run_lines   : ?eol:char -> string list   Deferred.t cmd with_run_flags
val run_one     : ?eol:char -> string option Deferred.t cmd with_run_flags
[@@deprecated "[since 2017-11] Use [run_one_line] to get a different behavior or \
               [run_first_line] to get the old behavior"]
val run_one_exn : ?eol:char -> string        Deferred.t cmd with_run_flags
[@@deprecated "[since 2017-11] Use [run_one_line_exn] to get a different behavior or \
               [run_first_line_exn] to get the old behavior"]
val run_one_line     : ?eol:char -> string Or_error.t Deferred.t cmd with_run_flags
val run_one_line_exn : ?eol:char -> string        Deferred.t cmd with_run_flags
val run_first_line     : ?eol:char -> string option Deferred.t cmd with_run_flags
val run_first_line_exn : ?eol:char -> string        Deferred.t cmd with_run_flags
val run_full    :      string Deferred.t                cmd with_run_flags
val run_full_and_error :   (string * string) Deferred.t cmd with_run_flags
val run_lines_stream : string Stream.t                  cmd with_run_flags

val sh         : ('a, unit          Deferred.t) sh_cmd with_run_flags
val sh_lines   : ('a, string list   Deferred.t) sh_cmd with_run_flags
val sh_one     : ('a, string option Deferred.t) sh_cmd with_run_flags
[@@deprecated "[since 2017-11] Use [sh_one_line] to get a different behavior or \
               [sh_first_line] to get the old behavior"]
val sh_one_exn : ('a, string        Deferred.t) sh_cmd with_run_flags
[@@deprecated "[since 2017-11] Use [sh_one_line_exn] to get a different behavior or \
               [sh_first_line_exn] to get the old behavior"]
val sh_one_line       : ('a,string Or_error.t Deferred.t) sh_cmd with_run_flags
val sh_one_line_exn   : ('a,string Deferred.t) sh_cmd with_run_flags
val sh_first_line       : ('a,string option Deferred.t) sh_cmd with_run_flags
val sh_first_line_exn   : ('a,string Deferred.t) sh_cmd with_run_flags
val sh_full    : ('a, string        Deferred.t) sh_cmd with_run_flags
val sh_full_and_error : ('a, (string * string) Deferred.t) sh_cmd with_run_flags
val sh_lines_stream : ('a, string Stream.t)     sh_cmd with_run_flags

val ssh         : ('a, unit          Deferred.t) sh_cmd with_run_flags with_ssh_flags
val ssh_lines   : ('a, string list   Deferred.t) sh_cmd with_run_flags with_ssh_flags
val ssh_full    : ('a, string        Deferred.t) sh_cmd with_run_flags with_ssh_flags
val ssh_one     : ('a, string option Deferred.t) sh_cmd with_run_flags with_ssh_flags
[@@deprecated "[since 2017-11] Use [ssh_one_line] to get a different behavior or \
               [ssh_first_line] to get the old behavior"]
val ssh_one_exn : ('a, string        Deferred.t) sh_cmd with_run_flags with_ssh_flags
[@@deprecated "[since 2017-11] Use [ssh_one_line_exn] to get a different behavior or \
               [ssh_first_line_exn] to get the old behavior"]
val ssh_one_line     : ('a, string Or_error.t Deferred.t) sh_cmd with_run_flags with_ssh_flags
val ssh_one_line_exn : ('a, string        Deferred.t) sh_cmd with_run_flags with_ssh_flags
val ssh_first_line     : ('a, string option Deferred.t) sh_cmd with_run_flags with_ssh_flags
val ssh_first_line_exn : ('a, string        Deferred.t) sh_cmd with_run_flags with_ssh_flags
val ssh_lines_stream : ('a, string Stream.t)     sh_cmd with_run_flags with_ssh_flags


(** {6 Small helper commands} *)

val mkdir : ?p:unit -> ?perm:int -> string -> unit Deferred.t

val scp : ?compress:bool -> ?recurse:bool -> ?user:string -> host:string
  -> string -> string -> unit Deferred.t
