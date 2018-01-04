(** This module is deprecated; please use [Async.Process]. *)

open! Core
open! Async

module Output : sig
  type 'a t = {
    stdout: 'a;
    stderr: 'a
  }
end

(** [create_process cmd f] runs cmd and hands details of the process to f.  When f is
    determined, or when f throws an exception, the background process is killed gracefully
    (TERM, wait, then KILL), all file descriptors are closed, and the process is reaped *)
val create
  :  ?kill:unit Deferred.t
  -> ?kill_how:[`by_pid | `by_group]
  -> prog:string
  -> args:string list
  -> ?env:Async.Process.env
  -> ?working_dir:string
  -> ?stdin:string
  -> f:(Pid.t -> stdin:Writer.t -> stdout:Reader.t -> stderr:Reader.t -> 'a Deferred.t)
  -> unit
  -> ('a * Core.Unix.Exit_or_signal.t) Deferred.t

type file_descr = Core.Unix.File_descr.t

(** [create_fds...] like create, except that the three file_descrs to be used for the
    stdin/stdout/stderr of the forked process are passed in. The passed-in file_descrs
    might for example have been obtained by a called to Unix.pipe(). *)
val create_fds
  :   kill:unit Deferred.t
  -> ?kill_how:[`by_pid | `by_group]
  -> prog:string
  -> args:string list
  -> ?env:[ Async.Process.env | `Replace_raw of string list ]
  -> stdin:file_descr
  -> stdout:file_descr
  -> stderr:file_descr
  -> f:(Pid.t -> unit)
  -> unit
  -> Unix.Exit_or_signal.t Deferred.t

(** [create_fds'...] like [create_fds] except that we wait for caller's [f] to become
    determined before calling [Unix.waitpid].  Also, result of caller's [f] is return with
    [Exit_or_signal.t] *)
val create_fds'
  :  kill:unit Deferred.t
  -> ?kill_how:[`by_pid | `by_group]
  -> prog:string
  -> args:string list
  -> ?env:[ Async.Process.env | `Replace_raw of string list ]
  -> stdin:file_descr
  -> stdout:file_descr
  -> stderr:file_descr
  -> f:(Pid.t -> 'a Deferred.t)
  -> unit
  -> ('a * Unix.Exit_or_signal.t) Deferred.t


(** [open_in ~prog ~args] runs prog with args and returns a readers connected to stdout
    and stderr.  When both of those readers are closed then the process is reaped (and
    killed if necessary).  is_ok defaults to Core.Unix.Exit_or_signal.ok. *)
val open_in
  :  ?is_ok:(Core.Unix.Exit_or_signal.t -> bool)
  -> ?kill:unit Deferred.t
  -> prog:string
  -> args:string list
  -> unit
  -> Reader.t Output.t Deferred.t

type 'a backtick =
  ?kill:unit Deferred.t
  -> ?env:Async.Process.env
  -> prog:string
  -> args:string list
  -> ?working_dir:string
  -> ?stdin:string
  -> unit
  -> 'a

(** [backtick_status ~prog ~args] runs prog with args and returns the full
    stdout and stderr as strings, together with the process status.  If
    [stdin] is provided, it will be written to the stdin of the program, but
    no guarantees are made as to whether the program actually reads this
    data.  In particular, the program may close stdin, or exit before the
    data has been read; no error is raised in these circumstances.  This
    behaviour is akin to:
    echo stdin | ...
    at the shell prompt. *)
val backtick_status : (string Output.t * Unix.Exit_or_signal.t) Deferred.t backtick


(** [backtick ~prog ~args] runs prog with args and returns the full stdout and stderr
    as strings (ignoring the process status) *)
val backtick : string Output.t Deferred.t backtick

val backtick_new : (string, exn) Result.t Deferred.t backtick

val backtick_new_exn : string Deferred.t backtick
