(** This module defines the [Run_in_fork] module for [Async_extended.Std]. It is
    useful when performing long-running calculations that need data from the
    parent, and which would otherwise block async.

    See lib/async_extended/example/run_in_fork.ml for an example.
*)
open Core
open Async

(** [run_in_fork] forks and then runs [f] in the child process, and then writes the result
    back to the parent process over a unix pipe. Note that [f] cannot contain any async
    calculations. An Error will be returned if an exception is thrown in the child, or the
    child disconnects before writing the result. *)
val run_in_fork
  :  ?max_len:int
  -> bin_t:'b Bin_prot.Type_class.t
  -> f:(unit -> 'b)
  -> unit
  -> 'b Or_error.t Deferred.t

type 'a ret =
  | Part of 'a
  | Err of Error.t
  | Done

(** [run_in_fork_multiple] is just like [run_in_fork] but allows the user to write
    multiple results from the child to the parent. This is useful when the the data being
    written over the pipe is very large, as deserializing the data may take a long time,
    and the user may want to split the data into smaller chunks. *)
val run_in_fork_multiple
  :  ?max_len:int
  -> bin_t:'b Bin_prot.Type_class.t
  -> f:(write:('b -> unit) -> unit)
  -> unit
  -> 'b ret Pipe.Reader.t
