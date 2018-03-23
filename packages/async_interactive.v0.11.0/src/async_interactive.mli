open! Core
open! Async

val interactive : bool ref

(** These [print*] functions only print when [!interactive]. *)
val print_string : string -> unit Deferred.t

val print_endline : string -> unit Deferred.t

val printf          : ('r, unit, string, unit Deferred.t) format4 -> 'r

val prints : string -> 'a -> ('a -> Sexp.t) -> unit Deferred.t

val print_s : Sexp.t -> unit Deferred.t

val ask_dispatch_gen
  :  f:(string -> ('a, string) Result.t)
  -> string
  -> 'a Deferred.t

module Choice : sig
  type +'a t

  val create : char -> 'a -> string -> 'a t

  val default : 'a t -> 'a t
end

(** [ask_dispatch_gen question choices] displays [question] and gets user input to select
    one of the [choices].  At most once choice can be the [default] choice. *)
val ask_dispatch_with_help
  :  ?show_options:bool  (** default is [true] *)
  -> string
  -> 'a Choice.t list
  -> 'a Deferred.t

val ask_yn
  :  ?default:bool
  -> string
  -> bool Deferred.t

val ask_ynf
  :  ?default:bool
  -> ('a, unit, string, bool Deferred.t) format4
  -> 'a

(** These [show*] functions print even when [not !interactive]. *)
val show_file
  :  ?pager:string
  -> ?msg:string
  -> file:string
  -> unit
  -> unit Deferred.t

val show_string_with_pager
  :  ?pager:string (** default: $PAGER if set, or else less *)
  -> string
  -> unit Deferred.t

module Job : sig
  (** This module allows you to have messages printed when you start and finish jobs
      without having a bad interaction in case of interleaved jobs run in parallel.

      Example of code:

      {[
        Interactive.Job.run !"starting doing stuff A in process %{Pid}" pid
          ~f:(fun () -> do_stuff_A ())
        >>= fun () ->
      ]}

      Example of output:

      1 process:

      {[
        starting doing stuff A in process 1234 ... done.
      ]}

      Multiple processes:

      {[
        starting doing stuff A in process 1234 ...
        starting doing stuff A in process 4321 ...
        all done.
        starting doing stuff A in process 5678 ... done.
      ]}
  *)
  val run : f:(unit -> 'a Deferred.t) -> ('r, unit, string, 'a Deferred.t) format4 -> 'r
end
