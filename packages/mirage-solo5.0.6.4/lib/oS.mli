module Lifecycle : sig

val await_shutdown_request :
  ?can_poweroff:bool ->
  ?can_reboot:bool ->
  unit -> [`Poweroff | `Reboot] Lwt.t
(** [await_shutdown_request ()] is thread that resolves when the domain is
    asked to shut down.
    The optional [poweroff] (default:[true]) and [reboot] (default:[false])
    arguments can be used to indicate which features the caller wants to
    advertise (however, you can still get a request for a mode you didn't claim
    to support). *)

end

module Main : sig
val wait_for_work_on_handle : int64 -> unit Lwt.t
val run : unit Lwt.t -> unit
end

module Memory : sig

  (** Memory management operations. *)

  (** Memory allocation statistics. Units are the system word size, as used by
      the OCaml stdlib Gc module. *)
  type stat = {
    heap_words : int;  (** total number of words allocatable on the heap. *)
    live_words : int;  (** number of live (i.e. allocated) words on the heap. *)
    stack_words : int; (** number of words in use by the program stack.
                           This includes any space reserved by a stack guard. *)
    free_words : int;  (** number of free (i.e. allocatable) words on the heap. *)
  }

  val quick_stat: unit -> stat
  (** [quick_stat ()]  returns memory allocation statistics. This call is
      computationally cheap, but the returned values may not be completely
      accurate. *)
end

module MM : sig
  val malloc_metrics : tags:'a Metrics.Tags.t -> ('a, unit -> Metrics.Data.t) Metrics.src
  [@@ocaml.deprecated "This function will be deprecated. Use {!Memory.quick_stat} instead."]
end

module Time : sig

val sleep_ns : int64 -> unit Lwt.t
(** [sleep_ns d] Block the current thread for [n] nanoseconds. *)
end

module Solo5 : sig

type solo5_result =
  | SOLO5_R_OK
  | SOLO5_R_AGAIN
  | SOLO5_R_EINVAL
  | SOLO5_R_EUNSPEC
(** A type mapping the C enum solo5_result_t to OCaml **)

end
