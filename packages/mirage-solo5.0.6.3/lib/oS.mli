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

module MM : sig
  val malloc_metrics : tags:'a Metrics.Tags.t -> ('a, unit -> Metrics.Data.t) Metrics.src
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
