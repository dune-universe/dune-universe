(** MirageOS Unix implementation of the [OS] module *)

module Lifecycle : sig
  val await_shutdown_request :
    ?can_poweroff:bool ->
    ?can_reboot:bool ->
    unit -> [`Poweroff | `Reboot] Lwt.t
    (** [await_shutdown_request ()] is thread that resolves when the domain is
       asked to shut down.  The optional [poweroff] (default:[true]) and
       [reboot] (default:[false]) arguments can be used to indicate which
       features the caller wants to advertise (however, you can still get a
       request for a mode you didn't claim to support). *)
end

module Main : sig
  val run : unit Lwt.t -> unit
end

module Time : sig
  val sleep_ns : int64 -> unit Lwt.t
  (** [sleep_ns d] Block the current thread for [n] nanoseconds. *)
end
