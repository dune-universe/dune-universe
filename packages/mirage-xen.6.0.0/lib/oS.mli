(*
 * Copyright (c) 2010 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (C) 2012-2014 Citrix Inc
 * 
 * Permission to use, copy, modify, and distribute this software for any
 * purpose with or without fee is hereby granted, provided that the above
 * copyright notice and this permission notice appear in all copies.
 *
 * THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
 * WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
 * ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
 * WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
 * ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
 * OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
 *)

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
val run : unit Lwt.t -> unit
end

module Memory : sig

(** Memory management operations. *)

(** Memory allocation statistics. Units are the system word size, as used by
 * the OCaml stdlib Gc module. *)
type stat = {
  heap_words : int;  (** total number of words allocatable on the heap. *)
  live_words : int;  (** number of live (i.e. allocated) words on the heap. *)
  stack_words : int; (** number of words in use by the program stack.
                      * This includes any space reserved by a stack guard. *)
  free_words : int;  (** number of free (i.e. allocatable) words on the heap. *)
}

val quick_stat: unit -> stat
(** [quick_stat ()]  returns memory allocation statistics. This call is
 * computationally cheap, but the returned values may not be completely
 * accurate. *)

end

module Time : sig

val sleep_ns : int64 -> unit Lwt.t
(** [sleep_ns d] Block the current thread for [n] nanoseconds. *)
end

module Xs : sig
  (** Xenstore client. *)

  include Xs_client_lwt.S
end

module Eventchn : sig
  (** Low-level event channels interface. *)

  type handle
  (** An initialised event channel interface. *)

  type t
  (** A local event channel. *)

  val to_int: t -> int
  (** [to_int evtchn] is the port number of [evtchn]. *)

  val of_int: int -> t
  (** [of_int n] is the [n]th event channel. *)

  val init: unit -> handle
  (** Return an initialised event channel interface. On error it
      will throw a Failure exception. *)

  val close: handle -> int
  (** Close an event channel interface and return the status code. *)

  val notify : handle -> t -> unit
  (** Notify the given event channel. On error it will throw a
      Failure exception. *)

  val bind_interdomain : handle -> int -> int -> t
  (** [bind_interdomain h domid remote_port] returns a local event
      channel connected to domid:remote_port. On error it will
      throw a Failure exception. *)

  val bind_unbound_port : handle -> int -> t
  (** [bind_unbound_port h remote_domid] returns a new event channel
      awaiting an interdomain connection from [remote_domid]. On error
      it will throw a Failure exception. *)

  val bind_dom_exc_virq : handle -> t
  (** Binds a local event channel to the VIRQ_DOM_EXC
      (domain exception VIRQ). On error it will throw a Failure
      exception. *)

  val unbind : handle -> t -> unit
  (** Unbinds the given event channel. On error it will throw a
      Failure exception. *)

  val unmask : handle -> t -> unit
  (** Unmasks the given event channel. On error it will throw a
      Failure exception. *)

  val is_valid : t -> bool
  (** [is_valid c] is true if [t] is bound. Bindings are invalidated
      after a domain resume. *)
end

module Activations : sig
  (** Event channels handlers. *)

  type event
  (** identifies the an event notification received from xen *)

  val program_start: event
  (** represents an event which 'fired' when the program started *)

  val after: Eventchn.t -> event -> event Lwt.t
  (** [next channel event] blocks until the system receives an event
      newer than [event] on channel [channel]. If an event is received
      while we aren't looking then this will be remembered and the
      next call to [after] will immediately unblock. If the system
      is suspended and then resumed, all event channel bindings are invalidated
      and this function will fail with Generation.Invalid *)

  (** {2 Low level interface} *)

  val wait : Eventchn.t -> unit Lwt.t
  (** [wait evtchn] is a cancellable thread that will wake up when
      [evtchn] is notified. Cancel it if you are no longer interested in
      waiting on [evtchn]. Note that if the notification is sent before
      [wait] is called then the notification is lost. *)

  val run : Eventchn.handle -> unit
  (** [run ()] goes through the event mask and activate any events,
      potentially spawning new threads. This function is called by
      [Main.run]. Do not call it unless you know what you are doing. *)

  val resume : unit -> unit
  (** [resume] needs to be called after the unikernel is
      resumed. However, this function is automatically called by
      {!Sched.suspend}. Do NOT use it unless you know what you are
      doing. *)

  val dump : unit -> unit
  (** [dump ()] prints internal state to the console for debugging *)
end

module Device_state : sig
  type state =
      Unknown
    | Initialising
    | InitWait
    | Initialised
    | Connected
    | Closing
    | Closed
    | Reconfiguring
    | Reconfigured
  val of_string : string -> state
  val to_string : state -> string
  val prettyprint : state -> string
end

module Xen : sig
  (** Some more Xen-specific that is neither part of scheduling
      nor startup info, and not tied to xenstore either. *)

  (** {2 Grants} *)

  (** Allow a local xen domain to read/write memory exported ("granted")
      from foreign domains. Safe memory sharing is a building block of all
      xen inter-domain communication protocols such as those for virtual
      network and disk devices.

      Foreign domains will explicitly "grant" us access to certain memory
      regions such as disk buffers. These regions are uniquely identified
      by the pair of (foreign domain id, integer reference) which is
      passed to us over some existing channel (typically via xenstore keys
      or via structures in previously-shared memory region). *)

  module Gntref : sig
    type t
    (** Type of a grant table index (an integer), called a grant reference in
        Xen's terminology. *)

    val of_string : string -> (t, [> `Msg of string]) result
    (** [of_string x] parses [x] as a 32-bit unsigned decimal. *)

    val to_string : t -> string
    (** [to_string x] is [x] formatted as a 32-bit unsigned decimal. *)

    val pp : t Fmt.t

    val to_int32 : t -> Int32.t
    val of_int32 : Int32.t -> t
  end

  (** {2 Receiving foreign pages} *)

  module Import : sig
    type t = {
      domid: int;
      (** foreign domain who is exporting memory *)
      ref: Gntref.t;
      (** id which identifies the specific export in the foreign domain *)
    }
    (** A foreign domain must explicitly "grant" us memory and send us the
        "reference". The pair of (foreign domain id, reference) uniquely
        identifies the block of memory. This pair ("grant") is transmitted
        to us out-of-band, usually either via xenstore during device setup or
        via a shared memory ring structure. *)

    module Local_mapping : sig
      type t
      (** Abstract type representing a locally-mapped shared memory page *)

      val to_buf: t -> Io_page.t

      val unmap_exn: t -> unit
      (** Unmap a single mapping (which may involve multiple grants). Throws a
          Failure if unsuccessful. *)

      val unmap: t -> (unit, [> `Msg of string]) result
      (** As above, but returning a [result] type. *)
    end

    val map_exn : t -> writable:bool -> Local_mapping.t
    (** [map_exn grant ~writable] creates a single mapping from
        [grant] that will be writable if [writable] is [true]. *)

    val map : t -> writable:bool -> (Local_mapping.t, [> `Msg of string]) result
    (** Like the above but wraps the result in a [result] type instead of
        raising an exception. *)

    val mapv_exn : t list -> writable:bool -> Local_mapping.t
    (** [mapv_exn grants ~writable] creates a single contiguous
        mapping from a list of grants that will be writable if
        [writable] is [true]. Note the grant list can involve grants
        from multiple domains. If the mapping fails (because at least
        one grant fails to be mapped), then all grants are unmapped. *)

    val mapv: t list -> writable:bool -> (Local_mapping.t, [> `Msg of string]) result
    (** Like the above but wraps the result in a [result] type instead of
        raising an exception. *)

    val with_mapping : t -> writable:bool ->
      (Local_mapping.t -> 'a Lwt.t) -> ('a, [> `Msg of string]) result Lwt.t
    (** [with_mapping grant ~writable f] maps [grant], calls [f] on
        the result, and then unmaps it. Returns [Error _] if the [map]
        operation fails. *)
  end

  (** {2 Offering pages to foreign domains} *)

  module Export : sig
    type t
    (** When sharing a number of pages with another domain, we receive back both the
        list of grant references shared and actually mapped page(s). The foreign
        domain can map the same shared memory, after being notified (e.g. via xenstore)
        of our domid and list of references. *)

    val refs : t -> Gntref.t list
    (** The grant references that have been shared with the foreign domain. *)

    val mapping : t -> Io_page.t
    (** Mapping of the shared memory. *)

    val share_pages : domid:int -> count:int -> writable:bool -> (t, [> `Grant_table_full]) result
    (** [share_pages ~domid ~count ~writable] shares [count] pages with foreign
        domain [domid]. [writable] determines whether or not the foreign domain
        can write to the shared memory. You must not allow the returned share
        to be GC'd before successfully calling [unshare] on it (if you do, the
        memory and grant ref will be leaked). *)

    val share_pages_exn : domid:int -> count:int -> writable:bool -> t
    (* Like [share_pages], but raise an exception on error. *)

    val try_unshare : release_refs:bool -> t -> (unit, [> `Busy]) result
    (** [try_unshare ~release_refs t] unshares a single mapping (which may
        involve multiple grants). If [release_refs = true] then it also calls
        [put] on each grant. If it encounters a grant that is still in use then
        it stops and returns [Error `Busy], with [t] containing the remaining grants.
        In this case, you must call [try_unshare] again later to retry. *)

    val unshare : release_refs:bool -> t -> unit Lwt.t
    (** Like [try_unshare], but keeps trying until all pages have been unshared. *)

    (** {2 Low-level interface} *)

    val get : unit -> Gntref.t Lwt.t
    (** Allocate a single grant table index *)

    val get_n : int -> Gntref.t list Lwt.t
    (** Allocate a block of n grant table indices *)

    val put : Gntref.t -> unit
    (** Deallocate a grant table index. *)

    val get_nonblock : unit-> Gntref.t option
    (** [get_nonblock ()] is [Some idx] if the grant table is not full,
        or [None] otherwise. *)

    val get_n_nonblock : int -> Gntref.t list
    (** [get_n_nonblock count] is a list of grant table indices of
        length [count], or [[]] if there if the table is too full to
        accomodate [count] new grant references. *)

    val num_free_grants : unit -> int
    (** [num_free_grants ()] returns the number of instantaneously free grant
        table indices *)

    val with_ref : (Gntref.t -> 'a Lwt.t) -> 'a Lwt.t
    (** [with_ref f] allocates a grant with [get], passes it to [f], and then [put]s it. *)

    val with_refs : int -> (Gntref.t list -> 'a Lwt.t) -> 'a Lwt.t

    val grant_access : domid:int -> writable:bool -> Gntref.t -> Io_page.t -> unit
    (** [grant_access ~domid ~writable gntref page] adds a grant table
        entry at index [gntref] to the grant table, granting access to
        [domid] to read [page], and write to is as well if [writable] is
        [true]. You must call [end_access] later to unshare the page. *)

    val try_end_access : release_ref:bool -> Gntref.t -> (unit, [> `Busy]) result
    (** [try_end_access gntref] removes entry index [gntref] from the grant
        table. If [release_ref = true] then it also calls [put gntref] on
        success. If the remote domain is still using the grant then it returns
        [Error `Busy] instead. *)

    val end_access : release_ref:bool -> Gntref.t -> unit Lwt.t
    (** Like [try_end_access], but if the remote domain is still using the
        grant then it sleeps for 10 seconds and tries again, until it succeeds.*)

    val with_grant : domid:int -> writable:bool -> Gntref.t ->
      Io_page.t -> (unit -> 'a Lwt.t) -> 'a Lwt.t
    (** [with_grant ~domid ~writable gntref page f] shares access to [page] with [domid],
        calls [f ()] and then unshares the page. *)

    val with_grants : domid:int -> writable:bool -> Gntref.t list ->
      Io_page.t list -> (unit -> 'a Lwt.t) -> 'a Lwt.t
  end

  val console : Gntref.t
  (** In xen-4.2 and later, the domain builder will allocate one of the
      reserved grant table entries and use it to pre-authorise the console
      backend domain. *)

  val xenstore : Gntref.t
  (** In xen-4.2 and later, the domain builder will allocate one of the
      reserved grant table entries and use it to pre-authorise the xenstore
      backend domain. *)
end
