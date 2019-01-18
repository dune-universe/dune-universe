(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
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

(** Lwt interface to shared memory ring. *)

exception Shutdown

open Ring

(** The (client) front-end connection to the shared ring. *)
module Front : sig

  type ('a,'b) t
  (** Type of a frontend connection to a shared ring. ['a] is the
      response type, and ['b] is the request id type (e.g. int or
      int64). *)

  val init : ('b -> string) -> ('a, 'b) Ring.Rpc.Front.t -> ('a,'b) t
  (** [init string_of_id ring] initialises a stateful lwt client
      attached to [ring]. *)

  val write : ('a, 'b) t -> (buf -> 'b) -> 'a Lwt.t Lwt.t
  (** [write ring req_fn] blocks until a ring slot is free, calls
      [req_fn] on it and returns a thread that will wakeup when the
      response will be available. *)

  val push : ('a, 'b) t -> (unit -> unit) -> unit
  (** [push ring notify_fn] advances [ring] pointers, exposing the
      written requests to the other end. If the other end won't see
      the update, [notify_fn] is called to signal it. *)

  val push_request_and_wait : ('a,'b) t -> (unit -> unit) -> (buf -> 'b) -> 'a Lwt.t
  (** [push_request_and_wait frontend notify_fn req_fn] is
      [write req_fn >>= fun t -> push notify_fn; return t]. *)

  val push_request_async : ('a,'b) t -> (unit -> unit) -> (buf -> 'b) ->
    ('a Lwt.t -> unit Lwt.t) -> unit Lwt.t
  (** [push_request_async ring notify_fn req_fn free_fn] is like
      [push_request_and_wait] except it calls [free_fn] on the result
      of [write] instead of returning it. *)

  val poll : ('a,'b) t -> (buf -> ('b * 'a)) -> unit
  (** [poll frontend resp_fn] polls the ring for responses, and wakes
      up any threads that are sleeping (as a result of calling
      [push_request_and_wait]). This can be called regularly, or
      triggered via some external event such as an event channel
      signal. *)

  val shutdown : ('a, 'b) t -> unit

  val wait_for_free : ('a, 'b) t -> int -> unit Lwt.t
  (** [wait_for_free frontend n] waits until at least [n] slots are free.
   * It doesn't reserve them, so you'll probably need to use your own mutex
   * around this call. Requests are handled in order, so even large requests
   * will eventually be served. *)

  val to_string : ('a,'b) t -> string
  (** [to_string t] returns debug-printable description of the ring
      metadata *)
end

(** The (server) back-end connection to the shared ring. *)
module Back : sig

  type ('a,'b) t
  (** Type of a backend connection to a shared ring. 'a is the
      response type, and 'b is the request id type (e.g. int or
      int64). *)

  val init : ('b -> string) -> ('a, 'b) Ring.Rpc.Back.t -> ('a,'b) t
  (** [init string_of_id ring] initialises a stateful lwt server
      attached to [ring]. *)

  val push_response : ('a, 'b) t -> (unit -> unit) -> (buf -> unit) -> unit
  (** [push_response t notify_fn fn] finds a free slot and applies it
      to [fn], signalling the client via [notify_fn] that a response
      is ready. *)
end
