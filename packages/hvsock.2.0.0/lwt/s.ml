(*
 * Copyright (C) 2016 Docker Inc
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
 *
 *)

module type FN = sig
  (** Call a blocking ('a -> 'b) function in a ('a -> 'b Lwt.t) context *)

  type ('request, 'response) t
  (** A function from 'request to 'response *)

  val create: ('request -> 'response) -> ('request, 'response) t
  val destroy: ('request, 'response) t -> unit

  val fn: ('request, 'response) t -> 'request -> 'response Lwt.t
  (** Apply the function *)

end

module type SOCKET = sig
  type t
  (** A socket which supports I/O via Lwt *)

  type sockaddr
  (** A socket address *)

  val string_of_sockaddr: sockaddr -> string

  val create: unit -> t
  (** [create ()] creates an unbound hypervisorsocket *)

  type fd
  (** A low-level file descriptor *)

  val to_fd: t -> fd option
  (** [to_fd t] returns the wrapped file descriptor. Note this only supports
      blocking I/O *)

  val bind: t -> sockaddr -> unit
  (** [bind t sockaddr] binds [socket] to [sockaddr] *)

  val listen: t -> int -> unit
  (** [listen t queue] *)

  val accept: t -> (t * sockaddr) Lwt.t
  (** [accept t] accepts a single connection *)

  val connect: ?timeout_ms:int -> t -> sockaddr -> unit Lwt.t
  (** [connect ?timeout_ms t sockaddr] connects to a remote partition *)

  val read: t -> Cstruct.t -> int Lwt.t
  (** [read t buf] reads as many bytes as available into [buf] returning
      the number of bytes read. *)

  val write: t -> Cstruct.t -> int Lwt.t
  (** [write t buf] writes as many bytes from [buf] to [t] as will currently
      fit inside [t]'s internal buffer, and return the number of bytes
      written *)

  val close: t -> unit Lwt.t
  (** [close t] closes a socket *)

  val shutdown_read: t -> unit Lwt.t
  (** [shutdown_read t] closes the read side of the socket *)

  val shutdown_write: t -> unit Lwt.t
  (** [shutdown_write t] closes the write side of the socket *)
end
