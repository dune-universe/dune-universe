(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Ipv6_multicast

module Socket : sig
  type 'a domain = 'a Ipv6_multicast.Socket.domain
  type 'a typ = 'a Ipv6_multicast.Socket.typ

  type ('domain, 'typ) t = private {
    sock : ('domain, 'typ) Ipv6_multicast.Socket.t ;
    fd : Unix.file_descr ;
    lwt : Lwt_unix.file_descr ;
  }

  val create :
    ?proto:int -> 'a Socket.domain -> 'b Socket.typ ->
    ('a Socket.domain, 'b Socket.typ) t
end

module Sockopt : sig
  val set :
    ('a Socket.domain, _ Socket.typ) Socket.t ->
    ('a Socket.domain, 'b) Ipv6_multicast.Sockopt.t -> 'b ->
    (unit, string) result
end

val bind :
  ('a Socket.domain, _ Socket.typ) Socket.t ->
  'a Socket.domain Sockaddr.t -> (unit, string) result

val connect :
  ('a Socket.domain, _ Socket.typ) Socket.t ->
  'a Socket.domain Sockaddr.t -> (unit, string) result Lwt.t

val send :
  ?saddr:_ Sockaddr.t ->
  ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t -> Cstruct.t ->
  (int, string) result Lwt.t

val send_bytes :
  ?saddr:_ Sockaddr.t ->
  ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t ->
  Bytes.t -> int -> int -> (int, string) result Lwt.t

val recv : ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t ->
  Cstruct.t -> (int, string) result Lwt.t

val recv_bytes : ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t ->
  Bytes.t -> int -> int -> (int, string) result Lwt.t

val recvfrom :
  ?flags:sendrecvflag list ->
  ('a Socket.domain, _ Socket.typ) Socket.t -> Cstruct.t ->
  (int * 'a Socket.domain Sockaddr.t, string) result Lwt.t

val recvfrom_bytes :
  ?flags:sendrecvflag list ->
  ('a Socket.domain, _ Socket.typ) Socket.t -> Bytes.t -> int -> int ->
  (int * 'a Socket.domain Sockaddr.t, string) result Lwt.t

(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff

   Permission to use, copy, modify, and/or distribute this software for any
   purpose with or without fee is hereby granted, provided that the above
   copyright notice and this permission notice appear in all copies.

   THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
   WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
   MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
   ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
   WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
   ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
   OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
  ---------------------------------------------------------------------------*)
