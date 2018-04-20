(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

module Iface : sig
  type t
  val of_string : string -> (t, string) result
  val of_string_opt : string -> t option
  val of_string_exn : string -> t
  val to_int : t -> int
  val to_int32 : t -> Int32.t
end

module Socket : sig
  type _ domain = private
    | Unix : [`Unix] domain
    | Inet : [`Inet] domain
    | Inet6 : [`Inet6] domain

  type unix = [`Unix] domain
  type inet = [`Inet] domain
  type inet6 = [`Inet6] domain

  val unix : unix
  val inet : inet
  val inet6 : inet6

  type 'a typ = ([< `Stream | `Dgram | `Raw | `Seqpacket] as 'a)
  val stream : [`Stream]
  val dgram : [`Dgram]
  val raw : [`Raw]
  val seqpacket : [`Seqpacket]

  type (_, _) t
  val create : ?proto:int -> 'a domain -> 'b typ -> ('a domain, 'b typ) t
  val to_fd : (_ domain, _ typ) t -> Unix.file_descr
end

module Sockaddr : sig
  type _ t = private
    | U : string -> Socket.unix t
    | V4 : { addr : Ipaddr.V4.t ;
             port : int } -> Socket.inet t
    | V6 : { addr : Ipaddr.V6.t ;
             port : int ;
             flowinfo : Int32.t ;
             scope_id : Int32.t
           } -> Socket.inet6 t

  val of_unix : string -> Socket.unix t
  val of_ipv4_port : Ipaddr.V4.t -> int -> Socket.inet t
  val of_ipv6_port :
    ?flowinfo:Int32.t -> ?scope_id:Int32.t ->
    Ipaddr.V6.t -> int -> Socket.inet6 t
  val to_sockaddr : _ t -> Unix.sockaddr
  val of_bytes : ?pos:int -> Cstruct.t -> 'a Socket.domain -> 'a Socket.domain t option
  val of_bytes_exn : ?pos:int -> Cstruct.t -> 'a Socket.domain -> 'a Socket.domain t
  val write : ?pos:int -> Cstruct.t -> _ t -> unit
  val to_bytes : _ t -> Cstruct.t
end

module Sockopt : sig
  type (_, _) t

  val v4_multicast_if : (Socket.inet, Iface.t) t
  val v4_multicast_ttl : (Socket.inet, int) t
  val v4_multicast_loop : (Socket.inet, bool) t
  val v4_add_membership : (Socket.inet, Socket.inet Sockaddr.t * int) t
  val v4_drop_membership : (Socket.inet, Socket.inet Sockaddr.t * int) t

  val v6_join_group : (Socket.inet6, Socket.inet6 Sockaddr.t) t
  val v6_leave_group : (Socket.inet6, Socket.inet6 Sockaddr.t) t
  val v6_multicast_hops : (Socket.inet6, int) t
  val v6_multicast_if : (Socket.inet6, Iface.t) t
  val v6_multicast_loop : (Socket.inet6, bool) t
  val unicast_hops : (Socket.inet6, int) t
  val v6only : (Socket.inet6, bool) t

  val set :
    ('a Socket.domain, _ Socket.typ) Socket.t ->
    ('a Socket.domain, 'b) t -> 'b ->
    (unit, string) result
end

val bind : ('a, _ Socket.typ) Socket.t -> 'a Sockaddr.t -> (unit, string) result
val connect : ('a, _ Socket.typ) Socket.t -> 'a Sockaddr.t -> (unit, string) result

type sendrecvflag =
  | Confirm
  | Dontroute
  | Dontwait
  | Eor
  | More
  | Nosignal
  | Oob
  | Cmsg_cloexec
  | Errqueue
  | Peek
  | Trunc
  | Waitall

val send :
  ?saddr:_ Sockaddr.t ->
  ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t ->
  Cstruct.t -> (int, string) result

val send_bytes :
  ?saddr:_ Sockaddr.t ->
  ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t ->
  Bytes.t -> int -> int -> (int, string) result

val recv :
  ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t ->
  Cstruct.t -> (int, string) result

val recv_bytes :
  ?flags:sendrecvflag list ->
  (_ Socket.domain, _ Socket.typ) Socket.t ->
  Bytes.t -> int -> int -> (int, string) result

val recvfrom :
  ?flags:sendrecvflag list ->
  ('a Socket.domain, _ Socket.typ) Socket.t -> Cstruct.t ->
  (int * 'a Socket.domain Sockaddr.t, string) result

val recvfrom_bytes :
  ?flags:sendrecvflag list ->
  ('a Socket.domain, _ Socket.typ) Socket.t -> Bytes.t -> int -> int ->
  (int * 'a Socket.domain Sockaddr.t, string) result

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
