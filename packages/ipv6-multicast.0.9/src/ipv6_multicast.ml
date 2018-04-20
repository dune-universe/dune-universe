(*---------------------------------------------------------------------------
   Copyright (c) 2016 Vincent Bernardoff. All rights reserved.
   Distributed under the ISC license, see terms at the end of the file.
  ---------------------------------------------------------------------------*)

open Rresult

external strerror : Bytes.t -> int = "ml_strerror_r" [@@noalloc]
let strerror () =
  let buf = Bytes.create 1024 in
  let endp = strerror buf in
  Error (Bytes.sub_string buf 0 endp)

let wrap  f = match f () with -1 -> strerror () | ret -> Ok ret
let wrap1 f a = match f a with -1 -> strerror () | ret -> Ok ret
let wrap2 f a b = match f a b with -1 -> strerror () | ret -> Ok ret
let wrap3 f a b c = match f a b c with -1 -> strerror () | ret -> Ok ret
let wrap4 f a b c d = match f a b c d with -1 -> strerror () | ret -> Ok ret
let wrap5 f a b c d e = match f a b c d e with -1 -> strerror () | ret -> Ok ret
let wrap6 f a b c d e g = match f a b c d e g with -1 -> strerror () | ret -> Ok ret
let wrap7 f a b c d e g h = match f a b c d e g h with -1 -> strerror () | ret -> Ok ret

module Iface = struct
  external scope_id_of_iface : string -> int = "ml_if_nametoindex" [@@noalloc]
  type t = int
  let of_string name = wrap1 scope_id_of_iface name
  let of_string_opt name = R.to_option (of_string name)
  let of_string_exn name = match (of_string name) with
  | Ok ret -> ret
  | Error msg -> failwith msg

  let to_int (x : t) = (x : int)
  let to_int32 = Int32.of_int
end

external sizeof_sockaddr_storage : unit -> int = "sizeof_sockaddr_storage" [@@noalloc]
let sockaddr_storage_bytes = sizeof_sockaddr_storage ()

external bind :
  Unix.file_descr -> Cstruct.buffer -> int = "ml_bind" [@@noalloc]
external connect :
  Unix.file_descr -> Cstruct.buffer -> int = "ml_connect" [@@noalloc]


module Socket = struct
  type _ domain =
    | Unix : [`Unix] domain
    | Inet : [`Inet] domain
    | Inet6 : [`Inet6] domain

  type unix = [`Unix] domain
  type inet = [`Inet] domain
  type inet6 = [`Inet6] domain

  type 'a typ = ([< `Stream | `Dgram | `Raw | `Seqpacket] as 'a)

  let unix = Unix
  let inet = Inet
  let inet6 = Inet6

  let stream = `Stream
  let dgram = `Dgram
  let raw = `Raw
  let seqpacket = `Seqpacket

  let socket_domain_of_domain :
    type a. a domain -> Unix.socket_domain = function
  | Unix -> Unix.PF_UNIX
  | Inet -> Unix.PF_INET
  | Inet6 -> Unix.PF_INET6

  let socket_type_of_typ = function
  | `Stream -> Unix.SOCK_STREAM
  | `Dgram -> Unix.SOCK_DGRAM
  | `Raw -> Unix.SOCK_RAW
  | `Seqpacket -> Unix.SOCK_SEQPACKET

  type (_, _) t =
      Socket : (Unix.file_descr * 'a domain * 'b typ) -> ('a domain, 'b typ) t

  let create ?(proto=0) domain typ =
    let fd =
      Unix.socket (socket_domain_of_domain domain) (socket_type_of_typ typ) proto in
    Socket (fd, domain, typ)

  let to_fd (Socket (fd, _, _)) = fd
end

module Sockaddr = struct
  type _ family =
    | Af_inet : [`Inet] family
    | Af_inet6 : [`Inet6] family
    | Af_unix : [`Unix] family
    | Af_unspec : [`Unspec] family

  external families : Cstruct.buffer -> int = "families" [@@noalloc]

  let cs_families =
    let cs = Cstruct.create (12 * 4) in
    let len = families cs.buffer in
    Cstruct.sub cs 0 len

  let int_of_family : type a. a family -> int = fun family ->
    Cstruct.LE.get_uint32 cs_families (4 * (Obj.magic family : int)) |>
    Int32.to_int

  type _ t =
    | U : string -> Socket.unix t
    | V4 : { addr : Ipaddr.V4.t ;
             port : int } -> Socket.inet t
    | V6 : { addr : Ipaddr.V6.t ;
             port : int ;
             flowinfo : Int32.t ;
             scope_id : Int32.t
           } -> Socket.inet6 t

  let of_unix path =
    if String.length path > 108 then
      invalid_arg "Sockaddr.of_unix: path too long" ;
    U path
  let of_ipv4_port addr port = V4 { addr ;  port }
  let of_ipv6_port ?(flowinfo=0l) ?(scope_id=0l) addr port =
    V6 { addr ; port ; flowinfo ; scope_id }

  let to_sockaddr : type a. a t -> Unix.sockaddr = function
  | U path -> Unix.ADDR_UNIX path
  | V4 { addr ; port } -> Unix.ADDR_INET (Ipaddr_unix.V4.to_inet_addr addr, port)
  | V6 { addr ; port } -> Unix.ADDR_INET (Ipaddr_unix.V6.to_inet_addr addr, port)

  let of_bytes_unix ?(pos=0) cs =
    let cs = Cstruct.shift cs pos in
    let family = Cstruct.LE.get_uint16 cs 0 in
    if family = int_of_family Af_unix then
      let path = Cstruct.(to_string (sub cs 2 (len cs))) in
      Some (U (String.(sub path 0 (index path '\x00'))))
    else None

  let of_bytes_v4 ?(pos=0) cs =
    let cs = Cstruct.shift cs pos in
    let family = Cstruct.LE.get_uint16 cs 0 in
    if family = int_of_family Af_inet then
      let port = Cstruct.BE.get_uint16 cs 2 in
      let addr = Cstruct.BE.get_uint32 cs 4 |> Ipaddr.V4.of_int32 in
      Some (V4 { addr ; port })
    else None

  let of_bytes_v6 ?(pos=0) cs =
    let cs = Cstruct.shift cs pos in
    let family = Cstruct.LE.get_uint16 cs 0 in
    if family = (int_of_family Af_inet6) then
      let port = Cstruct.BE.get_uint16 cs 2 in
      let flowinfo = Cstruct.LE.get_uint32 cs 4 in
      let addr = Cstruct.(sub cs 8 16 |> to_string |> Ipaddr.V6.of_bytes_exn) in
      let scope_id = Cstruct.LE.get_uint32 cs 24 in
      Some (V6 { addr ; port ; flowinfo ; scope_id })
    else None

  let of_bytes :
    type a. ?pos:int -> Cstruct.t -> a Socket.domain ->
    a Socket.domain t option = fun ?pos cs -> function
  | Socket.Unix -> of_bytes_unix ?pos cs
  | Socket.Inet -> of_bytes_v4 ?pos cs
  | Socket.Inet6 -> of_bytes_v6 ?pos cs

  let of_bytes_exn ?pos cs domain =
    match of_bytes ?pos cs domain with
    | None -> invalid_arg "Sockaddr.of_bytes_exn"
    | Some saddr -> saddr

  let write :
    type a. ?pos:int -> Cstruct.t -> a t -> unit = fun ?(pos=0) cs ->
    let cs = Cstruct.shift cs pos in
    function
    | U path ->
        Cstruct.LE.set_uint16 cs 0 (int_of_family Af_unix) ;
        Cstruct.blit_from_string path 0 cs 2 (String.length path)
    | V4 { addr ; port } ->
        Cstruct.LE.set_uint16 cs 0 (int_of_family Af_inet) ;
        Cstruct.BE.set_uint16 cs 2 port ;
        Cstruct.blit_from_string (Ipaddr.V4.to_bytes addr) 0 cs 4 4
    | V6 { addr ; port ; flowinfo ; scope_id } ->
        Cstruct.LE.set_uint16 cs 0 (int_of_family Af_inet6) ;
        Cstruct.BE.set_uint16 cs 2 port ;
        Cstruct.LE.set_uint32 cs 4 flowinfo ;
        Cstruct.blit_from_string (Ipaddr.V6.to_bytes addr) 0 cs 8 16 ;
        Cstruct.LE.set_uint32 cs 24 scope_id

  let to_bytes saddr =
    let cs = Cstruct.create sockaddr_storage_bytes in
    write cs saddr ;
    cs

  let to_group_req ?(iface=0) saddr =
    let cs = Cstruct.create (sockaddr_storage_bytes + 4) in
    Cstruct.LE.set_uint32 cs 0 (Int32.of_int iface) ;
    write ~pos:4 cs saddr ;
    cs
end

module Sockopt = struct
  external setsockopt :
    Unix.file_descr -> Int32.t -> Int32.t -> Cstruct.buffer -> int = "ml_setsockopt" [@@noalloc]
  external sizeof_ip_mreqn : unit -> int = "sizeof_ip_mreqn" [@@noalloc]
  let ip_mreqn_bytes = sizeof_ip_mreqn ()
  external sizeof_ipv6_mreq : unit -> int = "sizeof_ipv6_mreq" [@@noalloc]
  let ipv6_mreq_bytes = sizeof_ipv6_mreq ()
  external set_ip_mreqn : int -> string -> Cstruct.buffer -> unit = "set_ip_mreqn" [@@noalloc]
  external set_ipv6_mreq : int -> string -> Cstruct.buffer -> unit = "set_ipv6_mreq" [@@noalloc]

  type (_, _) t =
    | V4Multicast_if : (Socket.inet, Iface.t) t
    | V4Multicast_ttl : (Socket.inet, int) t
    | V4Multicast_loop : (Socket.inet, bool) t
    | V4Add_membership : (Socket.inet, Socket.inet Sockaddr.t * int) t
    | V4Drop_membership : (Socket.inet, Socket.inet Sockaddr.t * int) t
    | Join_group : (Socket.inet6, Socket.inet6 Sockaddr.t) t
    | Leave_group : (Socket.inet6, Socket.inet6 Sockaddr.t) t
    | Multicast_hops : (Socket.inet6, int) t
    | Multicast_if : (Socket.inet6, int) t
    | Multicast_loop : (Socket.inet6, bool) t
    | Unicast_hops : (Socket.inet6, int) t
    | V6only : (Socket.inet6, bool) t

  external sockopts : Cstruct.buffer -> int = "sockopts" [@@noalloc]
  let sockopts =
    let cs = Cstruct.create 1024 in
    let len = sockopts cs.buffer in
    Cstruct.sub cs 0 len

  let to_int32 : type a b. (a, b) t -> Int32.t =
    fun i -> Cstruct.LE.get_uint32 sockopts (4 * (Obj.magic i : int))

  let v4_multicast_if    = V4Multicast_if
  let v4_multicast_ttl   = V4Multicast_ttl
  let v4_multicast_loop  = V4Multicast_loop
  let v4_add_membership  = V4Add_membership
  let v4_drop_membership = V4Drop_membership
  let v6_join_group      = Join_group
  let v6_leave_group     = Leave_group
  let v6_multicast_hops  = Multicast_hops
  let v6_multicast_if    = Multicast_if
  let v6_multicast_loop  = Multicast_loop
  let unicast_hops       = Unicast_hops
  let v6only             = V6only

  external levels : Cstruct.buffer -> int = "levels" [@@noalloc]
  let int32_of_level =
    let cs = Cstruct.create 1024 in
    let len = levels cs.buffer in
    let cs = Cstruct.sub cs 0 len in
    fun i -> Cstruct.LE.get_uint32 cs (4 * i)

  let int32_of_level : type a b. (a, b) t -> Int32.t = fun t ->
    int32_of_level @@ match t with
    | V4Multicast_if    -> 1
    | V4Multicast_ttl   -> 1
    | V4Add_membership  -> 1
    | V4Drop_membership -> 1
    | _ -> 2

  let set_cstruct :
    type a. (a, _) Socket.t -> (a, _) t -> Cstruct.t -> (unit, string) result =
    fun (Socket.Socket (fd, _, _)) optname cs ->
      (* Printf.printf "%d\n%!" (Cstruct.len cs) ; *)
      wrap4 setsockopt fd (int32_of_level optname)
        (to_int32 optname) (Cstruct.to_bigarray cs) |>
      R.map ignore

  let set_bool fd opt v =
    let cs = Cstruct.create_unsafe 4 in
    Cstruct.LE.set_uint32 cs 0 (if v then 1l else 0l) ;
    set_cstruct fd opt cs

  let set_int fd opt v =
    let cs = Cstruct.create_unsafe 4 in
    Cstruct.LE.set_uint32 cs 0 (Int32.of_int v) ;
    set_cstruct fd opt cs

  external sizeof_ip_mreqn : unit -> int = "sizeof_ip_mreqn" [@@noalloc]
  let ip_mreqn_bytes = sizeof_ip_mreqn ()
  external sizeof_ipv6_mreq : unit -> int = "sizeof_ipv6_mreq" [@@noalloc]
  let ipv6_mreq_bytes = sizeof_ipv6_mreq ()

  external set_ip_mreqn : Cstruct.buffer -> Cstruct.buffer -> int -> unit =
    "set_ip_mreqn" [@@noalloc]
  external set_ipv6_mreq : Cstruct.buffer -> Cstruct.buffer -> unit =
    "set_ipv6_mreq" [@@noalloc]

  let ip_mreqn_of_sockaddr (saddr, iface) =
    let cs = Cstruct.create ip_mreqn_bytes in
    set_ip_mreqn cs.buffer (Sockaddr.to_bytes saddr |> Cstruct.to_bigarray) iface ;
    cs

  let ipv6_mreq_of_sockaddr saddr =
    let cs = Cstruct.create ipv6_mreq_bytes in
    set_ipv6_mreq cs.buffer (Sockaddr.to_bytes saddr |> Cstruct.to_bigarray) ;
    cs

  let set :
    type a b.
    (a Socket.domain, _ Socket.typ) Socket.t ->
    (a Socket.domain, b) t -> b -> (unit, string) result =
    fun socket opt arg -> match opt with
    | V4Multicast_if -> set_int socket opt (Iface.to_int arg)
    | V4Multicast_ttl -> set_int socket opt arg
    | V4Multicast_loop -> set_bool socket opt arg
    | V4Add_membership -> set_cstruct socket opt (ip_mreqn_of_sockaddr arg)
    | V4Drop_membership -> set_cstruct socket opt (ip_mreqn_of_sockaddr arg)

    | Join_group -> set_cstruct socket opt (ipv6_mreq_of_sockaddr arg)
    | Leave_group -> set_cstruct socket opt (ipv6_mreq_of_sockaddr arg)
    | Multicast_hops -> set_int socket opt arg
    | Multicast_if -> set_int socket opt (Iface.to_int arg)
    | Multicast_loop -> set_bool socket opt arg
    | Unicast_hops -> set_int socket opt arg
    | V6only -> set_bool socket opt arg
end

let bind :
  type a. ((a, _) Socket.t) -> a Sockaddr.t -> (unit, string) result =
  fun (Socket.Socket (fd, _, _)) saddr ->
    wrap2 bind fd (Sockaddr.to_bytes saddr |> Cstruct.to_bigarray) |>
    R.map ignore

let connect :
  type a. ((a, _) Socket.t) -> a Sockaddr.t -> (unit, string) result =
  fun (Socket.Socket (fd, _, _)) saddr ->
    wrap2 connect fd (Sockaddr.to_bytes saddr |> Cstruct.to_bigarray) |>
    R.map ignore

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

external sendrecvflags : Cstruct.buffer -> int = "sendrecvflags" [@@noalloc]
let int_of_sendrecvflag =
  let cs = Cstruct.create 1024 in
  let len = sendrecvflags cs.buffer in
  let cs = Cstruct.sub cs 0 len in
  fun (i : sendrecvflag) -> Cstruct.LE.get_uint32 cs (4 * (Obj.magic i : int))

let int_of_flags flags =
  List.fold_left (fun acc f -> Int32.logor acc (int_of_sendrecvflag f)) 0l flags

external send :
  Unix.file_descr -> Cstruct.buffer -> Int32.t -> int = "ml_send" [@@noalloc]
external sendto :
  Unix.file_descr -> Cstruct.buffer -> Int32.t -> Cstruct.buffer -> int = "ml_sendto" [@@noalloc]
external send_bytes :
  Unix.file_descr -> Bytes.t -> int -> int -> Int32.t -> int = "ml_send_bytes" [@@noalloc]
external sendto_bytes :
  Unix.file_descr -> Bytes.t -> int -> int -> Int32.t -> Cstruct.buffer -> int =
  "ml_sendto_bytes_bytecode" "ml_sendto_bytes" [@@noalloc]
external recv :
  Unix.file_descr -> Cstruct.buffer -> Int32.t -> int = "ml_recv" [@@noalloc]
external recvfrom :
  Unix.file_descr -> Cstruct.buffer -> Int32.t ->
  Cstruct.buffer -> Cstruct.buffer -> int = "ml_recvfrom" [@@noalloc]
external recv_bytes :
  Unix.file_descr -> Bytes.t -> int -> int -> Int32.t -> int = "ml_recv_bytes" [@@noalloc]
external recvfrom_bytes :
  Unix.file_descr -> Bytes.t -> int -> int -> Int32.t ->
  Cstruct.buffer -> Cstruct.buffer -> int =
  "ml_recvfrom_bytes_bytecode" "ml_recvfrom_bytes" [@@noalloc]

let send ?saddr ?(flags=[]) (Socket.Socket (fd, _, _)) buf =
  match saddr with
  | None -> wrap3 send fd buf.Cstruct.buffer (int_of_flags flags)
  | Some saddr ->
      Sockaddr.to_bytes saddr |> Cstruct.to_bigarray |>
      wrap4 sendto fd buf.Cstruct.buffer (int_of_flags flags)

let send_bytes ?saddr ?(flags=[]) (Socket.Socket (fd, _, _)) buf pos len =
  if (pos < 0 || len < 0 || pos + len > Bytes.length buf)
  then invalid_arg "send_bytes: bounds" ;
  match saddr with
  | None -> wrap5 send_bytes fd buf pos len (int_of_flags flags)
  | Some saddr ->
      Sockaddr.to_bytes saddr |> Cstruct.to_bigarray |>
      wrap6 sendto_bytes fd buf pos len (int_of_flags flags)

let recv ?(flags=[]) (Socket.Socket (fd, _, _)) buf =
  wrap3 recv fd buf.Cstruct.buffer (int_of_flags flags)

let recv_bytes ?(flags=[]) (Socket.Socket (fd, _, _)) buf pos len =
  if (pos < 0 || len < 0 || pos + len > Bytes.length buf)
  then invalid_arg "recv_bytes: bounds";
  wrap5 recv_bytes fd buf pos len (int_of_flags flags)

let recvfrom ?(flags=[]) (Socket.Socket (fd, domain, _)) buf =
  let open Cstruct in
  let cs = create_unsafe sockaddr_storage_bytes in
  let sizeof = create_unsafe 4 in
  Cstruct.LE.set_uint32 sizeof 0 (Int32.of_int sockaddr_storage_bytes) ;
  wrap5 recvfrom fd buf.buffer (int_of_flags flags)
    (to_bigarray cs) (to_bigarray sizeof) |>
  R.map begin fun nb_recv ->
    let len = Cstruct.LE.get_uint32 sizeof 0 |> Int32.to_int in
    let cs = Cstruct.sub cs 0 len in
    nb_recv, Sockaddr.of_bytes_exn cs domain
  end

let recvfrom_bytes ?(flags=[]) (Socket.Socket (fd, domain, _)) buf pos l =
  if (pos < 0 || l < 0 || pos + l > Bytes.length buf)
  then invalid_arg "recv_bytes: bounds";
  let open Cstruct in
  let cs = create_unsafe sockaddr_storage_bytes in
  let sizeof = create_unsafe 4 in
  Cstruct.LE.set_uint32 sizeof 0 (Int32.of_int sockaddr_storage_bytes) ;
  wrap7 recvfrom_bytes fd buf pos l (int_of_flags flags)
    (to_bigarray cs) (to_bigarray sizeof) |>
  R.map begin fun nb_recv ->
    let len = Cstruct.LE.get_uint32 sizeof 0 |> Int32.to_int in
    let cs = Cstruct.sub cs 0 len in
    nb_recv, Sockaddr.of_bytes_exn cs domain
  end

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
