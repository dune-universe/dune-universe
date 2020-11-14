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

type t = Unix.file_descr

type port = int32

type cid =
  | Any
  | Hypervisor
  | Host
  | Id of int32

let string_of_cid = function
  | Any -> "Any"
  | Hypervisor -> "Hypervisor"
  | Host -> "Host"
  | Id x -> Printf.sprintf "Id %lx" x

let int_of_cid = function
  | Any -> -1
  | Hypervisor -> 0
  | Host -> 2
  | Id 1l -> invalid_arg "CID 1 is reserved and must not be used"
  | Id x -> Int32.to_int x

let cid_of_int = function
  | -1 -> Any
  | 0 -> Hypervisor
  | 1 -> invalid_arg "CID 1 is reserved and must not be used"
  | 2 -> Host
  | x -> Id (Int32.of_int x)

external vm_sockets_get_local_cid: unit -> int = "stub_vsock_get_local_cid"

let local () = Id (Int32.of_int (vm_sockets_get_local_cid ()))

type sockaddr = {
  cid: cid;
  port: port;
}

let string_of_sockaddr { cid; port } =
  Printf.sprintf "AF_VSOCK { cid = %s; port = %lx }" (string_of_cid cid) port

external do_socket: unit -> Unix.file_descr = "stub_vsock_socket"

external do_bind: Unix.file_descr -> int -> int -> unit = "stub_vsock_bind"

external do_accept: Unix.file_descr -> Unix.file_descr * int * int = "stub_vsock_accept"

external do_connect: Unix.file_descr -> int -> int -> unit = "stub_vsock_connect"

let create = do_socket

let bind fd { cid; port } = do_bind fd (int_of_cid cid) (Int32.to_int port)

let accept fd =
  let new_fd, cid, port = do_accept fd in
  new_fd, { cid = cid_of_int cid; port = Int32.of_int port }

let connect ?timeout_ms:_ fd { cid; port } = do_connect fd (int_of_cid cid) (Int32.to_int port)

let read_into = Af_common.read_into
let writev = Af_common.writev

let shutdown_read fd = Unix.shutdown fd Unix.SHUTDOWN_RECEIVE
let shutdown_write fd = Unix.shutdown fd Unix.SHUTDOWN_SEND
let close = Unix.close
let listen = Unix.listen