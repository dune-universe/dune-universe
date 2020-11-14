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

open Lwt.Infix

(* Workarounds:
   1. select() is not implemented so we can't use regular non-blocking I/O
      i.e. we must use first class threads. Note that Lwt_Fn calls
      can block if the thread pool fills up. We create our own threads per
      connection to avoid this.
   2. connect() blocks forever instead of failing with ECONNREFUSED if the
      server is down when the client calls connect. We declare a 1s timeout
      and raise ECONNREFUSED ourselves.
*)

module Make(Time: Mirage_time_lwt.S)(Fn: S.FN)(Socket_family: Hvsock.Af_common.S) = struct

type op = {
  file_descr: Socket_family.t;
  buf: Cstruct.t;
}

type fd = Socket_family.t

type t = {
  mutable fd: Socket_family.t option;
  read: (op, int) Fn.t;
  write: (op, int) Fn.t;
}

type sockaddr = Socket_family.sockaddr

let string_of_sockaddr = Socket_family.string_of_sockaddr

let make fd =
  let read = Fn.create (fun op -> Socket_family.read_into op.file_descr op.buf) in
  let write = Fn.create (fun op -> Socket_family.writev op.file_descr [ op.buf ]) in
  { fd = Some fd; read; write; }

let create () = make (Socket_family.create ())

let to_fd t = t.fd

let detach f x =
  let fn = Fn.create f in
  Lwt.finalize
    (fun () -> Fn.fn fn x)
    (fun () -> Fn.destroy fn; Lwt.return_unit)

let close t = match t with
  | { fd = None; _ } -> Lwt.return ()
  | { fd = Some x; _ } ->
    t.fd <- None;
    Fn.destroy t.read;
    Fn.destroy t.write;
    detach Socket_family.close x

let shutdown_read t = match t with
  | { fd = None; _ } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "shutdown_read", ""))
  | { fd = Some x; _ } ->
    detach Socket_family.shutdown_read x

let shutdown_write t = match t with
  | { fd = None; _ } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "shutdown_write", ""))
  | { fd = Some x; _ } ->
    detach Socket_family.shutdown_write x

let bind t addr = match t with
  | { fd = None; _ } -> raise (Unix.Unix_error(Unix.EBADF, "bind", ""))
  | { fd = Some x; _ } -> Socket_family.bind x addr

let listen t n = match t with
  | { fd = None; _ } -> raise (Unix.Unix_error(Unix.EBADF, "bind", ""))
  | { fd = Some x; _ } -> Socket_family.listen x n

let accept = function
  | { fd = None; _ } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "accept", ""))
  | { fd = Some x; _ } ->
    detach Socket_family.accept x
    >>= fun (y, addr) ->
    Lwt.return (make y, addr)

let connect ?timeout_ms t addr = match t with
  | { fd = None; _ } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "connect", ""))
  | { fd = Some x; _ } ->
    detach (Socket_family.connect ?timeout_ms x) addr

let read t buf = match t with
  | { fd = None; _ } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "read", ""))
  | { fd = Some file_descr; read; _ } ->
    Fn.fn read { file_descr; buf }

let write t buf = match t with
  | { fd = None; _ } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "write", ""))
  | { fd = Some file_descr; write; _ } ->
    Fn.fn write { file_descr; buf }
end
