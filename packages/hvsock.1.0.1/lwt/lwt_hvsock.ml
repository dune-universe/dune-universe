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

open Hvsock
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

module type HVSOCK = sig
  type t
  val create: unit -> t
  val to_fd: t -> Unix.file_descr option
  val bind: t -> sockaddr -> unit
  val listen: t -> int -> unit
  val accept: t -> (t * sockaddr) Lwt.t
  val connect: ?timeout_ms:int -> t -> sockaddr -> unit Lwt.t
  val read: t -> Cstruct.t -> int Lwt.t
  val write: t -> Cstruct.t -> int Lwt.t
  val close: t -> unit Lwt.t
end

type buffer = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

external stub_ba_recv: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_recv"
external stub_ba_send: Unix.file_descr -> buffer -> int -> int -> int = "stub_hvsock_ba_send"

let cstruct_read fd b = stub_ba_recv fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len
let cstruct_write fd b = stub_ba_send fd b.Cstruct.buffer b.Cstruct.off b.Cstruct.len

type ('a, 'b) r =
  | Ok of 'a
  | Error of 'b

type result = (int, exn) r

type op = {
  file_descr: Unix.file_descr;
  buf: Cstruct.t;
}

module type FN = sig
  type ('request, 'response) t

  val create: ('request -> 'response) -> ('request, 'response) t
  val destroy: ('request, 'response) t -> unit

  val fn: ('request, 'response) t -> 'request -> 'response Lwt.t
end


module Make(Time: Mirage_time_lwt.S)(Fn: FN) = struct

type t = {
  mutable fd: Unix.file_descr option;
  read: (op, int) Fn.t;
  write: (op, int) Fn.t;
}

let make fd =
  let read = Fn.create (fun op -> cstruct_read op.file_descr op.buf) in
  let write = Fn.create (fun op -> cstruct_write op.file_descr op.buf) in
  { fd = Some fd; read; write; }

let create () = make (create ())

let to_fd t = t.fd

let detach f x =
  let fn = Fn.create f in
  Lwt.finalize
    (fun () -> Fn.fn fn x)
    (fun () -> Fn.destroy fn; Lwt.return_unit)

let close t = match t with
  | { fd = None } -> Lwt.return ()
  | { fd = Some x } ->
    t.fd <- None;
    Fn.destroy t.read;
    Fn.destroy t.write;
    detach Unix.close x

let bind t addr = match t with
  | { fd = None } -> raise (Unix.Unix_error(Unix.EBADF, "bind", ""))
  | { fd = Some x } -> bind x addr

let listen t n = match t with
  | { fd = None } -> raise (Unix.Unix_error(Unix.EBADF, "bind", ""))
  | { fd = Some x } -> Unix.listen x n

let accept = function
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "accept", ""))
  | { fd = Some x } ->
    detach accept x
    >>= fun (y, addr) ->
    Lwt.return (make y, addr)

let connect ?timeout_ms t addr = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "connect", ""))
  | { fd = Some x } ->
    detach (connect ?timeout_ms x) addr

let read t buf = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "read", ""))
  | { fd = Some file_descr; read } ->
    Fn.fn read { file_descr; buf }

let write t buf = match t with
  | { fd = None } -> Lwt.fail (Unix.Unix_error(Unix.EBADF, "write", ""))
  | { fd = Some file_descr; write } ->
    Fn.fn write { file_descr; buf }
end
