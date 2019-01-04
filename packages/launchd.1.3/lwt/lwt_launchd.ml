(*
 * Copyright (c) 2015 Unikernel Systems
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
open Lwt

external stub_launch_activate_socket: string -> Unix.file_descr = "stub_launch_activate_socket"

let read_int fd =
  let buffer = Cstruct.create 4 in
  Lwt_cstruct.complete (Lwt_cstruct.read fd) buffer
  >>= fun () ->
  return (Int32.to_int (Cstruct.BE.get_uint32 buffer 0))

let activate_socket name =
  let fd = Lwt_unix.of_unix_file_descr (stub_launch_activate_socket name) in
  read_int fd
  >>= function
  | 1 -> return (Result.Error (`Enoent name))
  | 2 -> return (Result.Error `Esrch)
  | 3 -> return (Result.Error `Ealready)
  | 0 ->
    read_int fd
    >>= fun n ->
    let rec read acc = function
      | 0 -> return (Result.Ok acc)
      | n ->
        read_int fd
        >>= fun x ->
        let (x': Unix.file_descr) = Obj.magic x in
        read (Lwt_unix.of_unix_file_descr x' :: acc) (n - 1) in
    read [] n
  | x -> failwith (Printf.sprintf "Internal error: received status code %d" x)
