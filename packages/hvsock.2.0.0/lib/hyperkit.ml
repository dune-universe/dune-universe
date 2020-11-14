(*
 * Copyright (C) 2018 Docker Inc
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

type sockaddr = {
  hyperkit_path: string;
  port: port;
}

let string_of_sockaddr { hyperkit_path; port } =
  Printf.sprintf "Hyperkit AF_VSOCK { hyperkit_path = %s; port = %lx }" hyperkit_path port

let create () =
  let fd = Unix.(socket PF_UNIX SOCK_STREAM 0) in
  Unix.(setsockopt_int fd SO_SNDBUF) (1 lsl 15);
  Unix.(setsockopt_int fd SO_RCVBUF) (1 lsl 15);
  fd

let connect ?timeout_ms:_ fd { hyperkit_path; port } =
  Unix.(connect fd (ADDR_UNIX (Filename.concat hyperkit_path "connect")));
  let line = Bytes.of_string (Printf.sprintf "00000003.%08lx\n" port) in
  let n = Unix.write fd line 0 (Bytes.length line) in
  if n < (Bytes.length line) then raise End_of_file

let bind fd { hyperkit_path; port } =
  let port' = Printf.sprintf "00000002.%08lx" port in
  let path = Filename.concat hyperkit_path port' in
  (try Unix.unlink path with Unix.Unix_error(Unix.ENOENT, _, _) -> ());
  Unix.(bind fd (ADDR_UNIX path))

let accept fd =
  let new_fd, _unix_sockaddr = Unix.accept fd in
  (* We don't know the remote AF_VSOCK sockaddr *)
  new_fd, { hyperkit_path = "<unknown hyperkit>"; port = -1l }

let read_into = Af_common.read_into
let writev = Af_common.writev

let shutdown_read fd = Unix.shutdown fd Unix.SHUTDOWN_RECEIVE
let shutdown_write fd = Unix.shutdown fd Unix.SHUTDOWN_SEND
let close = Unix.close
let listen = Unix.listen