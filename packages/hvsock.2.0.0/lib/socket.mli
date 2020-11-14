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

(** An interface for hypervisor sockets which hides as many of the differences
    between Linux, Windows and Mac as possible. *)

exception Unsupported_platform of string
(** An operation cannot be performed on this platform *)

type sockaddr
(** A socket address which can either be listened on or connected to. *)

val sockaddr_of_uri: Uri.t -> sockaddr
(** Parse a URI as a socket address. This currently supports
    - vsock://:80
    - hvsock://<VM GUID>/3049197C-9A4E-4FBF-9367-97F792F16994
    - hvsock://<VM name>/3049197C-9A4E-4FBF-9367-97F792F16994
    - vsock://2:80/
    - hyperkit://:80/Users/foo/Library/Containers/com.docker.docker/Data/vms/0
*)

val to_hyperv: sockaddr -> Af_hyperv.sockaddr option
(** Expose the AF_HYPERV socket address, if one exists *)

val to_vsock: sockaddr -> Af_vsock.sockaddr option
(** Expose the AF_VSOCK socket address, if one exists *)

val to_hyperkit: sockaddr -> Hyperkit.sockaddr option
(** Expose the hyperkit socket address, if one exists *)

include Af_common.S
  with type sockaddr := sockaddr
   and type t = Unix.file_descr
