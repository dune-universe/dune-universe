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

module Make
  (Time: Mirage_time_lwt.S)
  (Fn: S.FN)
  (Socket_family: Hvsock.Af_common.S):
  S.SOCKET
    with type sockaddr = Socket_family.sockaddr
     and type fd = Socket_family.t
(** Create a Lwt socket from a
    - source of timing
    - a means of running blocking calls in full threads
    - a means of creating sockets

    This is useful because some of the hypervisor sockets do not support
    select() or other methods of asynchronous I/O and we must therefore
    run the calls in background threads. *)
