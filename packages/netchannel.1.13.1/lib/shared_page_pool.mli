(*
 * Copyright (c) 2015 Thomas Leonard
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
 *)

(** Pool and reuse shared memory blocks. This saves having to reallocate and reshare
    pages of memory, which is slow. A block is half the size of a page, since network
    packets usually don't require more than this. *)

type t

val make : (OS.Xen.Gntref.t -> Io_page.t -> unit) -> t
(** [make grant_access] is a shared pool of blocks. When a new page is first
    allocated, [grant_access] is called to share it. *)

val use : t -> (id:Cstruct.uint16 -> OS.Xen.Gntref.t -> Cstruct.t -> ('a * unit Lwt.t) Lwt.t) -> ('a * unit Lwt.t) Lwt.t
(** [use t fn] calls [fn ~id gref block] with a free shared block of memory
    and that block's unique ID (note: the [gref] is NOT unique).
    The function should return a thread that indicates when the request has
    been added to the queue, by returning a result value and a second thread
    indicating when the block can be returned to the pool. *)

val blocks_needed : int -> int
(** Rounds a size in bytes up to the number of blocks needed for it. *)

val shutdown : t -> unit
(** Unshare and free all the pages of the pool once all outstanding requests
    have completed. Calling [use] after [shutdown] will return an error. *)
