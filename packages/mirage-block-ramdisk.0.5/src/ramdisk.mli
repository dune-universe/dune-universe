(*
 * Copyright (C) 2011-2013 Citrix Systems Inc
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

(** An in-memory BLOCK device also known as a Ramdisk *)

(** {2 Basic operation} *)

include Mirage_block.S

val connect: name:string -> t Lwt.t
(** Connect to the named ramdisk. *)

val create: name:string -> size_sectors:int64 -> sector_size:int
  -> (t, error) result Lwt.t
(** Create an in-memory block device (a "ramdisk") with a given name,
    total size in sectors and sector size. Two calls to [connect] with the
    same name will return the same block device *)

val destroy: name:string -> unit
(** Destroy removes an in-memory block device. Subsequent calls to
    [connect] will create a fresh empty device. *)

(** {2 Resizing support} *)

val resize : t -> int64 -> (unit, write_error) result Lwt.t
(** [resize t new_size_sectors] attempts to resize the connected device
    to have the given number of sectors. If successful, subsequent calls
    to [get_info] will reflect the new size. *)

(** {2 Querying sparseness information} *)

val seek_unmapped: t -> int64 -> (int64, error) result Lwt.t
(** [seek_unmapped t start] returns the offset of the next guaranteed
    zero-filled region (typically guaranteed because it is unmapped) *)

val seek_mapped: t -> int64 -> (int64, error) result Lwt.t
(** [seek_mapped t start] returns the offset of the next regoin of the
    device which may have data in it (typically this is the next mapped
    region) *)

(** {2 Compatibility} *)

val flush : t -> (unit, write_error) result Lwt.t
(** [flush t] is a no-op on a Ramdisk *)
