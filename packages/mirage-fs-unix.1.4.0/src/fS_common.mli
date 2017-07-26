(*
 * Copyright (c) 2013 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014 Thomas Gazagnaire <thomas@gazagnaire.org>
 * Copyright (c) 2014 Hannes Mehnert <hannes@mehnert.org>
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

open Result

type fs_error = [
  | `Unix_error of Unix.error
  | `Unix_errorno of int
  | `Negative_bytes
]
type error = [ Mirage_fs.error | fs_error ]
type write_error = [ Mirage_fs.write_error | fs_error | `Directory_not_empty ]

val pp_error: error Fmt.t
val pp_write_error: write_error Fmt.t
val mem_impl: string -> string -> (bool,  error) result Lwt.t
val read_impl: string -> string -> int -> int -> (Cstruct.t list, error) result Lwt.t
val size_impl: string -> string -> (int64, error) result Lwt.t
val resolve_filename: string -> string -> string
val map_write_error: Unix.error -> ('a, write_error) result
val map_error: Unix.error -> ('a, error) result
val err_catcher: exn -> ('a, error) result Lwt.t
val write_err_catcher: exn -> ('a, write_error) result Lwt.t
