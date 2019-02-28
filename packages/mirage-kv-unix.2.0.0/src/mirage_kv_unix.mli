(*
 * Copyright (c) 2013-2014 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2014      Thomas Gazagnaire <thomas@gazagnaire.org>
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

(** Loopback implementation of the FS signature. *)

[@@@ocaml.warning "-34"]

type error = [ Mirage_kv.error | `Storage_error of Mirage_kv.Key.t * string ]
type write_error = [ Mirage_kv.write_error | `Storage_error of Mirage_kv.Key.t * string | `Key_exists of Mirage_kv.Key.t ]

include Mirage_kv_lwt.RW
  with type error := error
   and type write_error := write_error

val connect : string -> t Lwt.t
