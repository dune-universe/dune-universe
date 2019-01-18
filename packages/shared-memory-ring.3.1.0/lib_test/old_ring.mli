(*
 * Copyright (c) 2011 Anil Madhavapeddy <anil@recoil.org>
 * Copyright (c) 2012 Citrix Systems, Inc
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

type buf = (char, Bigarray.int8_unsigned_elt, Bigarray.c_layout) Bigarray.Array1.t

module C_Console : sig
  type t
  val zero: t -> unit
  external unsafe_write : t -> string -> int -> int = "caml_console_ring_write"
  external unsafe_read : t -> string -> int -> int = "caml_console_ring_read"
  module Back : sig
	  val unsafe_write : t -> string -> int -> int
	  val unsafe_read : t -> string -> int -> int
  end
  val of_buf : buf -> t
end

module C_Xenstore : sig
  type t
  val zero: t -> unit
  external unsafe_write : t -> string -> int -> int = "caml_xenstore_ring_write"
  external unsafe_read : t -> string -> int -> int = "caml_xenstore_ring_read"
  module Back : sig
	  val unsafe_write : t -> string -> int -> int
	  val unsafe_read : t -> string -> int -> int
  end
  val of_buf : buf -> t
end
