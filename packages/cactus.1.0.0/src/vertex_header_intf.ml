(*
 * Copyright (c) 2021 Tarides <contact@tarides.com>
 * Copyright (c) 2021 Gabriel Belouze <gabriel.belouze@ens.psl.eu>
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

module type H = sig
  module Common : Field.COMMON
  (** Module for all common metadata. *)

  module Nentry : Field.INT

  module Ndeadentry : Field.INT

  type t

  val load : marker:(unit -> unit) -> bytes -> t

  val init : t -> Field.kind -> unit

  val size : int

  val pp : t Fmt.t

  val g_magic : t -> Common.Magic.t

  val s_magic : t -> Common.Magic.t -> unit

  val g_kind : t -> Common.Kind.t

  val s_kind : t -> Common.Kind.t -> unit

  val g_nentry : t -> Nentry.t

  val s_nentry : t -> Nentry.t -> unit

  val g_ndeadentry : t -> Ndeadentry.t

  val s_ndeadentry : t -> Ndeadentry.t -> unit
end

module type MAKER = functor (Params : Params.S) (Store : Store.S) (Common : Field.COMMON) ->
  H with module Common := Common

module type Vertex_header = sig
  module Make : MAKER
end
