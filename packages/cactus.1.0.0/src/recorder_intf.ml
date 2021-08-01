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

module type S = sig
  type t

  type key

  type value

  type op = Add of (key * value) | Find of (key * bool) | Mem of (key * bool) | Flush

  val v : string -> t (* [v path] creates a recorder to file [path] *)

  val record : t -> op -> unit

  val close : t -> unit

  val operations : string -> op Seq.t
  (* [operations path] is the sequence of operations recorded in file [path] *)
end

module type MAKER = functor (InKey : Input.Key) (InValue : Input.Value) ->
  S with type key = InKey.t and type value = InValue.t

module type Recorder = sig
  module Make : MAKER
end
