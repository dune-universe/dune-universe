(*
 * Copyright (c) 2015 Magnus Skjegstad <magnus@skjegstad.com>
 * Copyright (c) 2011-2013 Anil Madhavapeddy <anil@recoil.org>
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

open Mirage_net

module type BACKEND = sig
  type 'a io = 'a Lwt.t
  type buffer = Cstruct.t
  type id = int
  type macaddr = Macaddr.t
  type t

  val register : t -> (id, Net.error) result
  val unregister : t -> id -> unit io
  val mac : t -> id -> macaddr
  val write : t -> id -> size:int -> (buffer -> int) -> (unit, Net.error) result io
  val set_listen_fn : t -> id -> (buffer -> unit io) -> unit
  val unregister_and_flush : t -> id -> unit io
end


(** Dummy interface for software bridge. *)
module Make(B : BACKEND) : sig
  include Mirage_net.S
  val connect : ?size_limit:int -> B.t -> t Lwt.t
end
