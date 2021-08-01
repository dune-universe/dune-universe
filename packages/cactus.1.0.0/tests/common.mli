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

module MyKey : sig
  type t = string [@@deriving repr]

  val encode : t -> string

  val decode : string -> t

  val encoded_size : int
end

val generate_key : unit -> MyKey.t

module MyValue : sig
  type t = int * int * int [@@deriving repr]

  val encode : t -> string

  val decode : string -> t

  val encoded_size : int
end

module Input : Btree.Input.Size

module type TREE = Btree.S with type key = MyKey.t and type value = MyValue.t

val get_tree : ?random_failure:bool -> [ `V0 ] -> (module TREE)

val get_migrate_tree : [ `V0 ] -> (module TREE)

val set_report : string -> unit
(** [set_report root] set a reporter at root [root] *)

val v_to_s : [ `V0 ] -> string
