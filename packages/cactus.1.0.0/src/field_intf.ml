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

module type FIELD = sig
  (** Modules of this type are an abstraction barrier between the in-memory and on-disk
      representation of the metadata. *)

  type t

  type convert

  val set : marker:(unit -> unit) -> bytes -> off:int -> t -> unit
  (** [set marker buff off t] sets the field value in [buff] at offset [off]. [marker] is a function
      to mark a page as dirty. *)

  val get : bytes -> off:int -> t

  val size : int

  val to_t : convert -> t

  val from_t : t -> convert

  val pp : t Fmt.t

  val pp_raw : Format.formatter -> bytes -> off:int -> unit
end

type kind = Leaf | Node of int [@@deriving repr]

module type INT = FIELD with type convert := int

module type BOOL = FIELD with type convert := bool

module type STRING = FIELD with type convert := string

module type KIND = sig
  include FIELD with type convert := kind

  val of_depth : int -> t
  (** [of_depth depth] returns the kind of a vertex based on its depth.*)

  val to_depth : t -> int
  (** [to_depth t] returns the depth a vertex from its kind. *)
end

module type SIZE = sig
  val size : int
end

module type COMMON = sig
  module Version : INT

  module Magic : STRING
  (** Magic strings are used to mark the header of a btree or of a page. *)

  module Address : INT
  (** The address of a page. *)

  module Pointer : INT
  (** A pointer is an offset inside a page. *)

  module Kind : KIND
  (** The kind of a vertex, either a Node or a Leaf. *)
end

module type Field = sig
  type nonrec kind = kind

  module type INT = INT

  module type BOOL = BOOL

  module type COMMON = COMMON

  module MakeInt : functor (Size : SIZE) -> INT

  module MakeBool : functor (Size : SIZE) -> BOOL

  module MakeString : functor (Size : SIZE) -> STRING

  module MakeCommon : functor (Params : Params.S) -> COMMON
end
