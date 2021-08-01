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

module type HEADER = sig
  module Common : Field.COMMON

  type t
  (** The type for store headers. *)

  val size : int
  (** The size in bytes of a header. *)

  val load : bytes -> t

  val dump : t -> bytes

  val init : t -> root:int -> unit
  (** [init t root] initialises the header using the parameters set when creating the btree. *)

  val pp : Format.formatter -> t -> unit

  val g_root : t -> Common.Address.t

  val s_root : t -> Common.Address.t -> unit
end

module type S = sig
  exception RandomFailure

  type t
  (** The type for store handles. *)

  type address = int
  (** The type of page addresses. *)

  type page
  (** The type of pages. *)

  module Common : Field.COMMON

  module Page : sig
    type t
    (** The type for pages. *)

    type pointer = int
    (** The type for pointers. *)

    val kind : t -> Common.Kind.t
    (** [kind t] is the kind of t, either a Node or a Leaf. *)

    val buff : t -> bytes
    (** [buff t] holds the contents of the page [t]. *)

    val marker : t -> unit -> unit
    (** [marker page] is the function that marks [page] as dirty. A page is dirty when its in memory
        contents are not the same as the ones on disk. *)

    val flush : t -> unit
    (** [flush t] writes the contents of [t] to the disk. *)
  end
  with type t = page

  val init : root:string -> t
  (** [init root] opens a store at path [root]. *)

  val root : t -> address
  (** [root t] is the address of the root of the btree. *)

  val reroot : t -> address -> unit
  (** [reroot s a] changes the address of the root of [s] to [a]. *)

  val load : t -> address -> page
  (** [load t a] loads in memory the page at address [a]. *)

  val reload : t -> address -> unit
  (** [reload t a] moves [a] between caches. Called after a newly created page, it adds it to the
      right level of cache. *)

  val release : t -> unit
  (** [release store] tells the store that no loaded pages is going to be written on. This allows
      the store to clean part of the cache, and must be called as frequently as possible. *)

  val clear_cache : t -> unit

  val allocate : t -> address
  (** [allocate t] allocates a new page (with junk contents) and returns the address of the new
      page.*)

  val deallocate : t -> address -> unit
  (** [deallocate t a] deallocates the page at the address [a]. *)

  val flush : t -> unit

  val fsync : t -> unit

  val clear : t -> unit
  (** [clear t] removes every binding in [t]. *)

  val close : t -> unit

  val iter : t -> (address -> page -> unit) -> unit
  (** [iter t f] applies f on all alive pages in the btree. *)

  val pp_header : t Fmt.t

  module Private : sig
    val dir : t -> string

    val cache_size : t -> int

    val write : t -> string -> unit

    val init_migration : t -> unit

    val end_migration : t -> int -> address -> unit
  end
end

module type MAKER = functor (Params : Params.S) (Common : Field.COMMON) ->
  S with module Common = Common

module type Store = sig
  module type S = S

  module Make : MAKER
end
