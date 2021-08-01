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

module Func : sig
  (** This module exposes functions to produce statistics on function calls *)

  type t

  val v : counter_names:string list -> t
  (** creates a handle for function call statistics. The optional argument can be used to create
      named additional internal counters to keep track, for instance, of the size of the inputs. Use
      [] to only use the default counter that keeps track of the number of function calls *)

  val increment : t -> string -> int -> unit
  (** [incr t name n] increments the named counter with name [name] of [t] by [n] units*)

  val tic : t -> unit
  (** Play the internal clock *)

  val tac : t -> unit
  (** [tac t n] pauses the internal clock of [t]. Must follow a tic. *)

  val get_count : ?name:string -> t -> int
  (** Number of tic-tacs if no [name] is provided, else the counter associated to [name] *)

  val get_span : t -> Mtime.span
  (** Total time taken by tic-tacs *)

  val pp : t Fmt.t
end

type t

val reset : unit -> unit

val get : unit -> t

val get_by_name : t -> modul:string -> stat:string -> Func.t

val pp : t Fmt.t

val pp_json : t Fmt.t

type module_stats

module type Common = sig
  val name : string

  val reset : unit -> unit

  val setup_log : string list -> unit

  val get : unit -> module_stats

  val pp : module_stats Fmt.t
end

module Btree : sig
  include Common

  val stat_add : Func.t

  val stat_find : Func.t

  val stat_mem : Func.t
end

module Nodes : sig
  include Common

  val stat_create : Func.t

  val stat_load : Func.t

  val stat_split : Func.t

  val stat_shift : Func.t

  val stat_add : Func.t

  val stat_find : Func.t

  val stat_mem : Func.t
end

module Store : sig
  include Common

  val stat_flush : Func.t

  val stat_io_r : Func.t
  (** read syscalls *)

  val stat_io_w : Func.t
  (** write syscalls *)

  val stat_fsync : Func.t
end

module Utils : sig
  include Common

  val stat_binary_search : Func.t
end

module Miscellaneous : sig
  type t = private { density : float }

  val get : unit -> t

  val add_density_sample : float -> unit
end
