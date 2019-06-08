(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*           Mark Shinwell and Leo White, Jane Street Europe              *)
(*                                                                        *)
(*   Copyright 2015--2017 Jane Street Group LLC                           *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

module Position : sig

  type t

  val filename : t -> string
  val line_number : t -> int
  val start_char : t -> int
  val end_char : t -> int

  val print : Format.formatter -> t -> unit

end

module Location : sig

  type t

  val address : t -> Int64.t
  val symbol : t -> string option
  (* CR-soon mshinwell: We should think about a better representation for
     the inlined frames---perhaps trying to flatten them out into [Backtrace.t].
     However there are some tricky issues (addresses won't be unique keys,
     the structure of the code in Location.create_{ocaml,foreign} etc). *)
  val position : t -> Position.t list
  val foreign : t -> bool

  val print : Format.formatter -> t -> unit

end

module Backtrace : sig

  type t = Location.t list

  val print : Format.formatter -> t -> unit

end

module Allocation_entry : sig

  type t

  val backtrace : t -> Backtrace.t
  val blocks : t -> int
  val words : t -> int
  val allocations : t -> int

end

module Call_entry : sig

  type t

  val backtrace : t -> Backtrace.t
  val direct : t -> bool
  val calls : t -> int

end

module Stats : sig

  type t

  val minor_words : t -> int
  val promoted_words : t -> int
  val major_words : t -> int
  val minor_collections : t -> int
  val major_collections : t -> int
  val heap_words : t -> int
  val heap_chunks : t -> int
  val compactions : t -> int
  val top_heap_words : t -> int
  val words_scanned : t -> int
  val words_scanned_with_profinfo : t -> int

end


module Snapshot : sig

  type t

  val time : t -> float
  val stats : t -> Stats.t option
  val allocation_entries : t -> Allocation_entry.t list

end

module Series : sig

  type t

  val create : ?executable:string -> string -> t
  val snapshots : t -> Snapshot.t list
  val call_entries : t -> Call_entry.t list
  val has_call_counts : t -> bool
  val final_time : t -> float

end
