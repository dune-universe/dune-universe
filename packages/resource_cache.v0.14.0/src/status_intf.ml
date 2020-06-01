open! Core_kernel
open! Async_kernel
open! Import

module type S = sig
  module Key : sig
    type t [@@deriving sexp_of]

    include Comparable.S_plain with type t := t
    include Hashable.S_plain with type t := t
  end

  module Resource : sig
    type state =
      [ `Busy
      | `Idle
      | `Closing
      ]
    [@@deriving sexp_of, compare]

    type t =
      { state : state
      ; since : Time_ns.Span.t
      }
    [@@deriving fields, sexp_of, compare]
  end

  module Resource_list : sig
    type t [@@deriving sexp_of, compare]

    val key : t -> Key.t
    val resources : t -> Resource.t list
    val queue_length : t -> int
    val max_time_on_queue : t -> Time_ns.Span.t option
  end

  type t [@@deriving sexp_of, compare]

  val resource_lists : t -> Resource_list.t list
  val num_jobs_in_cache : t -> int

  module Make_stable : sig
    module V1 (Key : sig
        type t = Key.t [@@deriving sexp, bin_io]
      end) : sig
      type nonrec t = t [@@deriving sexp, bin_io]
    end
  end
end

module type Status = sig
  module type S = S
end
