type order =
  | Asc
  | Desc

module type Comparable = sig
  type t

  val compare : t -> t -> int
  (** [compare a b] Returns an integer less than zero if the first argument is
      strictly less than the second, zero if the arguments are equal, and an
      integer greater than zero if the first argument is strictly greater than
      the second. *)
end

exception Invalid_interval

module Make (Bound_compare : Comparable) : sig
  module Bound : sig
    type t =
      | Included of Bound_compare.t
      | Excluded of Bound_compare.t
      | Unbounded

    val compare_lower : t -> t -> int
    (** [compare_lower bound_a bound_b] compares two bounds as lower bounds.
        Returns an integer less than zero if the [bound_a] is strictly less than
        [bound_b], zero if the bounds are equal, and an integer greater than
        zero if [bound_a] is strictly greater than [bound_b]. *)

    val compare_upper : t -> t -> int
    (** [compare_upper bound_a bound_b] compares two bounds as upper bounds *)

    val min_lower : t -> t -> t
    (** [min_lower bound_a bound_b] chooses the minimum lower bound between
        [bound_a] and [bound_b] *)

    val max_upper : t -> t -> t
    (** [max_upper bound_a bound_b] chooses the maximum upper bound between
        [bound_a] and [bound_b] *)
  end

  module Interval : sig
    type t = private
      { low : Bound.t
      ; high : Bound.t
      }

    val create : Bound.t -> Bound.t -> t
    (** [create low high] creates an interval from the [low] bound and the
        [high] bound. Raises [Invalid_interval] if low is not less than high. *)

    val compare : t -> t -> int
    (** [compare ivl_a ivl_b] compares two intervals. Intervals are compared
        using their low bounds. If the lower bounds are equal, the high bound
        are compared. Two bounds are equal iff both their bounds are equal. *)

    val overlap_interval : t -> t -> t option
    (** [overlap_interval ivl_a ivl_b] calculates the interval that is the
        overlap between the two intervals. If there is no overlap, [None] is
        returned.*)

    val overlaps : t -> t -> bool
    (** [overlaps ivl_a ivl_b] returns [true] if the two intervals overlap,
        [false] otherwise. *)
  end

  module Gen : sig
    type 'a t

    val next : 'a t -> ((Interval.t * 'a list) * 'a t) option
    (** [next gen] retrieves the next interval and associated values of [gen]
        along with the updated state of the generator or [None] if the generator
        is exhausted. *)

    val fold : ('acc -> Interval.t -> 'a list -> 'acc) -> 'acc -> 'a t -> 'acc
    (** [fold fn acc gen] folds over [gen] using function [fn], which takes the
        accumulator, interval, and values as parameters and returns the updated
        accumulator. *)
  end

  type 'a t

  val empty : 'a t
  (** [empty] is an empty interval map. *)

  val size : 'a t -> int
  (** [size map] returns the number of values stored in the [map]. Multiple
      values may be stored with each interval, so the number of values is not
      necessarily the same as the number of intervals. *)

  val cardinal : 'a t -> int
  (** [cardinal map] is the same as [size map] *)

  val add : Interval.t -> 'a -> 'a t -> 'a t
  (** [add interval value map] adds [value] to [map] associated with [interval].
      Not tail recursive. *)

  val remove_by : Interval.t -> ('a -> bool) -> 'a t -> 'a t
  (** [remove_by interval value_rm_fn map] removes all values associated with
      [interval] for which [value_rm_fn] returns true in [map]. Not tail
      recursive. *)

  val remove_interval : Interval.t -> 'a t -> 'a t
  (** [remove_interval interval map] removes the interval and all associated
      values from [map]. Not tail recursive. *)

  val generator : ?order:order -> 'a t -> 'a Gen.t
  (** [generator order map] creates a generator that traverses [map]. See [Gen]
      for generator API. *)

  val fold : (Interval.t -> 'a list -> 'b -> 'b) -> 'a t -> 'b -> 'b
  (** [fold fn map acc] folds over [map] applying function [fn] in ascending
      order of the intervals. [fn] takes the interval, values, and the
      accumulator as parameters and returns the updated accumulator. Tail
      recursive. *)

  val mapi : (Interval.t -> 'a list -> 'b list) -> 'a t -> 'b t
  (** [mapi fn map] builds a new interval map by applying [fn] to the elements
      in [map]. Elements are not traversed in order. Tail recursive. *)

  val map : ('a list -> 'b list) -> 'a t -> 'b t
  (** [map fn map] is like [mapi] but [fn] does not receive the interval. Tail
      recursive. *)

  val iteri : (Interval.t -> 'a list -> unit) -> 'a t -> unit
  (** [iteri fn map] applies [fn] to every element of the map in ascending order
      of the intervals. [fn] received both the interval and associated values
      and returns [unit]. Tail recursive. *)

  val iter : ('a list -> unit) -> 'a t -> unit
  (** [iter fn map] is like [iteri] but [fn] does not receive the interval. Tail
      recursive. *)

  val to_list : 'a t -> (Interval.t * 'a list) list
  (** [to_list map] converts [map] into a [list] where the elements of the
      resulting list are in ascending order by interval. Tail recursive. *)

  val to_seq : 'a t -> (Interval.t * 'a list) Seq.t
  (** [to_seq map] converts [map] into a [Seq.t] where the elements of the
      resulting seq are in ascending order by interval. Seqs are lazy so
      elements of the map are not traversed until the resulting seq is
      traversed. *)

  val find_opt : Interval.t -> 'a t -> 'a list option
  (** [find_opt interval map] finds all values associated with [interval] in
      [map], or [None] if [map] does not contain [interval]. Tail recursive. *)

  val find : Interval.t -> 'a t -> 'a list
  (** [find interval map] finds all values associated with [interval] in [map],
      or raises [Not_found] if [map] does not contain [interval]. Tail
      recursive. *)

  val mem : Interval.t -> 'a t -> bool
  (** [mem interval map] returns [true] if [map] contains [interval], and
      [false] otherwise. Tail recursive. *)

  val query_interval : ?order:order -> Interval.t -> 'a t -> 'a Gen.t
  (** [query_interval interval map] finds all values associated with [interval]
      in the map. Results are provided as a generator, which traverses the map
      as results are read. *)

  val query_interval_list : Interval.t -> 'a t -> (Interval.t * 'a list) list
  (** [query_interval interval map] finds all values associated with [interval]
      in the map and returns the results as a list. Tail recursive. *)
end
