(**
   Data structures to represent sets of (possibly annotated) genomic regions

   This module is useful to deal with sets of genomic regions. It
   provides set operations like union, intersection, difference or
   membership tests. Specific data types are also provided when the
   regions are annotated with some value.

   Genomic regions are represented as a pair formed by a range and an
   abstract representation of a sequence/chromosome identifier. The
   data structures implemented here are parameterized over this
   abstract type. To obtain an implementation for the most common case
   where chromosomes are identified with a string, simply apply the
   functor [Make] on the [String] module.

   The functor [Make] provides four datatypes, which corresponds to
   variants where:

   - the regions in the set can overlap or not

   - the regions are annotated with some values
*)

(** A collection of non-overlapping regions (e.g. a set of CpG islands) *)
module Selection : sig
  type t

  val empty : t
  val add : t -> GLoc.t -> t

  val inter : t -> t -> t
  val union : t -> t -> t
  val diff : t -> t -> t
  val size : t -> int

  val intersects : t -> GLoc.t -> bool
  (** [intersects loc sel] returns [true] if [loc] has a non-empty
        intersection with [sel], and [false] otherwise. *)

  val overlap : t -> GLoc.t -> int

  val to_stream : t -> GLoc.t Stream.t

  val of_stream : GLoc.t Stream.t -> t
  (** [of_stream e] computes a selection (i.e. a set of non
      overlapping locations) as the union of the locations contained
      in [e] *)

end

(** A set of locations (e.g. a set of gene loci) *)
module LSet : sig
  type t

  val empty : t

  val to_stream : t -> GLoc.t Stream.t
  val of_stream : GLoc.t Stream.t -> t


  val intersects : t -> GLoc.t -> bool
  (** [intersects lset loc] returns [true] if [loc] has a non-empty
      intersection with one of the locations in [lset], and returns
      [false] otherwise *)

  val closest : t -> GLoc.t -> (GLoc.t * int) option
  (** [closest lset loc] returns the GLoc.t in [lset] that is the
      closest to [loc], along with the actual (minimal)
      distance. Returns [None] if there is no GLoc.t in [lset]
      that comes from the same chromosome than [loc]. *)

  val intersecting_elems : t -> GLoc.t -> GLoc.t Stream.t
  (** [intersecting_elems lset loc] returns a stream of all
      locations in [lset] that intersect [loc]. *)

end

(** A set of locations with an attached value on each of them *)
module LMap : sig
  type 'a t

  val empty : 'a t

  val add : 'a t -> GLoc.t -> 'a -> 'a t

  val to_stream : 'a t -> (GLoc.t * 'a) Stream.t
  val of_stream : (GLoc.t * 'a) Stream.t -> 'a t

  val intersects : 'a t -> GLoc.t -> bool
  (** [intersects lmap loc] returns [true] if [loc] has a non-empty
      intersection with one of the locations in [lmap], and returns
      [false] otherwise *)

  val closest : 'a t -> GLoc.t -> (GLoc.t * 'a * int) option
  (** [closest lmap loc] returns the GLoc.t in [lmap] that is the
      closest to [loc], along with its annotation and the actual (minimal)
      distance. Returns [None] if there is no GLoc.t in [lmap]
      that comes from the same chromosome than [loc]. *)

  val intersecting_elems : 'a t -> GLoc.t -> (GLoc.t * 'a) Stream.t
  (** [intersecting_elems lmap loc] returns a stream of elements
      in [lmap] whose GLoc.t intersects with [loc]. *)
end

module LAssoc : sig
  (** Sorted association list *)
  type 'a t = private (GLoc.t * 'a) list

  val of_alist : (GLoc.t * 'a) list -> 'a t

  val of_list : 'a list -> f:('a -> GLoc.t) -> 'a t

  val to_alist : 'a t -> (GLoc.t * 'a) list

  val filter :
    'a t ->
    f:(GLoc.t -> 'a -> bool) ->
    'a t

  val fold_neighbors :
    'a t -> 'b t ->
    init:(GLoc.t -> 'a -> 'c) ->
    f:(GLoc.t -> 'b -> 'c -> 'c) ->
    'c t

  val matching :
    mode:[`Interval | `Point] ->
    max_dist:int ->
    'a t -> 'b t ->
    [> `Match of (GLoc.t * 'a) * (GLoc.t * 'b)
    |  `Left of (GLoc.t * 'a)
    |  `Right of (GLoc.t * 'b)] list
end
