
(** Functorial interface. *)

module type Point =
sig
  (** A point. *)
  type t

  (** [dist] should be a distance function: symmetric, zero and
      the diagonal and verifying the triangular inequality.
      Be _very_ careful with the implementation of your metric
      (dist x x = 0.0, NaN is not a proper distance, etc). *)
  val dist: t -> t -> float
end

module Make: functor (P: Point) ->
sig
  (** A vantage point tree. *)
  type t

  (** Quality of the constructed tree.
      Tree construction takes more time with higher quality.
      Tree query time takes less time with higher tree quality.
      If you have 100k or more points, use a Good or Random tree. *)
  type quality = Optimal
               | Good of int (* sample size *)
               | Random

  (** [create quality points]
      create a vantage point tree of given quality containing all points. *)
  val create: quality -> P.t list -> t

  (** [nearest_neighbor p vpt] return the distance along with the nearest
      neighbor to query point [p] in [vpt]. Warning: there may be several
      points at this distance from [p] in [vpt],
      but a single (arbitrary) one is returned.
      If you are not happy with that, use a point type that is
      deduplicated (i.e. a point that holds the info for all points with
      the same coordinates). *)
  val nearest_neighbor: P.t -> t -> float * P.t

  (** [neighbors p tol vpt] return all points in [vpt] within
      [tol] distance from query point [p].
      I.e. all points returned are within [(d <= tol)]
      distance from [p]. *)
  val neighbors: P.t -> float -> t -> P.t list

  (** [to_list vpt] return the list of points in [vpt]. *)
  val to_list: t -> P.t list

  (** [is_empty vpt] test if [vpt] is empty. *)
  val is_empty: t -> bool

  (** [find query tree] return the first point with distance to [query] = 0.0.
      @raise [Not_found] if no such element exists.
      Warning: there may be several
      points at this distance from [p] in [vpt],
      but a single (arbitrary) one is returned. *)
  val find: P.t -> t -> P.t

  (** [mem query tree] return true if [query] can be found in [tree];
      false otherwise. *)
  val mem: P.t -> t -> bool

  (** [root tree] return the root point of the tree.
      @raise [Not_found] if [tree] is empty. *)
  val root: t -> P.t

  (** [check tree] test the tree invariant.
      Should always be true.
      If invariant doesn't hold, then this library has a bug
      (or your distance function is not a proper metric). *)
  val check: t -> bool

  (** [remove quality query tree] return an updated [tree] where the first
      element with distance = 0.0 to [query] was removed.
      The sub-tree that is reconstructed upon removal of [query] uses
      the specified [quality].
      @raise [Not_found] if [not (mem query tree)]. *)
  val remove: quality -> P.t -> t -> t
end
