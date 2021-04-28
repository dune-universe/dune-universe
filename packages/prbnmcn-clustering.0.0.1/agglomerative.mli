(** Agglomerative clustering functor. *)

module type Element_set = sig
  (** [t] is the type of (multi)-sets of elements. *)
  type t

  (** [elt] is the type of elements to be clustered. *)
  type elt

  (** [singleton x] is the cluster containing [x] as only element. *)
  val singleton : elt -> t

  (** The user should provide [dist], aa distance function on clusters.
      A typical (costly) choice is the Hausdorff distance (see e.g. the [gromov] package).
      Other, non-metric choices are sometimes used.
  *)
  val dist : t -> t -> float

  (** One should be able to "join" clusters. This can be multiset union, set union
      or any sensible overapproximation - the algorithm will work anyway (think
      for instance of convex hulls in R^n) *)
  val join : t -> t -> t
end

(** [Make] takes as first argument a module [E : Element] of elements
    admitting the structure of a metric space. The second argument of
    [Make] is a module endowing sets of elements with the structure of
    a metric space. *)
module Make : functor
  (E : Intf.Metric)
  (S : Element_set with type elt = E.t)
  -> sig
  type cluster = { set : S.t; tree : tree; uid : int }

  and tree = Node of cluster * cluster | Leaf

  val cluster : E.t list -> cluster

  (** [truncate c depth] returns all the sub-clusters at depth [depth].
      The depth of the root is 0. *)
  val truncate : cluster -> int -> S.t list

  (** Returns all clusters along their depths. *)
  val all_clusters : cluster -> (S.t * int) list
end
