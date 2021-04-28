(** K-medoids functor. *)

(** Initial choice of medoids.
    This implementation provides several initialization algorithms,
    the standard one being Kmedoids++, identical to Kmeans++ *)
type init =
  | Forgy
      (** [Forgy] selects k elements at random (without replacement) as initial centroids. *)
  | KmedoidsPP
      (** [KmedoidsPP] selects initial medoids iteratively with probabilities proportional
        to their distance to the previously selected centroids. This intuitively
        allows to spread them well. *)

(** Algorithm used to perform partitioning. *)
type algorithm =
  | PAM
      (** [PAM] stands for Partition Around Medoids - the classical greedy algorithm. Costly. *)
  | VoronoiIteration
      (** Another heuristic, proceeding similarly to Lloyd's algorithm for Kmeans. Less costly
        (but still more than Kmeans) but perhaps less precise. *)

(** See [K_means]. *)
type termination = Num_iter of int | Threshold of float | Min of constraints

and constraints = { max_iter : int; threshold : float }

(** Exception thrown by [k_medoids] in case something goes awry.*)
exception KmedoidsError of string

module Make : functor (E : Intf.Metric) -> sig
  (* In contrast with [k_means], k_medoids never has to produce new elements,
     and the distance function is only evaluated on the given set of elements.
     It might be worthwhile to precompute the distance on those elements,
     especially if the distance function is costly and if one wants to compute a clustering for several [k]
     or try different algorithms with the same dataset. This is done by partially evaluating [k_medoids] and
     setting precompute to true, as in:

     let cluster_function = k_medoids ~precompute:true ~elements in
     let res1 = cluster_function ~k:10 ~init:KmedoidsPP ~algorithm:PAM ~threshold:0.1 in
     let res2 = cluster_function ~k:15 ~init:KmedoidsPP ~algorithm:VoronoiIteration ~threshold:0.1 in
     ...
  *)

  (** [k_means] performs the clustering using the provided initialization method. *)
  val k_medoids :
    precompute:bool ->
    elements:E.t array ->
    k:int ->
    init:init ->
    algorithm:algorithm ->
    termination:termination ->
    Random.State.t ->
    E.t array array

  (** [cost] returns the sum over all classes of the sum of distances from
      the medoid of the class to all elements of the class. *)
  val cost : classes:E.t array array -> float
end
