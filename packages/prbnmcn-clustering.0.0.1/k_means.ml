(* We require elements to have the structure of a
   metric space and to support the computing of geometric means. *)
module type Element = sig
  include Intf.Metric

  val mean : t array -> t
end

type init = Forgy | RandomPartition | KmeansPP

(* K-means++ *)

type termination = Num_iter of int | Threshold of float | Min of constraints

and constraints = { max_iter : int; threshold : float }

exception KmeansError of string

module Make (E : Element) = struct
  type elt = E.t

  (* [closest elements centroid elti] returns the pair (m,d) such that
     [centroids.(m)] is closest to [elements.(elti)], and the distance
     is equal to d. *)
  let closest elements centroids elti =
    let m = ref 0 in
    let d = ref max_float in
    for i = 0 to Array.length centroids - 1 do
      let dist = E.dist elements.(elti) centroids.(i) in
      if dist < !d then (
        d := dist ;
        m := i)
    done ;
    (!m, !d)

  (* [compute_classes centroids elements] computes for each centroid
     the set of elements which are closest to it, i.e. it computes the
     voronoi partition of [elements] according to [centroids]. *)
  let compute_classes centroids elements =
    let classes : int list array = Array.make (Array.length centroids) [] in
    Array.iteri
      (fun elti _ ->
        let (k, _) = closest elements centroids elti in
        classes.(k) <- elti :: classes.(k))
      elements ;
    let classes =
      Array.to_list classes
      |> List.filter (function [] -> false | _ -> true)
      |> Array.of_list
    in
    Array.map Array.of_list classes

  (* [compute_centroids], given a partition [classes] of [elements], returns
     the centroid of each class. *)
  let compute_centroids (elements : elt array) (classes : int array array) =
    Array.map
      (fun arr -> E.mean (Array.map (fun i -> elements.(i)) arr))
      classes

  (* Voronoi iteration: partition according to the centroids, then update the centroids,
     etc under the centroids do not move collectively more than [threshold]. *)
  let rec iterate (centroids : E.t array) (elements : E.t array) niter
      (termination : termination) =
    let classes = compute_classes centroids elements in
    let centroids' = compute_centroids elements classes in
    let terminate =
      match termination with
      | Num_iter max_iter -> niter >= max_iter
      | Threshold threshold ->
          let dist =
            Array.mapi (fun i c -> E.dist c centroids'.(i)) centroids'
            |> Array.fold_left ( +. ) 0.0
          in
          dist < threshold
      | Min { max_iter; threshold } ->
          niter >= max_iter
          ||
          let dist =
            Array.mapi (fun i c -> E.dist c centroids'.(i)) centroids'
            |> Array.fold_left ( +. ) 0.0
          in
          dist < threshold
    in
    if terminate then classes
    else iterate centroids elements (niter + 1) termination

  (* [random_partition_init] picks [k] initial centroids by selecting a
     partition in [k] classes at random, and then computing the centroid
     of each class. *)
  let random_partition_init k elements rng_state =
    let classes = Array.make k [] in
    Array.iteri
      (fun elti _ ->
        let i = Random.State.int rng_state k in
        classes.(i) <- elti :: classes.(i))
      elements ;
    let classes = Array.map Array.of_list classes in
    compute_centroids elements classes

  let pick_uniformly arr rng_state =
    let c = Array.length arr in
    if c = 0 then
      raise
        (KmeansError "pick_uniformly: empty array - bug found, please report")
    else arr.(Random.State.int rng_state c)

  (* Given a discrete probability distribution stored in [arr], pick an index according
     to that distribution. *)
  (* Note that the distance to a point to itself is 0, so the probability for a centroid
     to pick itself is also zero. *)
  let pick_proportional arr =
    let total = Helpers.array_fsum arr in
    let r = Random.float total in
    let rec loop i acc =
      if acc <= arr.(i) then i else loop (i + 1) (acc -. arr.(i))
    in
    loop 0 r

  (* [kmeanspp_iter] selects [k] centroids iteratively: the first one is taken uniformly at
     random, and in the inductive step the next one is picked with a probability proportional
     to its squared distance to the closest centroid. *)
  let rec kmeanspp_iter k centroids elements =
    if k = 0 then centroids
    else
      let dists =
        Array.mapi
          (fun elti _ ->
            let (_, d) = closest elements centroids elti in
            d *. d)
          elements
      in
      let i = pick_proportional dists in
      let centroids = Array.concat [centroids; [| elements.(i) |]] in
      kmeanspp_iter (k - 1) centroids elements

  let kmeanspp_init k elements rng_state =
    if k < 1 then raise (KmeansError "kmeanspp_init: k < 1, error")
    else
      let elt = pick_uniformly elements rng_state in
      kmeanspp_iter (k - 1) [| elt |] elements

  let k_means_internal ~k ~(init : init) ~elements ~(termination : termination)
      rng_state =
    let centroids =
      match init with
      | Forgy -> Helpers.forgy_init k elements rng_state
      | RandomPartition -> random_partition_init k elements rng_state
      | KmeansPP -> kmeanspp_init k elements rng_state
    in
    iterate centroids elements 0 termination

  let k_means ~k ~init ~elements ~termination rng_state =
    if Array.length elements = 0 then
      raise (KmeansError "k_means: empty elements array")
    else
      let classes =
        k_means_internal ~k ~init ~elements ~termination rng_state
      in
      Array.map (Array.map (fun i -> elements.(i))) classes

  (* 2 x cluster_radius overapproximates the diameter, hopefully tightly *)
  (* let cluster_radius elements =
   *   let mean = E.mean elements in
   *   Array.fold_left (fun maxdist elt ->
   *       max maxdist (E.dist elt mean)
   *     ) (~-. max_float) elements *)

  (* let sum_of_cluster_radius classes =
   *   Helpers.array_fsum (Array.map cluster_radius classes) *)

  let total_squared_dist_to_mean elements =
    let mean = E.mean elements in
    Array.fold_left
      (fun acc elt ->
        let d = E.dist elt mean in
        acc +. (d *. d))
      0.0
      elements

  let cost ~classes =
    Helpers.array_fsum (Array.map total_squared_dist_to_mean classes)
end
