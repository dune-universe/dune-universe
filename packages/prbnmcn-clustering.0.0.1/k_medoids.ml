type init =
  | Forgy (* Selects k elements at random (without replacement) *)
  | KmedoidsPP

(* K-medoids++, cf K-means++ *)

type algorithm =
  | PAM
  (* Partition Around Medoids - the classical greedy algorithm. Costly. *)
  | VoronoiIteration

(* Another heuristic, less costly but perhaps less reliable. *)

type termination = Num_iter of int | Threshold of float | Min of constraints

and constraints = { max_iter : int; threshold : float }

exception KmedoidsError of string

module Make (E : Intf.Metric) = struct
  (* Avoid polymorphic refs for efficiency (perhaps a case of premature optimization but well) *)
  type fref = { mutable c : float }

  let fref x = { c = x }

  (* [closest dist elt medoids] returns the pair (m,d) such that [medoids.(m)] is closest
     to [elt] according to the distance function [dist], and the distance is d. *)
  let closest dist elt medoids =
    let m = ref 0 in
    let d = fref max_float in
    for i = 0 to Array.length medoids - 1 do
      let dist = dist elt medoids.(i) in
      if dist < d.c then (
        d.c <- dist ;
        m := i)
    done ;
    (!m, d.c)

  (* Internal function to compute cost of a choice of medoids. *)
  let cost_ dist elements medoids =
    let acc = ref 0.0 in
    for i = 0 to Array.length elements - 1 do
      let (_, dist_to_closest) = closest dist elements.(i) medoids in
      acc := !acc +. dist_to_closest
    done ;
    !acc

  (* One step of the PAM algorithm. For each medoid m, for each element e,
     evaluate the cost of the configuration where e replaces m as medoid.
     If cost is lower, keep e as medoid, otherwise keep m. *)
  let pam_step dist elements medoids =
    for mi = 0 to Array.length medoids - 1 do
      let current_cost = fref (cost_ dist elements medoids) in
      let m = medoids.(mi) in
      for ei = 0 to Array.length elements - 1 do
        let e = elements.(ei) in
        medoids.(mi) <- e ;
        let new_cost = cost_ dist elements medoids in
        if new_cost >= current_cost.c then medoids.(mi) <- m
        else current_cost.c <- new_cost
      done
    done

  (* [produce_clusters dist elements medoids] computes the voronoi
     partition induced by the choice of [medoids], according to [dist]. *)
  let produce_clusters dist elements medoids =
    let buckets = Array.make (Array.length medoids) [] in
    Array.iter
      (fun elt ->
        let (closest_idx, _) = closest dist elt medoids in
        buckets.(closest_idx) <- elt :: buckets.(closest_idx))
      elements ;
    Array.map Array.of_list buckets

  (* computes most central element of a subset of elements *)
  let compute_medoid_of_class dist cls =
    let centralities =
      Array.map
        (fun elt ->
          let dists = Array.map (dist elt) cls in
          let centrality = Helpers.array_fsum dists in
          (elt, centrality))
        cls
    in
    Array.sort (fun (_, c) (_, c') -> Float.compare c c') centralities ;
    let (elt, cost) = centralities.(0) in
    (elt, cost)

  let compute_medoids dist classes =
    let result = Array.map (compute_medoid_of_class dist) classes in
    let medoids = Array.map fst result in
    let costs = Helpers.array_fsum (Array.map snd result) in
    (medoids, costs)

  (* One step of the voronoi iteration algorithm.*)
  let voronoi_iteration_step dist elements medoids =
    let classes = produce_clusters dist elements medoids in
    for i = 0 to Array.length medoids - 1 do
      let v = fst (compute_medoid_of_class dist classes.(i)) in
      Array.unsafe_set medoids i v
    done

  (* Initialization stuff (see k_means.ml) *)
  let pick_uniformly arr rng_state =
    let c = Array.length arr in
    if c = 0 then
      raise
        (KmedoidsError "pick_uniformly: empty array - bug found, please report")
    else arr.(Random.State.int rng_state c)

  let pick_proportional arr =
    let total = Helpers.array_fsum arr in
    let r = Random.float total in
    let rec loop i acc =
      if acc <= arr.(i) then i else loop (i + 1) (acc -. arr.(i))
    in
    loop 0 r

  let cost ~classes = snd (compute_medoids E.dist classes)

  let rec kmedoidspp_iter dist k medoids elements =
    if k = 0 then medoids
    else
      let dists =
        Array.map
          (fun elt ->
            let (_, d) = closest dist elt medoids in
            d)
          elements
      in
      let i = pick_proportional dists in
      let medoids = Array.concat [medoids; [| elements.(i) |]] in
      kmedoidspp_iter dist (k - 1) medoids elements

  let kmedoidspp_init dist k elements rng_state =
    if k < 1 then raise (KmedoidsError "kmedoidspp_init: k < 1, error")
    else
      let elt = pick_uniformly elements rng_state in
      kmedoidspp_iter dist (k - 1) [| elt |] elements

  (* The [iterate_?] functions are parameterised by a step function [step]. Each will
     destructively update [medoids] and [elements] until termination. *)

  let iterate_n dist elements medoids step n =
    for _ = 1 to n do
      step dist elements medoids
    done

  let iterate_threshold dist elements medoids step threshold =
    let cost = fref (cost_ dist elements medoids) in
    let loop = ref true in
    while !loop do
      step dist elements medoids ;
      let new_cost = cost_ dist elements medoids in
      let delta = cost.c -. new_cost in
      if delta >= 0.0 && delta < threshold then loop := false
      else cost.c <- new_cost
    done

  let iterate_min dist elements medoids step n threshold =
    let cost = fref (cost_ dist elements medoids) in
    let exception Break in
    try
      for _ = 1 to n do
        step dist elements medoids ;
        let new_cost = cost_ dist elements medoids in
        let delta = cost.c -. new_cost in
        if delta >= 0.0 && delta < threshold then raise Break
        else cost.c <- new_cost
      done
    with Break -> ()

  let k_medoids_internal dist elements k (init : init) algorithm
      (termination : termination) rng_state =
    let medoids =
      (* Initialize medoids *)
      match init with
      | KmedoidsPP -> kmedoidspp_init dist k elements rng_state
      | Forgy -> Helpers.forgy_init k elements rng_state
    in
    (* Select algorithm used for iteration step *)
    let step =
      match algorithm with
      | PAM -> pam_step
      | VoronoiIteration -> voronoi_iteration_step
    in
    (match termination with
    | Num_iter n -> iterate_n dist elements medoids step n
    | Threshold threshold ->
        iterate_threshold dist elements medoids step threshold
    | Min { max_iter = n; threshold } ->
        iterate_min dist elements medoids step n threshold) ;
    produce_clusters dist elements medoids

  let k_medoids ~precompute ~elements =
    if precompute then
      let len = Array.length elements in
      let mat =
        Array.init len (fun i ->
            Array.init len (fun j -> E.dist elements.(i) elements.(j)))
      in
      let dist i j = mat.(i).(j) in
      let elements_indices = Array.init len (fun i -> i) in
      fun ~k ~init ~algorithm ~termination rng_state ->
        let clusters =
          k_medoids_internal
            dist
            elements_indices
            k
            init
            algorithm
            termination
            rng_state
        in
        Array.map (Array.map (fun i -> elements.(i))) clusters
    else fun ~k ~init ~algorithm ~termination rng_state ->
      k_medoids_internal E.dist elements k init algorithm termination rng_state
end
