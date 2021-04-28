module type Element_set = sig
  type t

  type elt

  val singleton : elt -> t

  val dist : t -> t -> float

  val join : t -> t -> t
end

module Make =
functor
  (E : Intf.Metric)
  (S : Element_set with type elt = E.t)
  ->
  struct
    type cluster = { set : S.t; tree : tree; uid : int }

    and tree = Node of cluster * cluster | Leaf

    let uid =
      let x = ref (-1) in
      fun () ->
        incr x ;
        !x

    let mkcluster set tree = { set; tree; uid = uid () }

    (* Hash distance computation between clusters. *)
    module Table = Hashtbl.Make (struct
      type t = cluster * cluster

      let equal (c1, c2) (c1', c2') =
        (c1.uid = c1'.uid && c2.uid = c2'.uid)
        || (c1.uid = c2'.uid && c2.uid = c1'.uid)

      let hash (c1, c2) =
        if c1.uid < c2.uid then Hashtbl.hash (c1.uid, c2.uid)
        else Hashtbl.hash (c2.uid, c1.uid)
    end)

    let dist sz =
      let table = Table.create sz in
      fun c1 c2 ->
        if c1.uid = c2.uid then 0.0
        else
          match Table.find_opt table (c1, c2) with
          | Some dist -> dist
          | None ->
              let dist = S.dist c1.set c2.set in
              Table.add table (c1, c2) dist ;
              dist

    let minimum_pairwise_distance (dist : cluster -> cluster -> float) clusters
        =
      match clusters with
      | [] -> invalid_arg "empty cluster list"
      | [c] -> (0.0, c, c)
      | c :: c' :: _tl ->
          let (acc, _) =
            List.fold_left
              (fun (acc, i) c ->
                let (acc, _) =
                  List.fold_left
                    (fun (acc, j) c' ->
                      let acc =
                        if j > i then
                          let (best_d, _, _) = acc in
                          let d = dist c c' in
                          if d < best_d then (d, c, c') else acc
                        else acc
                      in
                      (acc, j + 1))
                    (acc, 0)
                    clusters
                in
                (acc, i + 1))
              ((dist c c', c, c'), 0)
              clusters
          in
          acc

    let rec iterate dist clusters =
      match clusters with
      | [] -> invalid_arg "empty cluster list"
      | [c] -> c
      | _ ->
          let (_d, c, c') = minimum_pairwise_distance dist clusters in
          let clusters =
            List.filter (fun c0 -> c0.uid <> c.uid && c0.uid <> c'.uid) clusters
          in
          let joined = mkcluster (S.join c.set c'.set) (Node (c, c')) in
          iterate dist (joined :: clusters)

    let cluster elements =
      let len = List.length elements in
      let dist = dist len in
      let clusters =
        List.map (fun x -> mkcluster (S.singleton x) Leaf) elements
      in
      iterate dist clusters

    let truncate cluster depth =
      let rec truncate { set; tree; _ } depth queue acc =
        match tree with
        | Leaf -> (
            if depth > 0 then invalid_arg "truncate: tree too short"
            else
              let acc = set :: acc in
              match queue with
              | [] -> acc
              | (next, d) :: tl -> truncate next d tl acc)
        | Node (l, r) ->
            if depth = 0 then
              let acc = set :: acc in
              match queue with
              | [] -> acc
              | (next, d) :: tl -> truncate next d tl acc
            else truncate l (depth - 1) ((r, depth - 1) :: queue) acc
      in
      truncate cluster depth [] []

    let all_clusters cluster =
      let rec fold { set; tree; _ } depth acc =
        match tree with
        | Leaf -> (set, depth) :: acc
        | Node (l, r) ->
            fold r (depth + 1) (fold l (depth + 1) ((set, depth) :: acc))
      in
      fold cluster 0 []
  end
