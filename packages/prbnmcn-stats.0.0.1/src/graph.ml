(* Statistics on (simple, undirected) graphs. *)

module Dist = struct
  type t = Inf | Fin of int

  let zero = Fin 0

  let one = Fin 1

  let infty = Inf

  let ( + ) d1 d2 =
    match (d1, d2) with
    | (Inf, _) | (_, Inf) -> Inf
    | (Fin i, Fin j) -> Fin (i + j)

  let ( > ) d1 d2 =
    match (d1, d2) with
    | (Inf, Inf) -> false
    | (Inf, _) -> true
    | (_, Inf) -> false
    | (Fin i1, Fin i2) -> i1 > i2

  let max d1 d2 = if d2 > d1 then d2 else d1
end

module type Graph_statistics = sig
  (* [t] is the type of (undirected) graphs. *)
  type t

  type vertex

  (* Undirected edges. The [equal] and [hash] function are invariant under permutation
     of the vertices in the pair encoding the edge. *)
  module Undirected_edge : Basic_intf.Std with type t = vertex * vertex

  module Table : Hashtbl.S with type key = Undirected_edge.t

  module Vertex_bij : Finbij.S with type elt = vertex

  val adjacency_matrix : t -> (int * int) Linalg.Mat.Float.t * Vertex_bij.t

  val laplacian : t -> (int * int) Linalg.Mat.Float.t * Vertex_bij.t

  type distance_table = (vertex * vertex, Dist.t) Hashtbl.t

  val floyd_warshall : t -> Dist.t Table.t

  val diameter : t -> Dist.t

  val volume : t -> int

  val degree_dist : t -> (int, float) Stats_intf.fin_prb
end

module Make (Graph : Stats_intf.Graph) :
  Graph_statistics with type t = Graph.t and type vertex = Graph.vertex = struct
  type t = Graph.t

  type vertex = Graph.vertex

  let canon v1 v2 =
    let c = Graph.V.compare v1 v2 in
    match c with -1 | 0 -> (v1, v2) | 1 -> (v2, v1) | _ -> assert false

  module Undirected_edge :
    Basic_intf.Std with type t = Graph.vertex * Graph.vertex = struct
    type t = Graph.vertex * Graph.vertex

    let equal (v1, v2) (v1', v2') =
      let (v1, v2) = canon v1 v2 in
      let (v1', v2') = canon v1' v2' in
      Graph.V.compare v1 v1' = 0 && Graph.V.compare v2 v2' = 0

    let compare (v1, v2) (v1', v2') =
      let (v1, v2) = canon v1 v2 in
      let c = Graph.V.compare v1 v1' in
      if c <> 0 then c else Graph.V.compare v2 v2'

    let hash (v1, v2) =
      let (v1, v2) = canon v1 v2 in
      Hashtbl.hash (Graph.V.hash v1, Graph.V.hash v2)

    let pp fmtr (v1, v2) =
      Format.fprintf fmtr "(%a, %a)" Graph.V.pp v1 Graph.V.pp v2
  end

  module Table = Hashtbl.Make (Undirected_edge)
  module Vertex_bij = Finbij.Make (Graph.V)

  let adjacency_matrix graph : (int * int) Linalg.Mat.Float.t * Vertex_bij.t =
    let nb_vertex = Graph.nb_vertex graph in
    let vertices = Graph.fold_vertex (fun v l -> v :: l) graph [] in
    let vbij = Vertex_bij.of_list vertices in
    let shape = Linalg.Tensor.Int.(rank_two nb_vertex nb_vertex) in
    let matrix =
      Linalg.Intf.(
        Vec
          ( shape,
            fun (c, r) ->
              let vr = Vertex_bij.nth_exn vbij r in
              let vc = Vertex_bij.nth_exn vbij c in
              if Graph.mem_edge graph vr vc then 1.0 else 0.0 ))
    in
    (matrix, vbij)

  (* Following the definition in 'Spectral Graph Theory', Fan Chung Graham *)
  let laplacian graph : (int * int) Linalg.Mat.Float.t * Vertex_bij.t =
    let nb_vertex = Graph.nb_vertex graph in
    let vertices = Graph.fold_vertex (fun v l -> v :: l) graph [] in
    let vbij = Vertex_bij.of_list vertices in
    let shape = Linalg.Tensor.Int.(rank_two nb_vertex nb_vertex) in
    let matrix =
      Linalg.Intf.(
        Vec
          ( shape,
            fun (j, r) ->
              if r = j then
                let vr = Vertex_bij.nth_exn vbij r in
                let d = Graph.out_degree graph vr in
                if d = 0 then 0.0 else 1.0
              else
                let vr = Vertex_bij.nth_exn vbij r in
                let vj = Vertex_bij.nth_exn vbij j in
                if Graph.mem_edge graph vr vj then
                  let dr = float_of_int (Graph.out_degree graph vr) in
                  let dj = float_of_int (Graph.out_degree graph vj) in
                  ~-.1. /. sqrt (dr *. dj)
                else 0.0 ))
    in
    (matrix, vbij)

  type distance_table = (Graph.vertex * Graph.vertex, Dist.t) Hashtbl.t

  let floyd_warshall graph =
    let nb_vertex = Graph.nb_vertex graph in
    let table = Table.create (nb_vertex * nb_vertex * 2) in
    let find_dist table v1 v2 =
      match Table.find_opt table (canon v1 v2) with
      | None -> Dist.infty
      | Some dist -> dist
    in
    let set_dist table v1 v2 dist = Table.replace table (canon v1 v2) dist in
    Graph.iter_vertex (fun v -> Table.add table (v, v) Dist.zero) graph ;
    Graph.iter_edges (fun v1 v2 -> Table.add table (canon v1 v2) Dist.one) graph ;
    Graph.iter_vertex
      (fun vi ->
        Graph.iter_vertex
          (fun vj ->
            Graph.iter_vertex
              (fun vk ->
                let dij = find_dist table vi vj in
                let dik = find_dist table vi vk in
                let dkj = find_dist table vk vj in
                let len = Dist.(dik + dkj) in
                if Dist.(dij > len) then set_dist table vi vj len else ())
              graph)
          graph)
      graph ;
    table

  let diameter graph =
    Table.fold
      (fun _ dist acc -> Dist.max dist acc)
      (floyd_warshall graph)
      Dist.zero

  let volume graph =
    Graph.fold_vertex (fun v acc -> acc + Graph.out_degree graph v) graph 0

  let incr graph v map =
    let deg = Graph.out_degree graph v in
    Basic_impl.Int_map.update
      deg
      (fun count_opt -> Some (Option.value ~default:0 count_opt + 1))
      map

  let degree_dist graph =
    let degrees =
      Graph.fold_vertex (incr graph) graph Basic_impl.Int_map.empty
    in
    let degrees =
      Basic_impl.Int_map.fold
        (fun deg count acc -> (deg, float_of_int count) :: acc)
        degrees
        []
    in
    Fin.Float.(
      normalize
        (measure (module Basic_impl.Free_module.Float_valued.Int) degrees))
end
