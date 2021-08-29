(** Basic statistics on graphs (experimental) *)

(** [Dist] handles distances between vertices. The distance between
    two vertices is the length of a shortest path between those vertices. *)
module Dist : sig
  type t = Inf | Fin of int

  (** The distance between two vertices is [zero] iff they are equal. *)
  val zero : t

  (** The distance between two vertices is [one] iff they are adjacent. *)
  val one : t

  (** The distance between two vertices is [infty] iff they are disconnected. *)
  val infty : t

  (** Adding distances. *)
  val ( + ) : t -> t -> t

  (** Comparing distances. *)
  val ( > ) : t -> t -> bool

  (** Computing the [max] of two distances. *)
  val max : t -> t -> t
end

module type Graph_statistics = sig
  (** [t] is the type of (undirected) graphs. *)
  type t

  (** [vertex] is the type of vertices. *)
  type vertex

  (** Undirected edges. The [equal] and [hash] function are invariant under permutation
      of the vertices in the pair encoding the edge. *)
  module Undirected_edge : Basic_intf.Std with type t = vertex * vertex

  module Table : Hashtbl.S with type key = Undirected_edge.t

  (** Finite bijections between vertices and integers. *)
  module Vertex_bij : Finbij.S with type elt = vertex

  (** [adjacency_matrix g] computes the adjacency matrix of [g] as well as a
      bijection between the matrix dimensions and the vertices. The matrix is
      dense. Since the graph is undirected, it is also symmetric. *)
  val adjacency_matrix : t -> (int * int) Linalg.Mat.Float.t * Vertex_bij.t

  (** [laplacian g] computes the laplacian of the adjacency matrix,
      following the definition in 'Spectral Graph Theory', by Fan Chung Graham.
      The dimensions are the same as the adjacency matrix.
      A finite bijection is returned as well. *)
  val laplacian : t -> (int * int) Linalg.Mat.Float.t * Vertex_bij.t

  type distance_table = (vertex * vertex, Dist.t) Hashtbl.t

  (** Floyd-warshall algorithm. Complexity is O(V^3) where V is the number of
      vertices of the graph. Returns a table of all distances between pairs of
      vertices. *)
  val floyd_warshall : t -> Dist.t Table.t

  (** Computes the diameter of the graph, ie the maximum over all pair of vertices
      of the shortest-path distance between those vertices. *)
  val diameter : t -> Dist.t

  (** [volume g] computes the sum over all vertices of their degree. *)
  val volume : t -> int

  (** [degree_dist g] computes the degree distribution of [g]. *)
  val degree_dist : t -> (int, float) Stats_intf.fin_prb
end

(** Graph statistics generic on an undirected [Graph] implementation. *)
module Make (Graph : Stats_intf.Graph) :
  Graph_statistics with type t = Graph.t and type vertex = Graph.vertex
