(** Extraction of strongly connected components of directed graphs,
    required to analyze mutual recursive definitions in parse trees.
*)

module type G = sig
  type t
  type v
  val vertices : t -> v list
  val successors : t -> v -> v list
  val equal : v -> v -> bool
  val compare : v -> v -> int
end

module Make(G : G) : sig
  open G
    
  val sccs : t -> v list list
  (** pure implementation of sccs. random tested with sccs implementation of OCamlGraph *)

  val div_by_components : t -> v list list -> (v list * v list list) list
  (** Get edges of graph divided by components. Each components must be non empty.
      They must be distinct each other. *)

  val toposort : t -> v list option
  (** If a graph contains a cycle, returns [None] *)
end
