(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

module Make (G : Owl_computation_engine_sig.Flatten_Sig) : sig
  type t = G.graph

  val to_symbolic : t -> Owl_symbolic_graph.t

  val of_symbolic : Owl_symbolic_graph.t -> t

  val save : 'a -> 'b -> unit

  val load : 'a -> 'b
end

(*
open Owl_types

module Make (A : Ndarray_Mutable) : sig

  module G : Owl_computation_engine_sig.Flatten_Sig

  type t = G.graph

  val to_symbolic : t -> Owl_symbolic_graph.t
  val of_symbolic : Owl_symbolic_graph.t -> t
  val save : 'a -> 'b -> unit
  val load : 'a -> 'b

end

Error in to_symbolic : This expression has type OWL_Engine.t -> SymGraph.t
       but an expression was expected of type G.graph -> 'a
       Type OWL_Engine.t is not compatible with type G.graph 
*)
