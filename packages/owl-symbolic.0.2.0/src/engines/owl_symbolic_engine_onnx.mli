(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t = Onnx_types.model_proto

val of_symbolic : Owl_symbolic_graph.t -> t

val to_symbolic : t -> Owl_symbolic_graph.t

val save : t -> string -> unit

val load : string -> t

val compile : t -> string -> unit
(** Helper functions *)
