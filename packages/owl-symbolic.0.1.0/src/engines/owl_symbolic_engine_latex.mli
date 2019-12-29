(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

type t = string

val of_symbolic : Owl_symbolic_graph.t -> t

val to_symbolic : t -> Owl_symbolic_graph.t

val save : t -> string -> unit

val load : string -> t

val html : ?dot:bool -> exprs:Owl_symbolic_graph.t list -> t -> unit
(** Helper functions *)
