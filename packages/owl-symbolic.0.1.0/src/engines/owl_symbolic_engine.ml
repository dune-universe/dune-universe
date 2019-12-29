(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(*

open Owl_symbolic_types

module type Sig = sig

  type t

  val of_symbolic : symbolic_graph -> t

  val to_symbolic : t -> symbolic_graph

end

module Make (S : Owl_symbolic_engine_sig.Sig) = struct

  module Symbolic = S

  let to_symbolic = S.to_symbolic

  let of_symbolic = S.of_symbolic

end
*)
