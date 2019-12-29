(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_symbol
open Owl_symbolic_operator
open Owl_symbolic_infix

(** Convert symbol_graph into functional-rich cse_graph (or tree actually) *)

(* convert sym_graph to cas_graph *)
let build _sym_graph = ()

(* print expressoin on stdout *)
let pprint _cas_graph = ()

(* Operations *)

(* Assume the nodes are already canonically orderred *)
let extract_mul_coeff node =
  let ap = Owl_graph.attr node in
  match ap with
  | Mul _ ->
    let ps = Owl_graph.parents node in
    (match Owl_graph.attr ps.(0) with
    | Div _    -> ps.(0), ps.(1)
    | Int _    -> ps.(0), ps.(1)
    | NegOne _ -> ps.(0), int ~-1 * ps.(1)
    | _        -> int 1, node)
  | _     -> failwith "extract_mul_coeff: not mul op"
