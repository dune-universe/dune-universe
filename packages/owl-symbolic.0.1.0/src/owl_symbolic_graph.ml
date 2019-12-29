(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_graph
open Owl_symbolic_types
open Owl_symbolic_symbol

type symbol = Owl_symbolic_symbol.t Owl_graph.node

type t =
  { mutable sym_nodes : symbol array
  ; mutable name : string
  ; mutable node_names : string array
  }

(** A series of graph operations. *)

let _debug_shape = false

let make_node (sym : Owl_symbolic_symbol.t) (parents : symbol array) =
  let child = node sym in
  (* update the child's input and output shape *)
  if Array.length parents > 0
  then (
    let in_shapes =
      Array.map
        (fun sym_node -> Owl_graph.attr sym_node |> Owl_symbolic_symbol.out_shape)
        parents
    in
    let (shape : int array option array) = Owl_symbolic_shape.infer_shape in_shapes sym in
    (* TODO: remove this part in product code *)
    if _debug_shape = true
    then (
      let foo =
        match shape.(0) with
        | Some s -> s
        | None   -> [||]
      in
      Owl_log.info
        "%s: %s\n"
        (Owl_symbolic_symbol.name sym)
        (Owl_utils_array.to_string string_of_int foo));
    Owl_symbolic_symbol.set_out_shape sym shape);
  (* Connect child with parents *)
  connect_ancestors parents [| child |];
  let uniq_parents = Owl_utils_array.unique parents in
  Array.iter (fun parent -> connect_descendants [| parent |] [| child |]) uniq_parents;
  child


(* Create a symbolic graph; check duplicated names *)
let make_graph (nodes : symbol array) name =
  let node_names = ref [||] in
  Owl_graph.iter_ancestors
    (fun n ->
      let x = Owl_graph.attr n |> Owl_symbolic_symbol.name in
      node_names := Array.append [| x |] !node_names)
    nodes;
  let node_names = !node_names in
  if Owl_symbolic_utils.check_uniq node_names = false
  then raise (INVALID_NAME "make_graph: the nodes contain duplicated names");
  { sym_nodes = nodes; name; node_names }


(* Topological iteration *)
let topo_iter f (g : t) = iter_ancestors ~order:DFS ~traversal:PostOrder f g.sym_nodes

(* Get all the "variable" nodes in sym_graph *)
let get_input_nodes sym_graph =
  let inputs = ref [||] in
  topo_iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let op_typ = Owl_symbolic_symbol.op_type sym in
      if op_typ = "Variable" then inputs := Array.append !inputs [| sym_node |])
    sym_graph;
  !inputs


(* Assume only one output node in graph; note performance issue *)
let get_output_nodes sym_graph = sym_graph.sym_nodes

(** Utilities *)

let null_graph = make_graph [||] ""

let iter_print (g : t) =
  topo_iter
    (fun sym_node ->
      let a = Owl_graph.attr sym_node in
      Printf.fprintf stderr "%s\n" (Owl_symbolic_symbol.name a))
    g


let is_variable op_type = op_type = "Variable"

let name sym_node =
  let sym = Owl_graph.attr sym_node in
  Owl_symbolic_symbol.name sym


let length (g : t) =
  let cnt = ref 0 in
  topo_iter (fun _ -> cnt := !cnt + 1) g;
  !cnt


(** Print a symbolic tree (not graph) to terminal *)

let shape_or_value x =
  let sym = attr x in
  let shp = shape sym in
  if shp = [||]
  then (
    let v =
      match sym with
      | Int _   -> int_value sym |> string_of_int
      | Float _ -> float_value sym |> string_of_float
      | _       -> "NaN"
    in
    Printf.sprintf "v:%s" v)
  else (
    let shp_str = Owl_utils_array.to_string string_of_int shp in
    Printf.sprintf "s:%s" shp_str)


let refnum x = Owl_graph.outdegree x

let to_dot graph =
  let b = Buffer.create 512 in
  Buffer.add_string b "digraph CG {\nnode [shape=record];\n";
  iter_in_edges
    (fun u v -> Buffer.add_string b (Printf.sprintf "%i -> %i;\n" (id u) (id v)))
    (get_output_nodes graph);
  iter_ancestors
    (fun n ->
      let svs = shape_or_value n in
      Buffer.add_string
        b
        (Printf.sprintf
           "%i [ label=\"{{#%i | { %s | %s }} | r:%i; %s;}\""
           (id n)
           (id n)
           (name n)
           (Owl_symbolic_symbol.op_type (attr n))
           (refnum n)
           svs);
      Buffer.add_string b "];\n")
    (get_output_nodes graph);
  Buffer.add_char b '}';
  Buffer.contents b


let set_sym (n : symbol) (s : Owl_symbolic_symbol.t) = Owl_graph.set_attr n s
