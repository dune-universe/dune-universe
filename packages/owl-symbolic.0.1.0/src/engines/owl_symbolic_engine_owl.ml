(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

(*
open Owl_types

module Make (A : Ndarray_Mutable) = struct

  module G = Owl_computation_cpu_engine.Make (A)
*)

module Make (G : Owl_computation_engine_sig.Flatten_Sig) = struct
  open G.Optimiser.Operator.Symbol.Shape.Type

  type t = G.graph

  let build_symbol_var op name =
    let shape = op.shape in
    let s =
      match shape.(0) with
      | Some s -> s
      | None   -> failwith "unspecified owl shape"
    in
    let s = Owl_symbolic_utils.to_nchw_order s in
    (* HACK *)
    Owl_symbolic_operator.variable ~shape:s ~dtype:SNT_Float name


  let build_symbol_zeros shp name =
    let shp = Owl_symbolic_utils.to_nchw_order shp in
    (* HACK *)
    let ele_num = Owl_symbolic_utils.nelt shp in
    let flt_val = Array.make ele_num 0. in
    let tensor = Owl_symbolic_types.make_tensor ~flt_val shp in
    Owl_symbolic_operator.tensor ~name tensor


  let build_symbol_ones shp name =
    let shp = Owl_symbolic_utils.to_nchw_order shp in
    (* HACK *)
    let ele_num = Owl_symbolic_utils.nelt shp in
    let flt_val = Array.make ele_num 1. in
    let tensor = Owl_symbolic_types.make_tensor ~flt_val shp in
    Owl_symbolic_operator.tensor ~name tensor


  let build_symbol_uniform shp node =
    let name = Owl_graph.name node in
    let shp = Owl_symbolic_utils.hwio_to_oihw_order shp in
    (* HACK *)
    let inodes = Owl_graph.parents node in
    let low =
      G.Optimiser.Operator.Symbol.node_to_elt inodes.(0)
      |> G.Optimiser.Operator.Symbol.elt_to_float
    in
    let high =
      G.Optimiser.Operator.Symbol.node_to_elt inodes.(1)
      |> G.Optimiser.Operator.Symbol.elt_to_float
    in
    Owl_symbolic_operator.random_uniform ~name ~high ~low shp


  let build_symbol_const node =
    let cnode_attr = Owl_graph.attr node in
    let name = Owl_graph.name node in
    let shape =
      match cnode_attr.shape.(0) with
      | Some s -> s
      | None   -> failwith "Const: unspecified owl shape"
    in
    let shape = Owl_symbolic_utils.to_nchw_order shape in
    (* HACK *)
    let flt_val =
      if shape = [||]
      then
        [| G.Optimiser.Operator.Symbol.node_to_elt node
           |> G.Optimiser.Operator.Symbol.elt_to_float
        |]
      else
        (* TODO: G.node_to_arr node |> G.unpack_arr |> A.to_array *)
        failwith "Convert constant Ndarray value is not supported yet."
    in
    (* TODO: change the dtype to float/double accoding to specific Owl ndarray type *)
    let t = Owl_symbolic_types.make_tensor ~flt_val shape in
    Owl_symbolic_operator.tensor ~name t


  let build_symbol_sum' cnode_attr name sym_inputs =
    let shape = cnode_attr.shape in
    let len =
      match shape.(0) with
      | Some s -> Array.length s
      | None   -> failwith "Owl_engine/sum':unspecified owl shape."
    in
    let axes = Owl_utils_array.range 0 (len - 1) in
    Owl_symbolic_operator.reduce_sum ~name ~keepdims:false sym_inputs.(0) axes


  let build_symbol_reshape shp name sym_inputs =
    let shp = Owl_symbolic_utils.to_nchw_order shp in
    (* HACK *)
    Owl_symbolic_operator.reshape ~name shp sym_inputs.(0)


  let build_symbol node symdict =
    let cnode_attr = Owl_graph.attr node in
    let name = Owl_graph.name node in
    let (sym_inputs : Owl_symbolic_graph.symbol array) =
      Array.map
        (fun n ->
          let n = Owl_graph.name n in
          try Hashtbl.find symdict n with
          | Not_found -> failwith "owl_to_symbolic: input node not found.")
        (Owl_graph.parents node)
    in
    match cnode_attr.op with
    (* TODO: unify interfaces *)
    | Var -> build_symbol_var cnode_attr name
    | Zeros shp -> build_symbol_zeros shp name
    | Ones shp -> build_symbol_ones shp name
    | Uniform shp -> build_symbol_uniform shp node
    | Const -> build_symbol_const node
    | Sin -> Owl_symbolic_operator.sin ~name sym_inputs.(0)
    | Cos -> Owl_symbolic_operator.cos ~name sym_inputs.(0)
    | Sqrt -> Owl_symbolic_operator.sqrt ~name sym_inputs.(0)
    | Exp -> Owl_symbolic_operator.exp ~name sym_inputs.(0)
    | Log -> Owl_symbolic_operator.log ~name sym_inputs.(0)
    | Neg -> Owl_symbolic_operator.neg ~name sym_inputs.(0)
    | Scalar_Neg -> Owl_symbolic_operator.neg ~name sym_inputs.(0) (* ? *)
    | Relu -> Owl_symbolic_operator.relu ~name sym_inputs.(0)
    | Add -> Owl_symbolic_operator.add ~name sym_inputs.(0) sym_inputs.(1)
    | AddScalar -> Owl_symbolic_operator.add ~name sym_inputs.(0) sym_inputs.(1)
    | ScalarAdd -> Owl_symbolic_operator.add ~name sym_inputs.(0) sym_inputs.(1)
    | Sub -> Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1)
    | SubScalar -> Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1)
    | ScalarSub -> Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1)
    | Scalar_Sub -> Owl_symbolic_operator.sub ~name sym_inputs.(0) sym_inputs.(1) (* ? *)
    | Mul -> Owl_symbolic_operator.mul ~name sym_inputs.(0) sym_inputs.(1)
    | MulScalar -> Owl_symbolic_operator.mul ~name sym_inputs.(0) sym_inputs.(1)
    | ScalarMul -> Owl_symbolic_operator.mul ~name sym_inputs.(0) sym_inputs.(1)
    | Div -> Owl_symbolic_operator.div ~name sym_inputs.(0) sym_inputs.(1)
    | DivScalar -> Owl_symbolic_operator.div ~name sym_inputs.(0) sym_inputs.(1)
    | ScalarDiv -> Owl_symbolic_operator.div ~name sym_inputs.(0) sym_inputs.(1)
    | Pow -> Owl_symbolic_operator.pow ~name sym_inputs.(0) sym_inputs.(1)
    | PowScalar -> Owl_symbolic_operator.pow ~name sym_inputs.(0) sym_inputs.(1)
    | ScalarPow -> Owl_symbolic_operator.pow ~name sym_inputs.(0) sym_inputs.(1)
    | Dot (_, _, _, _) -> Owl_symbolic_operator.matmul ~name sym_inputs.(0) sym_inputs.(1)
    | SumReduce a -> Owl_symbolic_operator.reduce_sum ~name sym_inputs.(0) a
    | Sum a -> Owl_symbolic_operator.reduce_sum ~name sym_inputs.(0) [| a |]
    | Sum' -> build_symbol_sum' cnode_attr name sym_inputs
    | Max a -> Owl_symbolic_operator.reduce_max ~name sym_inputs.(0) [| a |]
    | Reshape shp -> build_symbol_reshape shp name sym_inputs
    | Conv2d (padding, strides) ->
      let pad =
        if padding = SAME then Owl_symbolic_types.SAME_UPPER else Owl_symbolic_types.VALID
      in
      Owl_symbolic_operator.conv ~name ~padding:pad ~strides sym_inputs.(0) sym_inputs.(1)
    | MaxPool2d (padding, kernel, strides) ->
      let pad =
        if padding = SAME then Owl_symbolic_types.SAME_LOWER else Owl_symbolic_types.VALID
      in
      let y, _ =
        Owl_symbolic_operator.maxpool ~name ~strides ~padding:pad sym_inputs.(0) kernel
      in
      y
    | _ ->
      failwith
        (Printf.sprintf
           "Node type not supported: %s"
           (G.Optimiser.Operator.Symbol.op_to_str cnode_attr.op))


  (** Main entry *)

  let to_symbolic (cgraph : t) =
    let outputs = G.get_outputs cgraph in
    (* Step 1: Preprocess: name each node properly *)
    Owl_graph.iter_ancestors
      ~order:DFS
      ~traversal:PostOrder
      (fun node ->
        let name = Owl_graph.name node in
        let name =
          if name <> ""
          then name
          else (
            let id = Owl_graph.id node in
            Printf.sprintf "owlnode%d" id)
        in
        Owl_graph.set_name node name)
      outputs;
    (* Step 2: iterate Owl CGraph to build symbols *)
    (* A dict of symname:symbol; TODO: change the length *)
    let syms = Hashtbl.create 100 in
    Owl_graph.iter_ancestors
      ~order:DFS
      ~traversal:PostOrder
      (fun node ->
        let name = Owl_graph.name node in
        let sym = build_symbol node syms in
        Hashtbl.add syms name sym)
      outputs;
    (* Step 3: choose only the output symbols to be in the graph *)
    let output_sym_nodes =
      Array.map
        (fun n ->
          let name = Owl_graph.name n in
          Hashtbl.find syms name)
        outputs
    in
    Owl_symbolic_graph.make_graph output_sym_nodes ""


  let of_symbolic (_sym_graph : Owl_symbolic_graph.t) =
    G.make_graph ~input:[||] ~output:[||] "dummy-graph"


  (** save an cgraph model to file *)
  let save _cgraph _filename = ()

  (** load an cgraph model from file *)
  let load _filename = Obj.magic None
end
