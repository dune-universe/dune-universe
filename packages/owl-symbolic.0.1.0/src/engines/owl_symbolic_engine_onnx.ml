(*
 * OWL - OCaml Scientific and Engineering Computing
 * Copyright (c) 2016-2020 Liang Wang <liang.wang@cl.cam.ac.uk>
 *)

open Owl_symbolic_specs
open Owl_symbolic_types
module S = Owl_symbolic_symbol

type t = Onnx_types.model_proto

(* TODO: refactoring-- simplify the building of attributions *)

(** Mapping functions *)

let map_elt_type_to_int32 typ =
  match typ with
  | SNT_Noop      -> Int32.of_int 0
  | SNT_Float     -> Int32.of_int 1
  | SNT_Uint8     -> Int32.of_int 2
  | SNT_Int8      -> Int32.of_int 3
  | SNT_Uint16    -> Int32.of_int 4
  | SNT_Int16     -> Int32.of_int 5
  | SNT_Int32     -> Int32.of_int 6
  | SNT_Int64     -> Int32.of_int 7
  | SNT_String    -> Int32.of_int 8
  | SNT_Bool      -> Int32.of_int 9
  | SNT_Float16   -> Int32.of_int 10
  | SNT_Double    -> Int32.of_int 11
  | SNT_Uint32    -> Int32.of_int 12
  | SNT_Uint64    -> Int32.of_int 13
  | SNT_Complex32 -> Int32.of_int 14
  | SNT_Complex64 -> Int32.of_int 15
  | SNT_SEQ _     -> failwith "map_elt_type_to_int32: type is sequence"


let map_sym_optyp_to_onnx sym_optyp =
  match sym_optyp with
  | "Int"     -> "Constant"
  | "Float"   -> "Constant"
  | "Complex" -> "Constant"
  | "Tensor"  -> "Constant"
  | _         -> sym_optyp


(** Wrapper for building onnx-proto's *)

let make_onnx_tensor_floats ?(shape = [||]) fs =
  let float_data = Array.to_list fs in
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 SNT_Float) in
  PT.default_tensor_proto ~dims ~float_data ~data_type ()


let make_onnx_tensor_ints ?(shape = [||]) i =
  let int32_data = Array.map Int32.of_int i |> Array.to_list in
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 SNT_Int32) in
  PT.default_tensor_proto ~dims ~int32_data ~data_type ()


let make_onnx_tensor_int64s ?(shape = [||]) i =
  let int64_data = Array.map Int64.of_int i |> Array.to_list in
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 SNT_Int64) in
  PT.default_tensor_proto ~dims ~int64_data ~data_type ()


let make_onnx_tensor_complex c =
  let r, i = c in
  let float_data = [ r; i ] in
  let dims = [] in
  let data_type = Some (map_elt_type_to_int32 SNT_Complex32) in
  PT.default_tensor_proto ~dims ~float_data ~data_type ()


let _make_onnx_initializers_raw name data_type shape raw_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 data_type) in
  let name = Some name in
  let raw_data = Some raw_data in
  PT.default_tensor_proto ~dims ~data_type ~name ~raw_data ()


let make_onnx_initializers_float name data_type shape float_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 data_type) in
  let name = Some name in
  let float_data = Array.to_list float_data in
  PT.default_tensor_proto ~dims ~data_type ~name ~float_data ()


let make_onnx_initializers_int32 name data_type shape int_data =
  let dims = Array.map Int64.of_int shape |> Array.to_list in
  let data_type = Some (map_elt_type_to_int32 data_type) in
  let name = Some name in
  let int32_data = Array.map Int32.of_int int_data |> Array.to_list in
  PT.default_tensor_proto ~dims ~data_type ~name ~int32_data ()


let make_onnx_io name elt_type shape =
  let dim =
    Array.map
      (fun d ->
        let value = PT.Dim_value (Int64.of_int d) in
        PT.default_tensor_shape_proto_dimension ~value ())
      shape
    |> Array.to_list
  in
  let shape = Some (PT.default_tensor_shape_proto ~dim ()) in
  let type_proto_tensor =
    PT.default_type_proto_tensor ~shape ~elem_type:(Some elt_type) ()
  in
  let value = PT.Tensor_type type_proto_tensor in
  let type_ = Some (PT.default_type_proto ~value ()) in
  PT.default_value_info_proto ~name:(Some name) ~type_ ()


let make_onnx_node op_type input_names output_names name attr =
  let input_names = Array.to_list input_names in
  let output_names = Array.to_list output_names in
  PT.default_node_proto
    ~input:input_names
    ~output:output_names
    ~name
    ~op_type
    ~attribute:attr
    ()


let make_onnx_graph
    ?(name = "owl_sym_graph")
    (nodes : Onnx_types.node_proto array)
    (initializer_ : Onnx_types.tensor_proto array)
    (inputs : Onnx_types.value_info_proto array)
    (outputs : Onnx_types.value_info_proto array)
  =
  let node = Array.to_list nodes in
  let input = Array.to_list inputs in
  let output = Array.to_list outputs in
  let initializer_ = Array.to_list initializer_ in
  let sparse_initializer = [] in
  let value_info = [] in
  let quantization_annotation = [] in
  PT.default_graph_proto
    ~name:(Some name)
    ~node
    ~initializer_
    ~input
    ~output
    ~sparse_initializer
    ~value_info
    ~quantization_annotation
    ()


let make_onnx_model graph =
  let ir_version = Some (Int64.of_int 6) in
  let producer_name = Some "owl" in
  let producer_version = Some "0.6.0" in
  let domain = Some "xyz.ocaml" in
  let model_version = Some (Int64.of_int 0) in
  let doc_string = Some "owl-symbolic" in
  let graph = Some graph in
  let opset = PT.default_operator_set_id_proto ~version:(Some (Int64.of_int 11)) () in
  let opset_import = [ opset ] in
  let metadata_props = [] in
  PT.default_model_proto
    ~ir_version
    ~producer_name
    ~producer_version
    ~domain
    ~model_version
    ~doc_string
    ~opset_import
    ~metadata_props
    ~graph
    ()


(** Functions to build part of onnx graph *)

let check_same types name =
  let flag = ref true in
  if Array.length types = 0
  then failwith "build_onnx_type_check: empty parents for non-input ops";
  Array.iter (fun t -> if t <> types.(0) then flag := false) types;
  if !flag = false
  then (
    let typs =
      Owl_utils_array.to_string
        (fun x -> map_elt_type_to_int32 x.(0) |> Int32.to_string)
        types
    in
    let msg = Printf.sprintf "%s: inputs are of differnt type: %s." name typs in
    raise (TYPE_CHECK msg))


let check_constraint t constraints name =
  if Array.mem t constraints = false
  then (
    let msg = Printf.sprintf "%s: input type not in constraints." name in
    raise (TYPE_CHECK msg))


(* This step performs type checking of the symbolic graph to see if it fits the ONNX operator schemas. 
 * Some things to note:
 *   + Both input and output types of each operator are array of number_type.
 *   + Type checking does not consider if the output is optional or not
 *   + The inferred output type of an operator is always unique. 
 *   + Do not change the structure of symgraph itself. 
 *   + This function just perform type checking, and thus returns nothing. 
 *   + There are 9 ONNX ops Sequence* that involves "seq(tensor)" types instead of tensor. Ignore them for now.
 *   + In the main body, I still check based on Symbolic node, not onnx nodes, this could be logically wrong.
 *)

let _types_constraint00 = [| SNT_Float; SNT_Float16; SNT_Double |]

let _types_constraint01 =
  [| SNT_Int8; SNT_Int16; SNT_Int32; SNT_Int64; SNT_Float16; SNT_Float; SNT_Double |]


let _types_constraint02 =
  [| SNT_Uint32; SNT_Uint64; SNT_Int32; SNT_Int64; SNT_Float16; SNT_Float; SNT_Double |]


let _types_constraint03 =
  [| SNT_Uint8
   ; SNT_Uint16
   ; SNT_Uint32
   ; SNT_Uint64
   ; SNT_Int8
   ; SNT_Int16
   ; SNT_Int32
   ; SNT_Int64
   ; SNT_Float16
   ; SNT_Float
   ; SNT_Double
   ; SNT_String
   ; SNT_Bool
   ; SNT_Complex32
   ; SNT_Complex64
  |]


let _types_constraint04 =
  [| SNT_Uint8
   ; SNT_Uint16
   ; SNT_Uint32
   ; SNT_Uint64
   ; SNT_Int8
   ; SNT_Int16
   ; SNT_Int32
   ; SNT_Int64
   ; SNT_Float16
   ; SNT_Float
   ; SNT_Double
  |]


let _types_constraint05 =
  [| SNT_Uint8
   ; SNT_Uint16
   ; SNT_Uint32
   ; SNT_Uint64
   ; SNT_Int8
   ; SNT_Int16
   ; SNT_Int32
   ; SNT_Int64
   ; SNT_Float16
   ; SNT_Float
   ; SNT_Double
   ; SNT_String
   ; SNT_Bool
  |]


let _types_constraint06 = [| SNT_Uint8; SNT_Uint16; SNT_Uint32; SNT_Uint64 |]

let type_check_pattern00 sym = [| Owl_symbolic_symbol.dtype sym |]

let type_check_pattern01 target_type type_constraint name =
  check_constraint target_type.(0) type_constraint name;
  [| target_type.(0) |]


let type_check_pattern02 target_types type_constraint name =
  check_same target_types name;
  check_constraint target_types.(0).(0) type_constraint name;
  [| target_types.(0).(0) |]


let type_check_pattern03 target_types type_constraint0 type_constraint1 name =
  check_constraint target_types.(0).(0) type_constraint0 name;
  check_constraint target_types.(1).(0) type_constraint1 name;
  [| target_types.(0).(0) |]


let build_onnx_type_check (sym_graph : Owl_symbolic_graph.t) =
  let len = Owl_symbolic_graph.length sym_graph in
  let dtypes = Hashtbl.create len in
  Owl_symbolic_graph.topo_iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      (* Get input types *)
      let parents = Owl_graph.parents sym_node in
      let ptypes =
        Array.map
          (fun sym_node ->
            let s = Owl_graph.attr sym_node in
            match s with
            | Owl_symbolic_symbol.Float _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Int _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Tensor _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Complex _ -> [| Owl_symbolic_symbol.dtype s |]
            | Owl_symbolic_symbol.Variable _ -> [| Owl_symbolic_symbol.dtype s |]
            | _ -> Hashtbl.find dtypes (S.name s))
          parents
      in
      (* Type checking *)
      let out_type =
        match sym with
        | Float _              -> type_check_pattern00 sym
        | Int _                -> type_check_pattern00 sym
        | Pi _                 -> type_check_pattern00 sym
        | Tensor _             -> type_check_pattern00 sym
        | Complex _            -> type_check_pattern00 sym
        | Variable _           -> type_check_pattern00 sym
        | RandomUniform _      ->
          let dt = Owl_symbolic_symbol.dtype sym in
          type_check_pattern01 [| dt |] _types_constraint00 name
        | RandomNormal _       ->
          let dt = Owl_symbolic_symbol.dtype sym in
          type_check_pattern01 [| dt |] _types_constraint00 name
        | Sin _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Cos _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Tan _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Asin _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Acos _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Atan _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Sinh _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Cosh _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Tanh _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Asinh _              -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Acosh _              -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Atanh _              -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Sqrt _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Exp _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Log _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Erf _                -> type_check_pattern01 ptypes.(0) _types_constraint04 name
        | Sigmoid _            -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Neg _                -> type_check_pattern01 ptypes.(0) _types_constraint01 name
        | Sign _               -> type_check_pattern01 ptypes.(0) _types_constraint04 name
        | Floor _              -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Ceil _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Round _              -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Clip _               ->
          (* TODO: we set the min/max tensor to be float; 
           * change dtypes to be consistent with that of input data
           * This step could be finished somewhere else, since it really doesn't fit. s *)
          Owl_symbolic_symbol.update_tensor_dtype
            (Owl_graph.attr parents.(1))
            ptypes.(0).(0);
          Owl_symbolic_symbol.update_tensor_dtype
            (Owl_graph.attr parents.(2))
            ptypes.(0).(0);
          type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Relu _               -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Elu _                -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | LeakyRelu _          -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Softmax _            -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Softsign _           -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Softplus _           -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Add _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Sub _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Mul _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Div _                -> type_check_pattern02 ptypes _types_constraint02 name
        | Pow _                -> type_check_pattern02 ptypes _types_constraint00 name
        | Mod _                -> type_check_pattern02 ptypes _types_constraint04 name
        | MatMul _             -> type_check_pattern02 ptypes _types_constraint02 name
        | Gemm _               -> type_check_pattern02 ptypes _types_constraint02 name
        | Max _                -> type_check_pattern02 ptypes _types_constraint00 name
        | Min _                -> type_check_pattern02 ptypes _types_constraint00 name
        | Sum _                -> type_check_pattern02 ptypes _types_constraint00 name
        | Mean _               -> type_check_pattern02 ptypes _types_constraint00 name
        | And _                -> type_check_pattern02 ptypes [| SNT_Bool |] name
        | Or _                 -> type_check_pattern02 ptypes [| SNT_Bool |] name
        | Not _                -> type_check_pattern02 ptypes [| SNT_Bool |] name
        | Xor _                -> type_check_pattern02 ptypes [| SNT_Bool |] name
        | Greater _            ->
          type_check_pattern02 ptypes _types_constraint04 name |> ignore;
          [| SNT_Bool |]
        | Less _               ->
          type_check_pattern02 ptypes _types_constraint04 name |> ignore;
          [| SNT_Bool |]
        | Equal _              ->
          type_check_pattern02 ptypes _types_constraint04 name |> ignore;
          [| SNT_Bool |]
        | BitShift _           -> type_check_pattern02 ptypes _types_constraint06 name
        | ReduceSum _          -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceMax _          -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceMin _          -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceMean _         -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceSumSquare _    -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceProd _         -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceLogSum _       -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceLogSumExp _    -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceL1 _           -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | ReduceL2 _           -> type_check_pattern01 ptypes.(0) _types_constraint02 name
        | Reshape _            ->
          type_check_pattern03 ptypes _types_constraint03 [| SNT_Int64 |] name
        | Identity s           ->
          let idx = s.idx in
          [| ptypes.(0).(idx) |]
        | Split s              ->
          let n = Array.length s.split in
          let t = type_check_pattern01 ptypes.(0) _types_constraint03 name in
          Array.make n t.(0)
        | Concat _             -> type_check_pattern02 ptypes _types_constraint03 name
        | Pad _                ->
          let t = type_check_pattern01 ptypes.(0) _types_constraint04 name in
          type_check_pattern01 ptypes.(1) [| SNT_Int64 |] name |> ignore;
          if Array.length ptypes = 3
          then type_check_pattern01 ptypes.(2) _types_constraint04 name |> ignore;
          t
        | Cast x               ->
          type_check_pattern01 ptypes.(0) _types_constraint05 name |> ignore;
          let t = x.target in
          type_check_pattern01 [| t |] _types_constraint05 name
        | Squeeze _            -> type_check_pattern01 ptypes.(0) _types_constraint03 name
        | Tile _               ->
          type_check_pattern01 ptypes.(1) [| SNT_Int64 |] name |> ignore;
          type_check_pattern01 ptypes.(0) _types_constraint03 name
        | Shape _              ->
          type_check_pattern01 ptypes.(0) _types_constraint03 name |> ignore;
          [| SNT_Int64 |]
        | Size _               ->
          type_check_pattern01 ptypes.(0) _types_constraint03 name |> ignore;
          [| SNT_Int64 |]
        | Transpose _          -> type_check_pattern01 ptypes.(0) _types_constraint03 name
        | Slice _              ->
          type_check_pattern02
            (Array.sub ptypes 1 (Array.length ptypes - 1))
            [| SNT_Int32; SNT_Int64 |]
            name
          |> ignore;
          type_check_pattern01 ptypes.(0) _types_constraint03 name
        | SpaceToDepth _       -> type_check_pattern01 ptypes.(0) _types_constraint03 name
        | IsNaN _              ->
          type_check_pattern01 ptypes.(0) _types_constraint00 name |> ignore;
          [| SNT_Bool |]
        | NonZero _            ->
          type_check_pattern01 ptypes.(0) _types_constraint00 name |> ignore;
          [| SNT_Int64 |]
        | Where _              ->
          type_check_pattern01 ptypes.(0) [| SNT_Bool |] name |> ignore;
          type_check_pattern02 [| ptypes.(1); ptypes.(2) |] _types_constraint03 name
        | ScatterElements _    ->
          type_check_pattern01 ptypes.(1) [| SNT_Int32; SNT_Int64 |] name |> ignore;
          type_check_pattern02 [| ptypes.(0); ptypes.(2) |] _types_constraint03 name
        | ScatterND _          ->
          type_check_pattern01 ptypes.(1) [| SNT_Int32; SNT_Int64 |] name |> ignore;
          type_check_pattern02 [| ptypes.(0); ptypes.(2) |] _types_constraint03 name
        | GatherElements _     ->
          type_check_pattern01 ptypes.(1) [| SNT_Int32; SNT_Int64 |] name |> ignore;
          type_check_pattern01 ptypes.(0) _types_constraint03 name
        | GatherND _           ->
          type_check_pattern01 ptypes.(1) [| SNT_Int64 |] name |> ignore;
          type_check_pattern01 ptypes.(0) _types_constraint03 name
        | Conv _               -> type_check_pattern02 ptypes _types_constraint00 name
        | ConvTranspose _      -> type_check_pattern02 ptypes _types_constraint00 name
        | MaxPool _            ->
          let t1 = type_check_pattern01 ptypes.(0) _types_constraint00 name in
          let t2 = SNT_Int64 in
          [| t1.(0); t2 |]
        | AveragePool _        -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | BatchNormalization _ ->
          let t = type_check_pattern02 ptypes _types_constraint00 name in
          Array.make 5 t.(0)
        | InstanceNorm _       -> type_check_pattern02 ptypes _types_constraint00 name
        | Dropout _            ->
          let t = type_check_pattern01 ptypes.(0) _types_constraint00 name in
          [| t.(0); SNT_Bool |]
        | GlobalAveragePool _  -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | GlobalMaxPool _      -> type_check_pattern01 ptypes.(0) _types_constraint00 name
        | Flatten _            -> type_check_pattern01 ptypes.(0) _types_constraint03 name
        | LSTM _               ->
          (* TODO: the optional sequence_len has int32 type *)
          let t = type_check_pattern02 ptypes _types_constraint00 name in
          [| t.(0); t.(0); t.(0) |]
        | RoiAlign _           ->
          assert (Array.length ptypes = 3);
          type_check_pattern01 ptypes.(2) [| SNT_Int64 |] name |> ignore;
          type_check_pattern02 [| ptypes.(0); ptypes.(1) |] _types_constraint00 name
        | SequenceEmpty s      -> [| SNT_SEQ s.dtype |]
        | _                    -> [| SNT_Noop |]
      in
      Hashtbl.add dtypes name out_type)
    sym_graph;
  dtypes


(* Build ONNX attributions for each node 
 * Attributes scheme: https://github.com/onnx/onnx/blob/master/docs/Operators.md
 *)

let build_onnx_attrs_float sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.float_value sym in
  let tensor = Some (make_onnx_tensor_floats [| v |]) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_int sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.int_value sym in
  let tensor = Some (make_onnx_tensor_ints [| v |]) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_complex sym =
  let name = Some "value" in
  (* create "value" attribute for Constant *)
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.complex_value sym in
  let tensor = Some (make_onnx_tensor_complex v) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_pi _sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = Owl_const.pi in
  let tensor = Some (make_onnx_tensor_floats [| v |]) in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


let build_onnx_attrs_tensor sym =
  (* create "value" attribute for Constant *)
  let name = Some "value" in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Tensor in
  let v = S.tensor_value sym in
  let tensor =
    match v.dtype with
    | SNT_Float ->
      let flts =
        match v.flt_val with
        | Some f -> f
        | None   -> [||]
      in
      Some (make_onnx_tensor_floats ~shape:v.shape flts)
    | SNT_Int32 ->
      let ints =
        match v.int_val with
        | Some i -> i
        | None   -> [||]
      in
      Some (make_onnx_tensor_ints ~shape:v.shape ints)
    | SNT_Int64 ->
      let ints =
        match v.int_val with
        | Some i -> i
        | None   -> [||]
      in
      Some (make_onnx_tensor_int64s ~shape:v.shape ints)
    | _         ->
      let t = Owl_symbolic_types.number_type_to_string v.dtype in
      let err_msg = Printf.sprintf "build_onnx_attrs: unsupported type: %s\n" t in
      failwith err_msg
  in
  let a_value = PT.default_attribute_proto ~name ~type_ ~t:tensor () in
  [ a_value ]


(** Build attributes for ONNX nodes *)

let make_attr_int name i =
  let name = Some name in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Int in
  let i = Some (Int64.of_int i) in
  PT.default_attribute_proto ~name ~type_ ~i ()


let make_attr_ints name ints =
  let name_shape = Some name in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Ints in
  let ints = Array.map Int64.of_int ints |> Array.to_list in
  PT.default_attribute_proto ~name:name_shape ~type_ ~ints ()


let make_attr_flt name f =
  let name = Some name in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Float in
  PT.default_attribute_proto ~name ~type_ ~f ()


let make_attr_flts name fs =
  let name = Some name in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Float in
  let floats = Array.to_list fs in
  PT.default_attribute_proto ~name ~type_ ~floats ()


let make_attr_string name s =
  let name = Some name in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.String in
  let s = Some (Bytes.of_string s) in
  PT.default_attribute_proto ~name ~type_ ~s ()


let make_attr_strings name ss =
  let name = Some name in
  let (type_ : PT.attribute_proto_attribute_type option) = Some PT.Strings in
  let strings = Array.map Bytes.of_string ss |> Array.to_list in
  PT.default_attribute_proto ~name ~type_ ~strings ()


let build_onnx_attrs_randomuniform (x : Owl_symbolic_ops_generator.RandomUniform.t) =
  let attr_dtype =
    make_attr_int "dtype" (x.dtype |> map_elt_type_to_int32 |> Int32.to_int)
  in
  let attr_high = make_attr_flt "high" (Some x.high) in
  let attr_low = make_attr_flt "low" (Some x.low) in
  let attr_seed = make_attr_flt "seed" x.seed in
  let attr_shape = make_attr_ints "shape" x.shape in
  [ attr_dtype; attr_high; attr_low; attr_seed; attr_shape ]


let build_onnx_attrs_randomnormal (x : Owl_symbolic_ops_generator.RandomNormal.t) =
  let attr_dtype =
    make_attr_int "dtype" (x.dtype |> map_elt_type_to_int32 |> Int32.to_int)
  in
  let attr_mean = make_attr_flt "mean" (Some x.mean) in
  let attr_scale = make_attr_flt "scale" (Some x.stddev) in
  let attr_seed = make_attr_flt "seed" x.seed in
  let attr_shape = make_attr_ints "shape" x.shape in
  [ attr_dtype; attr_mean; attr_scale; attr_seed; attr_shape ]


let build_onnx_attrs_softmax axis =
  let attr_axis = make_attr_int "axis" axis in
  [ attr_axis ]


let build_onnx_attrs_fmod (x : Owl_symbolic_ops_math.Mod.t) =
  let attr_fmod = make_attr_int "fmod" x.fmod in
  [ attr_fmod ]


let build_onnx_elu alpha =
  let attr_alpha = make_attr_flt "alpha" (Some alpha) in
  [ attr_alpha ]


let build_onnx_attrs_gemm (x : Owl_symbolic_ops_math.Gemm.t) =
  let attr_alpha = make_attr_flt "alpha" (Some x.alpha) in
  let attr_beta = make_attr_flt "beta" (Some x.beta) in
  let i = if x.transA then 1 else 0 in
  let attr_transA = make_attr_int "transA" i in
  let i = if x.transB then 1 else 0 in
  let attr_transB = make_attr_int "transB" i in
  [ attr_alpha; attr_beta; attr_transA; attr_transB ]


let build_onnx_attrs_bitshift direction =
  let attr_dir = make_attr_string "direction" direction in
  [ attr_dir ]


let build_onnx_attrs_reduce axes keepdims =
  let attr_axes = make_attr_ints "axes" axes in
  let i = if keepdims = true then 1 else 0 in
  let attr_keepdims = make_attr_int "keepdims" i in
  [ attr_axes; attr_keepdims ]


let build_onnx_attrs_split (x : Owl_symbolic_ops_tensor.Split.t) =
  let attr_axis = make_attr_int "axis" x.axis in
  let attr_split = make_attr_ints "split" x.split in
  [ attr_axis; attr_split ]


let build_onnx_attrs_concat (x : Owl_symbolic_ops_tensor.Concat.t) =
  let attr_axis = make_attr_int "axis" x.axis in
  [ attr_axis ]


let build_onnx_attrs_pad (x : Owl_symbolic_ops_tensor.Pad.t) =
  let attr_mode = make_attr_string "mode" x.mode in
  [ attr_mode ]


let build_onnx_attrs_cast (x : Owl_symbolic_ops_tensor.Cast.t) =
  let i = x.target |> map_elt_type_to_int32 |> Int32.to_int in
  let attr_to = make_attr_int "to" i in
  [ attr_to ]


let build_onnx_attrs_squeeze (x : Owl_symbolic_ops_tensor.Squeeze.t) =
  match x.axes with
  | Some axes ->
    let attr_axes = make_attr_ints "axes" axes in
    [ attr_axes ]
  | None      -> []


let build_onnx_attrs_transpose (x : Owl_symbolic_ops_tensor.Transpose.t) =
  match x.perm with
  | Some p ->
    let attr_perm = make_attr_ints "perm" p in
    [ attr_perm ]
  | None   -> []


let build_onnx_attrs_spacetodepth blocksize =
  let attr_block = make_attr_int "blocksize" blocksize in
  [ attr_block ]


let build_onnx_attrs_scatter_elements axis =
  let attr_axis = make_attr_int "axis" axis in
  [ attr_axis ]


let build_onnx_attrs_conv (x : Owl_symbolic_ops_nn.Conv.t) =
  let attr_pad =
    match x.pads with
    | Some pad -> make_attr_ints "pads" pad
    | None     -> make_attr_string "auto_pad" x.auto_pad
  in
  let attr_dil = make_attr_ints "dilations" x.dilations in
  let attr_strides = make_attr_ints "strides" x.strides in
  let attr_group = make_attr_int "group" x.group in
  [ attr_pad; attr_dil; attr_group; attr_strides ]


let build_onnx_attrs_conv_transpose (x : Owl_symbolic_ops_nn.ConvTranspose.t) =
  let attr_pad =
    match x.pads with
    | Some pad -> make_attr_ints "pads" pad
    | None     -> make_attr_string "auto_pad" x.auto_pad
  in
  let attr_dil = make_attr_ints "dilations" x.dilations in
  let attr_strides = make_attr_ints "strides" x.strides in
  let attr_group = make_attr_int "group" x.group in
  (* TODO: kernel_shape, output_padding, output_shape *)
  [ attr_pad; attr_dil; attr_group; attr_strides ]


let build_onnx_attrs_maxpool (x : Owl_symbolic_ops_nn.MaxPool.t) =
  let attr_pad =
    match x.pads with
    | Some pad -> make_attr_ints "pads" pad
    | None     -> make_attr_string "auto_pad" x.auto_pad
  in
  let attr_ceil = make_attr_int "ceil_mode" x.ceil_mode in
  let attr_dil = make_attr_ints "dilations" x.dilations in
  let attr_kernel = make_attr_ints "kernel_shape" x.kernel_shp in
  let attr_strides = make_attr_ints "strides" x.strides in
  let attr_order = make_attr_int "storage_order" x.storage_order in
  [ attr_pad; attr_ceil; attr_dil; attr_kernel; attr_strides; attr_order ]


let build_onnx_attrs_avgpool (x : Owl_symbolic_ops_nn.AveragePool.t) =
  let attr_pad =
    match x.pads with
    | Some pad -> make_attr_ints "pads" pad
    | None     -> make_attr_string "auto_pad" x.auto_pad
  in
  let i = if x.ceil_mode then 1 else 0 in
  let attr_ceil = make_attr_int "ceil_mode" i in
  let i = if x.count_include_pad then 1 else 0 in
  let attr_count = make_attr_int "count_include_pad" i in
  let attr_kernel = make_attr_ints "kernel_shape" x.kernel_shp in
  let attr_strides = make_attr_ints "strides" x.strides in
  [ attr_pad; attr_ceil; attr_count; attr_kernel; attr_strides ]


let build_onnx_attrs_batch_norm eps momentum =
  let attr_eps = make_attr_flt "epsilon" (Some eps) in
  let attr_m = make_attr_flt "momentum" (Some momentum) in
  [ attr_eps; attr_m ]


let build_onnx_attrs_instance_norm eps =
  let attr_eps = make_attr_flt "epsilon" (Some eps) in
  [ attr_eps ]


let build_onnx_attrs_dropout (x : Owl_symbolic_ops_nn.Dropout.t) =
  let attr_ratio = make_attr_flt "ratio" (Some x.ratio) in
  [ attr_ratio ]


let build_onnx_attrs_flatten axis =
  let attr_axis = make_attr_int "axis" axis in
  [ attr_axis ]


let build_onnx_attrs_lstm (x : Owl_symbolic_ops_rnn.LSTM.t) =
  let attr_hidden = make_attr_int "hidden_size" x.hidden_size in
  let strings = Array.map Owl_symbolic_types.activation_to_string x.activations in
  let attr_activation = make_attr_strings "activations" strings in
  let attr_direction = make_attr_string "direction" x.direction in
  let attr_forget = make_attr_int "input_forget" x.input_forget in
  let attrs = [ attr_hidden; attr_activation; attr_direction; attr_forget ] in
  let attrs =
    match x.activation_alpha with
    | Some a ->
      let attr_alpha = make_attr_flts "activation_alpha" a in
      List.append attrs [ attr_alpha ]
    | None   -> attrs
  in
  let attrs =
    match x.activation_beta with
    | Some a ->
      let attr_beta = make_attr_flts "activation_beta" a in
      List.append attrs [ attr_beta ]
    | None   -> attrs
  in
  let attrs =
    match x.clip with
    | Some c ->
      let attr_clip = make_attr_flt "clip" (Some c) in
      List.append attrs [ attr_clip ]
    | None   -> attrs
  in
  attrs


let build_onnx_attrs_roi_align (x : Owl_symbolic_ops_object_detection.RoiAlign.t) =
  let s =
    match x.mode with
    | `avg -> "avg"
    | `max -> "max"
  in
  let attr_mode = make_attr_string "mode" s in
  let attr_height = make_attr_int "output_height" x.output_height in
  let attr_width = make_attr_int "output_width" x.output_width in
  let attr_ratio = make_attr_int "sampling_ratio" x.sampling_ratio in
  let attr_scale = make_attr_flt "spatial_scale" (Some x.spatial_scale) in
  [ attr_mode; attr_height; attr_width; attr_ratio; attr_scale ]


let build_onnx_attrs_seq_empty (x : Owl_symbolic_ops_sequence.SequenceEmpty.t) =
  let i = map_elt_type_to_int32 x.dtype |> Int32.to_int in
  let attr_dtype = make_attr_int "dtype" i in
  [ attr_dtype ]


let build_onnx_attrs sym =
  let onnx_attrs =
    match sym with
    | S.Float _              -> build_onnx_attrs_float sym
    | S.Int _                -> build_onnx_attrs_int sym
    | S.Complex _            -> build_onnx_attrs_complex sym
    | S.Pi _                 -> build_onnx_attrs_pi sym
    | S.Tensor _             -> build_onnx_attrs_tensor sym
    | S.RandomUniform x      -> build_onnx_attrs_randomuniform x
    | S.RandomNormal x       -> build_onnx_attrs_randomnormal x
    | S.Softmax x            -> build_onnx_attrs_softmax x.axis
    | S.Mod x                -> build_onnx_attrs_fmod x
    | S.LeakyRelu x          -> build_onnx_elu x.alpha
    | S.Elu x                -> build_onnx_elu x.alpha
    | S.Gemm x               -> build_onnx_attrs_gemm x
    | S.BitShift x           -> build_onnx_attrs_bitshift x.direction
    | S.ReduceSum x          -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceMax x          -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceMin x          -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceMean x         -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceSumSquare x    -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceProd x         -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceLogSum x       -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceLogSumExp x    -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceL1 x           -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.ReduceL2 x           -> build_onnx_attrs_reduce x.axes x.keepdims
    | S.Split x              -> build_onnx_attrs_split x
    | S.Concat x             -> build_onnx_attrs_concat x
    | S.Pad x                -> build_onnx_attrs_pad x
    | S.Cast x               -> build_onnx_attrs_cast x
    | S.Squeeze x            -> build_onnx_attrs_squeeze x
    | S.Transpose x          -> build_onnx_attrs_transpose x
    | S.SpaceToDepth x       -> build_onnx_attrs_spacetodepth x.blocksize
    | S.ScatterElements x    -> build_onnx_attrs_scatter_elements x.axis
    | S.Conv x               -> build_onnx_attrs_conv x
    | S.ConvTranspose x      -> build_onnx_attrs_conv_transpose x
    | S.MaxPool x            -> build_onnx_attrs_maxpool x
    | S.AveragePool x        -> build_onnx_attrs_avgpool x
    | S.BatchNormalization x -> build_onnx_attrs_batch_norm x.epsilon x.momentum
    | S.InstanceNorm x       -> build_onnx_attrs_instance_norm x.eps
    | S.Flatten x            -> build_onnx_attrs_flatten x.axis
    | S.SequenceEmpty x      -> build_onnx_attrs_seq_empty x
    | S.Dropout x            -> build_onnx_attrs_dropout x
    | S.LSTM x               -> build_onnx_attrs_lstm x
    | S.RoiAlign x           -> build_onnx_attrs_roi_align x
    | _                      -> []
  in
  onnx_attrs


(** Core function. Converts symbolic nodes to onnx nodes. *)
let build_onnx_nodes (sym_graph : Owl_symbolic_graph.t) =
  let nodes = ref [||] in
  Owl_symbolic_graph.topo_iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let op_type = S.op_type sym in
      (* Input variable does not belong to graph *)
      if not (Owl_symbolic_graph.is_variable op_type)
      then (
        let name = S.name sym in
        let input_names = S.input sym in
        let output_names = S.output sym in
        let op_type = Some (map_sym_optyp_to_onnx op_type) in
        (* Build onnx attributes  *)
        let onnx_attrs = build_onnx_attrs sym in
        let name = Some name in
        let n = make_onnx_node op_type input_names output_names name onnx_attrs in
        nodes := Array.append !nodes [| n |]))
    sym_graph;
  !nodes


let build_onnx_inputs sym_graph _type_dict =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      let elt_type = S.dtype sym |> map_elt_type_to_int32 in
      let shape = S.shape sym in
      make_onnx_io name elt_type shape)
    (Owl_symbolic_graph.get_input_nodes sym_graph)


let build_onnx_outputs sym_graph type_dict =
  Array.map
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      let elt_type = (Hashtbl.find type_dict name).(0) |> map_elt_type_to_int32 in
      let shape = S.out_shape sym in
      let shape =
        match shape.(0) with
        | Some s -> s
        | None   -> failwith "build_onnx_outputs: non-specified output shape."
      in
      make_onnx_io name elt_type shape)
    (Owl_symbolic_graph.get_output_nodes sym_graph)


let build_onnx_initializers sym_graph =
  let inits = ref [||] in
  Array.iter
    (fun sym_node ->
      let sym = Owl_graph.attr sym_node in
      let name = S.name sym in
      let (init : tensor option) = S.initializer_ sym in
      match init with
      | Some init ->
        let dtype = init.dtype in
        let shape = init.shape in
        let init_tensor =
          match dtype with
          | SNT_Float ->
            let flt_val = init.flt_val in
            let flt_val =
              match flt_val with
              | Some f -> f
              | None   -> [||]
            in
            make_onnx_initializers_float name dtype shape flt_val
          | SNT_Int32 ->
            let int_val = init.int_val in
            let int_val =
              match int_val with
              | Some i -> i
              | None   -> [||]
            in
            make_onnx_initializers_int32 name dtype shape int_val
          | _         -> failwith "build_onnx_initializers: unsupported type"
        in
        inits := Array.append [| init_tensor |] !inits
      | None      -> ())
    (Owl_symbolic_graph.get_input_nodes sym_graph);
  !inits


(** Main entry of conversion to ONNX graph *)
let of_symbolic (sym_graph : Owl_symbolic_graph.t) =
  (* Step 0: walk through the sym_graph and check shapes *)
  let type_dict = build_onnx_type_check sym_graph in
  (* Step 1: convert symbolic nodes to  *)
  let nodes = build_onnx_nodes sym_graph in
  (* Steps 1.x : more processing such as rewriting complex nodes *)

  (* Step 2: inpput/output  *)
  let inputs = build_onnx_inputs sym_graph type_dict in
  let outputs = build_onnx_outputs sym_graph type_dict in
  (* Step 3: initializers, corresponding to each input *)
  let initializer_ = build_onnx_initializers sym_graph in
  (* Step N: Maybe some post-processing steps *)

  (* Final Step: make graph and model *)
  let graph = make_onnx_graph nodes initializer_ inputs outputs in
  make_onnx_model graph


(** Main entry of conversion from ONNX graph (dummy) *)
let to_symbolic (_onnx_graph : Onnx_types.model_proto) = Owl_symbolic_graph.null_graph

(** save an onnx model to Protobuf file *)
let save (onnx_model : Onnx_types.model_proto) filename =
  let encoder = Pbrt.Encoder.create () in
  PB.encode_model_proto onnx_model encoder;
  let oc = open_out filename in
  output_bytes oc (Pbrt.Encoder.to_bytes encoder);
  close_out oc


(** load an onnx model from Protobuf file *)
let load _filename = Obj.magic None

let compile x filename = Obj.magic (x, filename)
