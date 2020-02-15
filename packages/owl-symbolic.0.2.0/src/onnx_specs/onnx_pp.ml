[@@@ocaml.warning "-27-30-39"]

let rec pp_version fmt (v : Onnx_types.version) =
  match v with
  | Onnx_types.Start_version -> Format.fprintf fmt "Start_version"
  | Onnx_types.Ir_version_2017_10_10 -> Format.fprintf fmt "Ir_version_2017_10_10"
  | Onnx_types.Ir_version_2017_10_30 -> Format.fprintf fmt "Ir_version_2017_10_30"
  | Onnx_types.Ir_version_2017_11_3 -> Format.fprintf fmt "Ir_version_2017_11_3"
  | Onnx_types.Ir_version_2019_1_22 -> Format.fprintf fmt "Ir_version_2019_1_22"
  | Onnx_types.Ir_version_2019_3_18 -> Format.fprintf fmt "Ir_version_2019_3_18"
  | Onnx_types.Ir_version -> Format.fprintf fmt "Ir_version"


let rec pp_attribute_proto_attribute_type
    fmt
    (v : Onnx_types.attribute_proto_attribute_type)
  =
  match v with
  | Onnx_types.Undefined      -> Format.fprintf fmt "Undefined"
  | Onnx_types.Float          -> Format.fprintf fmt "Float"
  | Onnx_types.Int            -> Format.fprintf fmt "Int"
  | Onnx_types.String         -> Format.fprintf fmt "String"
  | Onnx_types.Tensor         -> Format.fprintf fmt "Tensor"
  | Onnx_types.Graph          -> Format.fprintf fmt "Graph"
  | Onnx_types.Sparse_tensor  -> Format.fprintf fmt "Sparse_tensor"
  | Onnx_types.Floats         -> Format.fprintf fmt "Floats"
  | Onnx_types.Ints           -> Format.fprintf fmt "Ints"
  | Onnx_types.Strings        -> Format.fprintf fmt "Strings"
  | Onnx_types.Tensors        -> Format.fprintf fmt "Tensors"
  | Onnx_types.Graphs         -> Format.fprintf fmt "Graphs"
  | Onnx_types.Sparse_tensors -> Format.fprintf fmt "Sparse_tensors"


let rec pp_tensor_proto_segment fmt (v : Onnx_types.tensor_proto_segment) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "begin_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.begin_;
    Pbrt.Pp.pp_record_field
      "end_"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.end_;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_string_string_entry_proto fmt (v : Onnx_types.string_string_entry_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "key"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.key;
    Pbrt.Pp.pp_record_field
      "value"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.value;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_tensor_proto_data_location fmt (v : Onnx_types.tensor_proto_data_location) =
  match v with
  | Onnx_types.Default  -> Format.fprintf fmt "Default"
  | Onnx_types.External -> Format.fprintf fmt "External"


let rec pp_tensor_proto fmt (v : Onnx_types.tensor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "dims"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.dims;
    Pbrt.Pp.pp_record_field
      "data_type"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int32)
      fmt
      v.Onnx_types.data_type;
    Pbrt.Pp.pp_record_field
      "segment"
      (Pbrt.Pp.pp_option pp_tensor_proto_segment)
      fmt
      v.Onnx_types.segment;
    Pbrt.Pp.pp_record_field
      "float_data"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_float)
      fmt
      v.Onnx_types.float_data;
    Pbrt.Pp.pp_record_field
      "int32_data"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int32)
      fmt
      v.Onnx_types.int32_data;
    Pbrt.Pp.pp_record_field
      "string_data"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_bytes)
      fmt
      v.Onnx_types.string_data;
    Pbrt.Pp.pp_record_field
      "int64_data"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.int64_data;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.name;
    Pbrt.Pp.pp_record_field
      "doc_string"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.doc_string;
    Pbrt.Pp.pp_record_field
      "raw_data"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_bytes)
      fmt
      v.Onnx_types.raw_data;
    Pbrt.Pp.pp_record_field
      "external_data"
      (Pbrt.Pp.pp_list pp_string_string_entry_proto)
      fmt
      v.Onnx_types.external_data;
    Pbrt.Pp.pp_record_field
      "data_location"
      (Pbrt.Pp.pp_option pp_tensor_proto_data_location)
      fmt
      v.Onnx_types.data_location;
    Pbrt.Pp.pp_record_field
      "double_data"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_float)
      fmt
      v.Onnx_types.double_data;
    Pbrt.Pp.pp_record_field
      "uint64_data"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.uint64_data;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_sparse_tensor_proto fmt (v : Onnx_types.sparse_tensor_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "values"
      (Pbrt.Pp.pp_option pp_tensor_proto)
      fmt
      v.Onnx_types.values;
    Pbrt.Pp.pp_record_field
      "indices"
      (Pbrt.Pp.pp_option pp_tensor_proto)
      fmt
      v.Onnx_types.indices;
    Pbrt.Pp.pp_record_field
      "dims"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.dims;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_tensor_shape_proto_dimension_value
    fmt
    (v : Onnx_types.tensor_shape_proto_dimension_value)
  =
  match v with
  | Onnx_types.Dim_value x -> Format.fprintf fmt "@[Dim_value(%a)@]" Pbrt.Pp.pp_int64 x
  | Onnx_types.Dim_param x -> Format.fprintf fmt "@[Dim_param(%a)@]" Pbrt.Pp.pp_string x


and pp_tensor_shape_proto_dimension fmt (v : Onnx_types.tensor_shape_proto_dimension) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "value"
      pp_tensor_shape_proto_dimension_value
      fmt
      v.Onnx_types.value;
    Pbrt.Pp.pp_record_field
      "denotation"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.denotation;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_tensor_shape_proto fmt (v : Onnx_types.tensor_shape_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "dim"
      (Pbrt.Pp.pp_list pp_tensor_shape_proto_dimension)
      fmt
      v.Onnx_types.dim;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_type_proto_tensor fmt (v : Onnx_types.type_proto_tensor) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "elem_type"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int32)
      fmt
      v.Onnx_types.elem_type;
    Pbrt.Pp.pp_record_field
      "shape"
      (Pbrt.Pp.pp_option pp_tensor_shape_proto)
      fmt
      v.Onnx_types.shape;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_type_proto_value fmt (v : Onnx_types.type_proto_value) =
  match v with
  | Onnx_types.Tensor_type x   ->
    Format.fprintf fmt "@[Tensor_type(%a)@]" pp_type_proto_tensor x
  | Onnx_types.Sequence_type x ->
    Format.fprintf fmt "@[Sequence_type(%a)@]" pp_type_proto_sequence x
  | Onnx_types.Map_type x      -> Format.fprintf fmt "@[Map_type(%a)@]" pp_type_proto_map x


and pp_type_proto fmt (v : Onnx_types.type_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "value" pp_type_proto_value fmt v.Onnx_types.value;
    Pbrt.Pp.pp_record_field
      "denotation"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.denotation;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


and pp_type_proto_sequence fmt (v : Onnx_types.type_proto_sequence) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "elem_type"
      (Pbrt.Pp.pp_option pp_type_proto)
      fmt
      v.Onnx_types.elem_type;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


and pp_type_proto_map fmt (v : Onnx_types.type_proto_map) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "key_type"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int32)
      fmt
      v.Onnx_types.key_type;
    Pbrt.Pp.pp_record_field
      "value_type"
      (Pbrt.Pp.pp_option pp_type_proto)
      fmt
      v.Onnx_types.value_type;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_value_info_proto fmt (v : Onnx_types.value_info_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.name;
    Pbrt.Pp.pp_record_field
      "type_"
      (Pbrt.Pp.pp_option pp_type_proto)
      fmt
      v.Onnx_types.type_;
    Pbrt.Pp.pp_record_field
      "doc_string"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.doc_string;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_tensor_annotation fmt (v : Onnx_types.tensor_annotation) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "tensor_name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.tensor_name;
    Pbrt.Pp.pp_record_field
      "quant_parameter_tensor_names"
      (Pbrt.Pp.pp_list pp_string_string_entry_proto)
      fmt
      v.Onnx_types.quant_parameter_tensor_names;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_attribute_proto fmt (v : Onnx_types.attribute_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.name;
    Pbrt.Pp.pp_record_field
      "ref_attr_name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.ref_attr_name;
    Pbrt.Pp.pp_record_field
      "doc_string"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.doc_string;
    Pbrt.Pp.pp_record_field
      "type_"
      (Pbrt.Pp.pp_option pp_attribute_proto_attribute_type)
      fmt
      v.Onnx_types.type_;
    Pbrt.Pp.pp_record_field "f" (Pbrt.Pp.pp_option Pbrt.Pp.pp_float) fmt v.Onnx_types.f;
    Pbrt.Pp.pp_record_field "i" (Pbrt.Pp.pp_option Pbrt.Pp.pp_int64) fmt v.Onnx_types.i;
    Pbrt.Pp.pp_record_field "s" (Pbrt.Pp.pp_option Pbrt.Pp.pp_bytes) fmt v.Onnx_types.s;
    Pbrt.Pp.pp_record_field "t" (Pbrt.Pp.pp_option pp_tensor_proto) fmt v.Onnx_types.t;
    Pbrt.Pp.pp_record_field "g" (Pbrt.Pp.pp_option pp_graph_proto) fmt v.Onnx_types.g;
    Pbrt.Pp.pp_record_field
      "sparse_tensor"
      (Pbrt.Pp.pp_option pp_sparse_tensor_proto)
      fmt
      v.Onnx_types.sparse_tensor;
    Pbrt.Pp.pp_record_field
      "floats"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_float)
      fmt
      v.Onnx_types.floats;
    Pbrt.Pp.pp_record_field
      "ints"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.ints;
    Pbrt.Pp.pp_record_field
      "strings"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_bytes)
      fmt
      v.Onnx_types.strings;
    Pbrt.Pp.pp_record_field
      "tensors"
      (Pbrt.Pp.pp_list pp_tensor_proto)
      fmt
      v.Onnx_types.tensors;
    Pbrt.Pp.pp_record_field
      "graphs"
      (Pbrt.Pp.pp_list pp_graph_proto)
      fmt
      v.Onnx_types.graphs;
    Pbrt.Pp.pp_record_field
      "sparse_tensors"
      (Pbrt.Pp.pp_list pp_sparse_tensor_proto)
      fmt
      v.Onnx_types.sparse_tensors;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


and pp_graph_proto fmt (v : Onnx_types.graph_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "node" (Pbrt.Pp.pp_list pp_node_proto) fmt v.Onnx_types.node;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.name;
    Pbrt.Pp.pp_record_field
      "initializer_"
      (Pbrt.Pp.pp_list pp_tensor_proto)
      fmt
      v.Onnx_types.initializer_;
    Pbrt.Pp.pp_record_field
      "sparse_initializer"
      (Pbrt.Pp.pp_list pp_sparse_tensor_proto)
      fmt
      v.Onnx_types.sparse_initializer;
    Pbrt.Pp.pp_record_field
      "doc_string"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.doc_string;
    Pbrt.Pp.pp_record_field
      "input"
      (Pbrt.Pp.pp_list pp_value_info_proto)
      fmt
      v.Onnx_types.input;
    Pbrt.Pp.pp_record_field
      "output"
      (Pbrt.Pp.pp_list pp_value_info_proto)
      fmt
      v.Onnx_types.output;
    Pbrt.Pp.pp_record_field
      "value_info"
      (Pbrt.Pp.pp_list pp_value_info_proto)
      fmt
      v.Onnx_types.value_info;
    Pbrt.Pp.pp_record_field
      "quantization_annotation"
      (Pbrt.Pp.pp_list pp_tensor_annotation)
      fmt
      v.Onnx_types.quantization_annotation;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


and pp_node_proto fmt (v : Onnx_types.node_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "input"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.input;
    Pbrt.Pp.pp_record_field
      "output"
      (Pbrt.Pp.pp_list Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.output;
    Pbrt.Pp.pp_record_field
      "name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.name;
    Pbrt.Pp.pp_record_field
      "op_type"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.op_type;
    Pbrt.Pp.pp_record_field
      "domain"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.domain;
    Pbrt.Pp.pp_record_field
      "attribute"
      (Pbrt.Pp.pp_list pp_attribute_proto)
      fmt
      v.Onnx_types.attribute;
    Pbrt.Pp.pp_record_field
      "doc_string"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.doc_string;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_operator_set_id_proto fmt (v : Onnx_types.operator_set_id_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "domain"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.domain;
    Pbrt.Pp.pp_record_field
      "version"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.version;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_model_proto fmt (v : Onnx_types.model_proto) =
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field
      "ir_version"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.ir_version;
    Pbrt.Pp.pp_record_field
      "opset_import"
      (Pbrt.Pp.pp_list pp_operator_set_id_proto)
      fmt
      v.Onnx_types.opset_import;
    Pbrt.Pp.pp_record_field
      "producer_name"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.producer_name;
    Pbrt.Pp.pp_record_field
      "producer_version"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.producer_version;
    Pbrt.Pp.pp_record_field
      "domain"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.domain;
    Pbrt.Pp.pp_record_field
      "model_version"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_int64)
      fmt
      v.Onnx_types.model_version;
    Pbrt.Pp.pp_record_field
      "doc_string"
      (Pbrt.Pp.pp_option Pbrt.Pp.pp_string)
      fmt
      v.Onnx_types.doc_string;
    Pbrt.Pp.pp_record_field
      "graph"
      (Pbrt.Pp.pp_option pp_graph_proto)
      fmt
      v.Onnx_types.graph;
    Pbrt.Pp.pp_record_field
      "metadata_props"
      (Pbrt.Pp.pp_list pp_string_string_entry_proto)
      fmt
      v.Onnx_types.metadata_props;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()


let rec pp_tensor_proto_data_type fmt (v : Onnx_types.tensor_proto_data_type) =
  match v with
  | Onnx_types.Undefined  -> Format.fprintf fmt "Undefined"
  | Onnx_types.Float      -> Format.fprintf fmt "Float"
  | Onnx_types.Uint8      -> Format.fprintf fmt "Uint8"
  | Onnx_types.Int8       -> Format.fprintf fmt "Int8"
  | Onnx_types.Uint16     -> Format.fprintf fmt "Uint16"
  | Onnx_types.Int16      -> Format.fprintf fmt "Int16"
  | Onnx_types.Int32      -> Format.fprintf fmt "Int32"
  | Onnx_types.Int64      -> Format.fprintf fmt "Int64"
  | Onnx_types.String     -> Format.fprintf fmt "String"
  | Onnx_types.Bool       -> Format.fprintf fmt "Bool"
  | Onnx_types.Float16    -> Format.fprintf fmt "Float16"
  | Onnx_types.Double     -> Format.fprintf fmt "Double"
  | Onnx_types.Uint32     -> Format.fprintf fmt "Uint32"
  | Onnx_types.Uint64     -> Format.fprintf fmt "Uint64"
  | Onnx_types.Complex64  -> Format.fprintf fmt "Complex64"
  | Onnx_types.Complex128 -> Format.fprintf fmt "Complex128"
  | Onnx_types.Bfloat16   -> Format.fprintf fmt "Bfloat16"
