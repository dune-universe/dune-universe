[@@@ocaml.warning "-27-30-39"]

type version =
  | Start_version
  | Ir_version_2017_10_10
  | Ir_version_2017_10_30
  | Ir_version_2017_11_3
  | Ir_version_2019_1_22
  | Ir_version_2019_3_18
  | Ir_version

type attribute_proto_attribute_type =
  | Undefined
  | Float
  | Int
  | String
  | Tensor
  | Graph
  | Sparse_tensor
  | Floats
  | Ints
  | Strings
  | Tensors
  | Graphs
  | Sparse_tensors

type tensor_proto_segment =
  { begin_ : int64 option
  ; end_ : int64 option
  }

type string_string_entry_proto =
  { key : string option
  ; value : string option
  }

type tensor_proto_data_location =
  | Default
  | External

type tensor_proto =
  { dims : int64 list
  ; data_type : int32 option
  ; segment : tensor_proto_segment option
  ; float_data : float list
  ; int32_data : int32 list
  ; string_data : bytes list
  ; int64_data : int64 list
  ; name : string option
  ; doc_string : string option
  ; raw_data : bytes option
  ; external_data : string_string_entry_proto list
  ; data_location : tensor_proto_data_location option
  ; double_data : float list
  ; uint64_data : int64 list
  }

type sparse_tensor_proto =
  { values : tensor_proto option
  ; indices : tensor_proto option
  ; dims : int64 list
  }

type tensor_shape_proto_dimension_value =
  | Dim_value of int64
  | Dim_param of string

and tensor_shape_proto_dimension =
  { value : tensor_shape_proto_dimension_value
  ; denotation : string option
  }

type tensor_shape_proto = { dim : tensor_shape_proto_dimension list }

type type_proto_tensor =
  { elem_type : int32 option
  ; shape : tensor_shape_proto option
  }

type type_proto_value =
  | Tensor_type   of type_proto_tensor
  | Sequence_type of type_proto_sequence
  | Map_type      of type_proto_map

and type_proto =
  { value : type_proto_value
  ; denotation : string option
  }

and type_proto_sequence = { elem_type : type_proto option }

and type_proto_map =
  { key_type : int32 option
  ; value_type : type_proto option
  }

type value_info_proto =
  { name : string option
  ; type_ : type_proto option
  ; doc_string : string option
  }

type tensor_annotation =
  { tensor_name : string option
  ; quant_parameter_tensor_names : string_string_entry_proto list
  }

type attribute_proto =
  { name : string option
  ; ref_attr_name : string option
  ; doc_string : string option
  ; type_ : attribute_proto_attribute_type option
  ; f : float option
  ; i : int64 option
  ; s : bytes option
  ; t : tensor_proto option
  ; g : graph_proto option
  ; sparse_tensor : sparse_tensor_proto option
  ; floats : float list
  ; ints : int64 list
  ; strings : bytes list
  ; tensors : tensor_proto list
  ; graphs : graph_proto list
  ; sparse_tensors : sparse_tensor_proto list
  }

and graph_proto =
  { node : node_proto list
  ; name : string option
  ; initializer_ : tensor_proto list
  ; sparse_initializer : sparse_tensor_proto list
  ; doc_string : string option
  ; input : value_info_proto list
  ; output : value_info_proto list
  ; value_info : value_info_proto list
  ; quantization_annotation : tensor_annotation list
  }

and node_proto =
  { input : string list
  ; output : string list
  ; name : string option
  ; op_type : string option
  ; domain : string option
  ; attribute : attribute_proto list
  ; doc_string : string option
  }

type operator_set_id_proto =
  { domain : string option
  ; version : int64 option
  }

type model_proto =
  { ir_version : int64 option
  ; opset_import : operator_set_id_proto list
  ; producer_name : string option
  ; producer_version : string option
  ; domain : string option
  ; model_version : int64 option
  ; doc_string : string option
  ; graph : graph_proto option
  ; metadata_props : string_string_entry_proto list
  }

type tensor_proto_data_type =
  | Undefined
  | Float
  | Uint8
  | Int8
  | Uint16
  | Int16
  | Int32
  | Int64
  | String
  | Bool
  | Float16
  | Double
  | Uint32
  | Uint64
  | Complex64
  | Complex128
  | Bfloat16

let rec default_version () = (Start_version : version)

let rec default_attribute_proto_attribute_type () =
  (Undefined : attribute_proto_attribute_type)


let rec default_tensor_proto_segment
    ?(begin_ : int64 option = None)
    ?(end_ : int64 option = None)
    ()
    : tensor_proto_segment
  =
  { begin_; end_ }


let rec default_string_string_entry_proto
    ?(key : string option = None)
    ?(value : string option = None)
    ()
    : string_string_entry_proto
  =
  { key; value }


let rec default_tensor_proto_data_location () = (Default : tensor_proto_data_location)

let rec default_tensor_proto
    ?(dims : int64 list = [])
    ?(data_type : int32 option = None)
    ?(segment : tensor_proto_segment option = None)
    ?(float_data : float list = [])
    ?(int32_data : int32 list = [])
    ?(string_data : bytes list = [])
    ?(int64_data : int64 list = [])
    ?(name : string option = None)
    ?(doc_string : string option = None)
    ?(raw_data : bytes option = None)
    ?(external_data : string_string_entry_proto list = [])
    ?(data_location : tensor_proto_data_location option = None)
    ?(double_data : float list = [])
    ?(uint64_data : int64 list = [])
    ()
    : tensor_proto
  =
  { dims
  ; data_type
  ; segment
  ; float_data
  ; int32_data
  ; string_data
  ; int64_data
  ; name
  ; doc_string
  ; raw_data
  ; external_data
  ; data_location
  ; double_data
  ; uint64_data
  }


let rec default_sparse_tensor_proto
    ?(values : tensor_proto option = None)
    ?(indices : tensor_proto option = None)
    ?(dims : int64 list = [])
    ()
    : sparse_tensor_proto
  =
  { values; indices; dims }


let rec default_tensor_shape_proto_dimension_value () : tensor_shape_proto_dimension_value
  =
  Dim_value 0L


and default_tensor_shape_proto_dimension
    ?(value : tensor_shape_proto_dimension_value = Dim_value 0L)
    ?(denotation : string option = None)
    ()
    : tensor_shape_proto_dimension
  =
  { value; denotation }


let rec default_tensor_shape_proto ?(dim : tensor_shape_proto_dimension list = []) ()
    : tensor_shape_proto
  =
  { dim }


let rec default_type_proto_tensor
    ?(elem_type : int32 option = None)
    ?(shape : tensor_shape_proto option = None)
    ()
    : type_proto_tensor
  =
  { elem_type; shape }


let rec default_type_proto_value () : type_proto_value =
  Tensor_type (default_type_proto_tensor ())


and default_type_proto
    ?(value : type_proto_value = Tensor_type (default_type_proto_tensor ()))
    ?(denotation : string option = None)
    ()
    : type_proto
  =
  { value; denotation }


and default_type_proto_sequence ?(elem_type : type_proto option = None) ()
    : type_proto_sequence
  =
  { elem_type }


and default_type_proto_map
    ?(key_type : int32 option = None)
    ?(value_type : type_proto option = None)
    ()
    : type_proto_map
  =
  { key_type; value_type }


let rec default_value_info_proto
    ?(name : string option = None)
    ?(type_ : type_proto option = None)
    ?(doc_string : string option = None)
    ()
    : value_info_proto
  =
  { name; type_; doc_string }


let rec default_tensor_annotation
    ?(tensor_name : string option = None)
    ?(quant_parameter_tensor_names : string_string_entry_proto list = [])
    ()
    : tensor_annotation
  =
  { tensor_name; quant_parameter_tensor_names }


let rec default_attribute_proto
    ?(name : string option = None)
    ?(ref_attr_name : string option = None)
    ?(doc_string : string option = None)
    ?(type_ : attribute_proto_attribute_type option = None)
    ?(f : float option = None)
    ?(i : int64 option = None)
    ?(s : bytes option = None)
    ?(t : tensor_proto option = None)
    ?(g : graph_proto option = None)
    ?(sparse_tensor : sparse_tensor_proto option = None)
    ?(floats : float list = [])
    ?(ints : int64 list = [])
    ?(strings : bytes list = [])
    ?(tensors : tensor_proto list = [])
    ?(graphs : graph_proto list = [])
    ?(sparse_tensors : sparse_tensor_proto list = [])
    ()
    : attribute_proto
  =
  { name
  ; ref_attr_name
  ; doc_string
  ; type_
  ; f
  ; i
  ; s
  ; t
  ; g
  ; sparse_tensor
  ; floats
  ; ints
  ; strings
  ; tensors
  ; graphs
  ; sparse_tensors
  }


and default_graph_proto
    ?(node : node_proto list = [])
    ?(name : string option = None)
    ?(initializer_ : tensor_proto list = [])
    ?(sparse_initializer : sparse_tensor_proto list = [])
    ?(doc_string : string option = None)
    ?(input : value_info_proto list = [])
    ?(output : value_info_proto list = [])
    ?(value_info : value_info_proto list = [])
    ?(quantization_annotation : tensor_annotation list = [])
    ()
    : graph_proto
  =
  { node
  ; name
  ; initializer_
  ; sparse_initializer
  ; doc_string
  ; input
  ; output
  ; value_info
  ; quantization_annotation
  }


and default_node_proto
    ?(input : string list = [])
    ?(output : string list = [])
    ?(name : string option = None)
    ?(op_type : string option = None)
    ?(domain : string option = None)
    ?(attribute : attribute_proto list = [])
    ?(doc_string : string option = None)
    ()
    : node_proto
  =
  { input; output; name; op_type; domain; attribute; doc_string }


let rec default_operator_set_id_proto
    ?(domain : string option = None)
    ?(version : int64 option = None)
    ()
    : operator_set_id_proto
  =
  { domain; version }


let rec default_model_proto
    ?(ir_version : int64 option = None)
    ?(opset_import : operator_set_id_proto list = [])
    ?(producer_name : string option = None)
    ?(producer_version : string option = None)
    ?(domain : string option = None)
    ?(model_version : int64 option = None)
    ?(doc_string : string option = None)
    ?(graph : graph_proto option = None)
    ?(metadata_props : string_string_entry_proto list = [])
    ()
    : model_proto
  =
  { ir_version
  ; opset_import
  ; producer_name
  ; producer_version
  ; domain
  ; model_version
  ; doc_string
  ; graph
  ; metadata_props
  }


let rec default_tensor_proto_data_type () = (Undefined : tensor_proto_data_type)
