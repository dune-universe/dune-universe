(** onnx.proto Types *)

[@@@warning "-30"]

(** {2 Types} *)

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
  | Tensor_type of type_proto_tensor
  | Sequence_type of type_proto_sequence
  | Map_type of type_proto_map

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

(** {2 Default values} *)

val default_version : unit -> version
(** [default_version ()] is the default value for type [version] *)

val default_attribute_proto_attribute_type : unit -> attribute_proto_attribute_type
(** [default_attribute_proto_attribute_type ()] is the default value for type [attribute_proto_attribute_type] *)

val default_tensor_proto_segment
  :  ?begin_:int64 option
  -> ?end_:int64 option
  -> unit
  -> tensor_proto_segment
(** [default_tensor_proto_segment ()] is the default value for type [tensor_proto_segment] *)

val default_string_string_entry_proto
  :  ?key:string option
  -> ?value:string option
  -> unit
  -> string_string_entry_proto
(** [default_string_string_entry_proto ()] is the default value for type [string_string_entry_proto] *)

val default_tensor_proto_data_location : unit -> tensor_proto_data_location
(** [default_tensor_proto_data_location ()] is the default value for type [tensor_proto_data_location] *)

val default_tensor_proto
  :  ?dims:int64 list
  -> ?data_type:int32 option
  -> ?segment:tensor_proto_segment option
  -> ?float_data:float list
  -> ?int32_data:int32 list
  -> ?string_data:bytes list
  -> ?int64_data:int64 list
  -> ?name:string option
  -> ?doc_string:string option
  -> ?raw_data:bytes option
  -> ?external_data:string_string_entry_proto list
  -> ?data_location:tensor_proto_data_location option
  -> ?double_data:float list
  -> ?uint64_data:int64 list
  -> unit
  -> tensor_proto
(** [default_tensor_proto ()] is the default value for type [tensor_proto] *)

val default_sparse_tensor_proto
  :  ?values:tensor_proto option
  -> ?indices:tensor_proto option
  -> ?dims:int64 list
  -> unit
  -> sparse_tensor_proto
(** [default_sparse_tensor_proto ()] is the default value for type [sparse_tensor_proto] *)

val default_tensor_shape_proto_dimension_value
  :  unit
  -> tensor_shape_proto_dimension_value
(** [default_tensor_shape_proto_dimension_value ()] is the default value for type [tensor_shape_proto_dimension_value] *)

val default_tensor_shape_proto_dimension
  :  ?value:tensor_shape_proto_dimension_value
  -> ?denotation:string option
  -> unit
  -> tensor_shape_proto_dimension
(** [default_tensor_shape_proto_dimension ()] is the default value for type [tensor_shape_proto_dimension] *)

val default_tensor_shape_proto
  :  ?dim:tensor_shape_proto_dimension list
  -> unit
  -> tensor_shape_proto
(** [default_tensor_shape_proto ()] is the default value for type [tensor_shape_proto] *)

val default_type_proto_tensor
  :  ?elem_type:int32 option
  -> ?shape:tensor_shape_proto option
  -> unit
  -> type_proto_tensor
(** [default_type_proto_tensor ()] is the default value for type [type_proto_tensor] *)

val default_type_proto_value : unit -> type_proto_value
(** [default_type_proto_value ()] is the default value for type [type_proto_value] *)

val default_type_proto
  :  ?value:type_proto_value
  -> ?denotation:string option
  -> unit
  -> type_proto
(** [default_type_proto ()] is the default value for type [type_proto] *)

val default_type_proto_sequence
  :  ?elem_type:type_proto option
  -> unit
  -> type_proto_sequence
(** [default_type_proto_sequence ()] is the default value for type [type_proto_sequence] *)

val default_type_proto_map
  :  ?key_type:int32 option
  -> ?value_type:type_proto option
  -> unit
  -> type_proto_map
(** [default_type_proto_map ()] is the default value for type [type_proto_map] *)

val default_value_info_proto
  :  ?name:string option
  -> ?type_:type_proto option
  -> ?doc_string:string option
  -> unit
  -> value_info_proto
(** [default_value_info_proto ()] is the default value for type [value_info_proto] *)

val default_tensor_annotation
  :  ?tensor_name:string option
  -> ?quant_parameter_tensor_names:string_string_entry_proto list
  -> unit
  -> tensor_annotation
(** [default_tensor_annotation ()] is the default value for type [tensor_annotation] *)

val default_attribute_proto
  :  ?name:string option
  -> ?ref_attr_name:string option
  -> ?doc_string:string option
  -> ?type_:attribute_proto_attribute_type option
  -> ?f:float option
  -> ?i:int64 option
  -> ?s:bytes option
  -> ?t:tensor_proto option
  -> ?g:graph_proto option
  -> ?sparse_tensor:sparse_tensor_proto option
  -> ?floats:float list
  -> ?ints:int64 list
  -> ?strings:bytes list
  -> ?tensors:tensor_proto list
  -> ?graphs:graph_proto list
  -> ?sparse_tensors:sparse_tensor_proto list
  -> unit
  -> attribute_proto
(** [default_attribute_proto ()] is the default value for type [attribute_proto] *)

val default_graph_proto
  :  ?node:node_proto list
  -> ?name:string option
  -> ?initializer_:tensor_proto list
  -> ?sparse_initializer:sparse_tensor_proto list
  -> ?doc_string:string option
  -> ?input:value_info_proto list
  -> ?output:value_info_proto list
  -> ?value_info:value_info_proto list
  -> ?quantization_annotation:tensor_annotation list
  -> unit
  -> graph_proto
(** [default_graph_proto ()] is the default value for type [graph_proto] *)

val default_node_proto
  :  ?input:string list
  -> ?output:string list
  -> ?name:string option
  -> ?op_type:string option
  -> ?domain:string option
  -> ?attribute:attribute_proto list
  -> ?doc_string:string option
  -> unit
  -> node_proto
(** [default_node_proto ()] is the default value for type [node_proto] *)

val default_operator_set_id_proto
  :  ?domain:string option
  -> ?version:int64 option
  -> unit
  -> operator_set_id_proto
(** [default_operator_set_id_proto ()] is the default value for type [operator_set_id_proto] *)

val default_model_proto
  :  ?ir_version:int64 option
  -> ?opset_import:operator_set_id_proto list
  -> ?producer_name:string option
  -> ?producer_version:string option
  -> ?domain:string option
  -> ?model_version:int64 option
  -> ?doc_string:string option
  -> ?graph:graph_proto option
  -> ?metadata_props:string_string_entry_proto list
  -> unit
  -> model_proto
(** [default_model_proto ()] is the default value for type [model_proto] *)

val default_tensor_proto_data_type : unit -> tensor_proto_data_type
(** [default_tensor_proto_data_type ()] is the default value for type [tensor_proto_data_type] *)
