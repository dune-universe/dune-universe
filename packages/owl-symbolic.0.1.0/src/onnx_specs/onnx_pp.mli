(** onnx.proto Pretty Printing *)

(** {2 Formatters} *)

val pp_version : Format.formatter -> Onnx_types.version -> unit
(** [pp_version v] formats v *)

val pp_attribute_proto_attribute_type
  :  Format.formatter
  -> Onnx_types.attribute_proto_attribute_type
  -> unit
(** [pp_attribute_proto_attribute_type v] formats v *)

val pp_tensor_proto_segment : Format.formatter -> Onnx_types.tensor_proto_segment -> unit
(** [pp_tensor_proto_segment v] formats v *)

val pp_string_string_entry_proto
  :  Format.formatter
  -> Onnx_types.string_string_entry_proto
  -> unit
(** [pp_string_string_entry_proto v] formats v *)

val pp_tensor_proto_data_location
  :  Format.formatter
  -> Onnx_types.tensor_proto_data_location
  -> unit
(** [pp_tensor_proto_data_location v] formats v *)

val pp_tensor_proto : Format.formatter -> Onnx_types.tensor_proto -> unit
(** [pp_tensor_proto v] formats v *)

val pp_sparse_tensor_proto : Format.formatter -> Onnx_types.sparse_tensor_proto -> unit
(** [pp_sparse_tensor_proto v] formats v *)

val pp_tensor_shape_proto_dimension_value
  :  Format.formatter
  -> Onnx_types.tensor_shape_proto_dimension_value
  -> unit
(** [pp_tensor_shape_proto_dimension_value v] formats v *)

val pp_tensor_shape_proto_dimension
  :  Format.formatter
  -> Onnx_types.tensor_shape_proto_dimension
  -> unit
(** [pp_tensor_shape_proto_dimension v] formats v *)

val pp_tensor_shape_proto : Format.formatter -> Onnx_types.tensor_shape_proto -> unit
(** [pp_tensor_shape_proto v] formats v *)

val pp_type_proto_tensor : Format.formatter -> Onnx_types.type_proto_tensor -> unit
(** [pp_type_proto_tensor v] formats v *)

val pp_type_proto_value : Format.formatter -> Onnx_types.type_proto_value -> unit
(** [pp_type_proto_value v] formats v *)

val pp_type_proto : Format.formatter -> Onnx_types.type_proto -> unit
(** [pp_type_proto v] formats v *)

val pp_type_proto_sequence : Format.formatter -> Onnx_types.type_proto_sequence -> unit
(** [pp_type_proto_sequence v] formats v *)

val pp_type_proto_map : Format.formatter -> Onnx_types.type_proto_map -> unit
(** [pp_type_proto_map v] formats v *)

val pp_value_info_proto : Format.formatter -> Onnx_types.value_info_proto -> unit
(** [pp_value_info_proto v] formats v *)

val pp_tensor_annotation : Format.formatter -> Onnx_types.tensor_annotation -> unit
(** [pp_tensor_annotation v] formats v *)

val pp_attribute_proto : Format.formatter -> Onnx_types.attribute_proto -> unit
(** [pp_attribute_proto v] formats v *)

val pp_graph_proto : Format.formatter -> Onnx_types.graph_proto -> unit
(** [pp_graph_proto v] formats v *)

val pp_node_proto : Format.formatter -> Onnx_types.node_proto -> unit
(** [pp_node_proto v] formats v *)

val pp_operator_set_id_proto
  :  Format.formatter
  -> Onnx_types.operator_set_id_proto
  -> unit
(** [pp_operator_set_id_proto v] formats v *)

val pp_model_proto : Format.formatter -> Onnx_types.model_proto -> unit
(** [pp_model_proto v] formats v *)

val pp_tensor_proto_data_type
  :  Format.formatter
  -> Onnx_types.tensor_proto_data_type
  -> unit
(** [pp_tensor_proto_data_type v] formats v *)
