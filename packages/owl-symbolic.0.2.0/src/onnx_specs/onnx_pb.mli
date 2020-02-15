(** onnx.proto Binary Encoding *)

(** {2 Protobuf Encoding} *)

val encode_version : Onnx_types.version -> Pbrt.Encoder.t -> unit
(** [encode_version v encoder] encodes [v] with the given [encoder] *)

val encode_attribute_proto_attribute_type
  :  Onnx_types.attribute_proto_attribute_type
  -> Pbrt.Encoder.t
  -> unit
(** [encode_attribute_proto_attribute_type v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_proto_segment
  :  Onnx_types.tensor_proto_segment
  -> Pbrt.Encoder.t
  -> unit
(** [encode_tensor_proto_segment v encoder] encodes [v] with the given [encoder] *)

val encode_string_string_entry_proto
  :  Onnx_types.string_string_entry_proto
  -> Pbrt.Encoder.t
  -> unit
(** [encode_string_string_entry_proto v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_proto_data_location
  :  Onnx_types.tensor_proto_data_location
  -> Pbrt.Encoder.t
  -> unit
(** [encode_tensor_proto_data_location v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_proto : Onnx_types.tensor_proto -> Pbrt.Encoder.t -> unit
(** [encode_tensor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_sparse_tensor_proto : Onnx_types.sparse_tensor_proto -> Pbrt.Encoder.t -> unit
(** [encode_sparse_tensor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_shape_proto_dimension_value
  :  Onnx_types.tensor_shape_proto_dimension_value
  -> Pbrt.Encoder.t
  -> unit
(** [encode_tensor_shape_proto_dimension_value v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_shape_proto_dimension
  :  Onnx_types.tensor_shape_proto_dimension
  -> Pbrt.Encoder.t
  -> unit
(** [encode_tensor_shape_proto_dimension v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_shape_proto : Onnx_types.tensor_shape_proto -> Pbrt.Encoder.t -> unit
(** [encode_tensor_shape_proto v encoder] encodes [v] with the given [encoder] *)

val encode_type_proto_tensor : Onnx_types.type_proto_tensor -> Pbrt.Encoder.t -> unit
(** [encode_type_proto_tensor v encoder] encodes [v] with the given [encoder] *)

val encode_type_proto_value : Onnx_types.type_proto_value -> Pbrt.Encoder.t -> unit
(** [encode_type_proto_value v encoder] encodes [v] with the given [encoder] *)

val encode_type_proto : Onnx_types.type_proto -> Pbrt.Encoder.t -> unit
(** [encode_type_proto v encoder] encodes [v] with the given [encoder] *)

val encode_type_proto_sequence : Onnx_types.type_proto_sequence -> Pbrt.Encoder.t -> unit
(** [encode_type_proto_sequence v encoder] encodes [v] with the given [encoder] *)

val encode_type_proto_map : Onnx_types.type_proto_map -> Pbrt.Encoder.t -> unit
(** [encode_type_proto_map v encoder] encodes [v] with the given [encoder] *)

val encode_value_info_proto : Onnx_types.value_info_proto -> Pbrt.Encoder.t -> unit
(** [encode_value_info_proto v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_annotation : Onnx_types.tensor_annotation -> Pbrt.Encoder.t -> unit
(** [encode_tensor_annotation v encoder] encodes [v] with the given [encoder] *)

val encode_attribute_proto : Onnx_types.attribute_proto -> Pbrt.Encoder.t -> unit
(** [encode_attribute_proto v encoder] encodes [v] with the given [encoder] *)

val encode_graph_proto : Onnx_types.graph_proto -> Pbrt.Encoder.t -> unit
(** [encode_graph_proto v encoder] encodes [v] with the given [encoder] *)

val encode_node_proto : Onnx_types.node_proto -> Pbrt.Encoder.t -> unit
(** [encode_node_proto v encoder] encodes [v] with the given [encoder] *)

val encode_operator_set_id_proto
  :  Onnx_types.operator_set_id_proto
  -> Pbrt.Encoder.t
  -> unit
(** [encode_operator_set_id_proto v encoder] encodes [v] with the given [encoder] *)

val encode_model_proto : Onnx_types.model_proto -> Pbrt.Encoder.t -> unit
(** [encode_model_proto v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_proto_data_type
  :  Onnx_types.tensor_proto_data_type
  -> Pbrt.Encoder.t
  -> unit
(** [encode_tensor_proto_data_type v encoder] encodes [v] with the given [encoder] *)

(** {2 Protobuf Decoding} *)

val decode_version : Pbrt.Decoder.t -> Onnx_types.version
(** [decode_version decoder] decodes a [version] value from [decoder] *)

val decode_attribute_proto_attribute_type
  :  Pbrt.Decoder.t
  -> Onnx_types.attribute_proto_attribute_type
(** [decode_attribute_proto_attribute_type decoder] decodes a [attribute_proto_attribute_type] value from [decoder] *)

val decode_tensor_proto_segment : Pbrt.Decoder.t -> Onnx_types.tensor_proto_segment
(** [decode_tensor_proto_segment decoder] decodes a [tensor_proto_segment] value from [decoder] *)

val decode_string_string_entry_proto
  :  Pbrt.Decoder.t
  -> Onnx_types.string_string_entry_proto
(** [decode_string_string_entry_proto decoder] decodes a [string_string_entry_proto] value from [decoder] *)

val decode_tensor_proto_data_location
  :  Pbrt.Decoder.t
  -> Onnx_types.tensor_proto_data_location
(** [decode_tensor_proto_data_location decoder] decodes a [tensor_proto_data_location] value from [decoder] *)

val decode_tensor_proto : Pbrt.Decoder.t -> Onnx_types.tensor_proto
(** [decode_tensor_proto decoder] decodes a [tensor_proto] value from [decoder] *)

val decode_sparse_tensor_proto : Pbrt.Decoder.t -> Onnx_types.sparse_tensor_proto
(** [decode_sparse_tensor_proto decoder] decodes a [sparse_tensor_proto] value from [decoder] *)

val decode_tensor_shape_proto_dimension_value
  :  Pbrt.Decoder.t
  -> Onnx_types.tensor_shape_proto_dimension_value
(** [decode_tensor_shape_proto_dimension_value decoder] decodes a [tensor_shape_proto_dimension_value] value from [decoder] *)

val decode_tensor_shape_proto_dimension
  :  Pbrt.Decoder.t
  -> Onnx_types.tensor_shape_proto_dimension
(** [decode_tensor_shape_proto_dimension decoder] decodes a [tensor_shape_proto_dimension] value from [decoder] *)

val decode_tensor_shape_proto : Pbrt.Decoder.t -> Onnx_types.tensor_shape_proto
(** [decode_tensor_shape_proto decoder] decodes a [tensor_shape_proto] value from [decoder] *)

val decode_type_proto_tensor : Pbrt.Decoder.t -> Onnx_types.type_proto_tensor
(** [decode_type_proto_tensor decoder] decodes a [type_proto_tensor] value from [decoder] *)

val decode_type_proto_value : Pbrt.Decoder.t -> Onnx_types.type_proto_value
(** [decode_type_proto_value decoder] decodes a [type_proto_value] value from [decoder] *)

val decode_type_proto : Pbrt.Decoder.t -> Onnx_types.type_proto
(** [decode_type_proto decoder] decodes a [type_proto] value from [decoder] *)

val decode_type_proto_sequence : Pbrt.Decoder.t -> Onnx_types.type_proto_sequence
(** [decode_type_proto_sequence decoder] decodes a [type_proto_sequence] value from [decoder] *)

val decode_type_proto_map : Pbrt.Decoder.t -> Onnx_types.type_proto_map
(** [decode_type_proto_map decoder] decodes a [type_proto_map] value from [decoder] *)

val decode_value_info_proto : Pbrt.Decoder.t -> Onnx_types.value_info_proto
(** [decode_value_info_proto decoder] decodes a [value_info_proto] value from [decoder] *)

val decode_tensor_annotation : Pbrt.Decoder.t -> Onnx_types.tensor_annotation
(** [decode_tensor_annotation decoder] decodes a [tensor_annotation] value from [decoder] *)

val decode_attribute_proto : Pbrt.Decoder.t -> Onnx_types.attribute_proto
(** [decode_attribute_proto decoder] decodes a [attribute_proto] value from [decoder] *)

val decode_graph_proto : Pbrt.Decoder.t -> Onnx_types.graph_proto
(** [decode_graph_proto decoder] decodes a [graph_proto] value from [decoder] *)

val decode_node_proto : Pbrt.Decoder.t -> Onnx_types.node_proto
(** [decode_node_proto decoder] decodes a [node_proto] value from [decoder] *)

val decode_operator_set_id_proto : Pbrt.Decoder.t -> Onnx_types.operator_set_id_proto
(** [decode_operator_set_id_proto decoder] decodes a [operator_set_id_proto] value from [decoder] *)

val decode_model_proto : Pbrt.Decoder.t -> Onnx_types.model_proto
(** [decode_model_proto decoder] decodes a [model_proto] value from [decoder] *)

val decode_tensor_proto_data_type : Pbrt.Decoder.t -> Onnx_types.tensor_proto_data_type
(** [decode_tensor_proto_data_type decoder] decodes a [tensor_proto_data_type] value from [decoder] *)
