(** tensor_shape.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_tensor_shape_proto_dim : Tensor_shape_types.tensor_shape_proto_dim -> Pbrt.Encoder.t -> unit
(** [encode_tensor_shape_proto_dim v encoder] encodes [v] with the given [encoder] *)

val encode_tensor_shape_proto : Tensor_shape_types.tensor_shape_proto -> Pbrt.Encoder.t -> unit
(** [encode_tensor_shape_proto v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_tensor_shape_proto_dim : Pbrt.Decoder.t -> Tensor_shape_types.tensor_shape_proto_dim
(** [decode_tensor_shape_proto_dim decoder] decodes a [tensor_shape_proto_dim] value from [decoder] *)

val decode_tensor_shape_proto : Pbrt.Decoder.t -> Tensor_shape_types.tensor_shape_proto
(** [decode_tensor_shape_proto decoder] decodes a [tensor_shape_proto] value from [decoder] *)
