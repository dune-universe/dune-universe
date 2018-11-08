(** tensor.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_tensor_proto : Tensor_types.tensor_proto -> Pbrt.Encoder.t -> unit
(** [encode_tensor_proto v encoder] encodes [v] with the given [encoder] *)

val encode_variant_tensor_data_proto : Tensor_types.variant_tensor_data_proto -> Pbrt.Encoder.t -> unit
(** [encode_variant_tensor_data_proto v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_tensor_proto : Pbrt.Decoder.t -> Tensor_types.tensor_proto
(** [decode_tensor_proto decoder] decodes a [tensor_proto] value from [decoder] *)

val decode_variant_tensor_data_proto : Pbrt.Decoder.t -> Tensor_types.variant_tensor_data_proto
(** [decode_variant_tensor_data_proto decoder] decodes a [variant_tensor_data_proto] value from [decoder] *)
