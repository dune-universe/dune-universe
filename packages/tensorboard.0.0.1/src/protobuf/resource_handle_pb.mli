(** resource_handle.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_resource_handle_proto : Resource_handle_types.resource_handle_proto -> Pbrt.Encoder.t -> unit
(** [encode_resource_handle_proto v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_resource_handle_proto : Pbrt.Decoder.t -> Resource_handle_types.resource_handle_proto
(** [decode_resource_handle_proto decoder] decodes a [resource_handle_proto] value from [decoder] *)
