(** types.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_data_type : Types_types.data_type -> Pbrt.Encoder.t -> unit
(** [encode_data_type v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_data_type : Pbrt.Decoder.t -> Types_types.data_type
(** [decode_data_type decoder] decodes a [data_type] value from [decoder] *)
