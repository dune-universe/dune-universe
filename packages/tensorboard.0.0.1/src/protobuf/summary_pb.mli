(** summary.proto Binary Encoding *)


(** {2 Protobuf Encoding} *)

val encode_summary_description : Summary_types.summary_description -> Pbrt.Encoder.t -> unit
(** [encode_summary_description v encoder] encodes [v] with the given [encoder] *)

val encode_histogram_proto : Summary_types.histogram_proto -> Pbrt.Encoder.t -> unit
(** [encode_histogram_proto v encoder] encodes [v] with the given [encoder] *)

val encode_summary_metadata_plugin_data : Summary_types.summary_metadata_plugin_data -> Pbrt.Encoder.t -> unit
(** [encode_summary_metadata_plugin_data v encoder] encodes [v] with the given [encoder] *)

val encode_summary_metadata : Summary_types.summary_metadata -> Pbrt.Encoder.t -> unit
(** [encode_summary_metadata v encoder] encodes [v] with the given [encoder] *)

val encode_summary_image : Summary_types.summary_image -> Pbrt.Encoder.t -> unit
(** [encode_summary_image v encoder] encodes [v] with the given [encoder] *)

val encode_summary_audio : Summary_types.summary_audio -> Pbrt.Encoder.t -> unit
(** [encode_summary_audio v encoder] encodes [v] with the given [encoder] *)

val encode_summary_value_value : Summary_types.summary_value_value -> Pbrt.Encoder.t -> unit
(** [encode_summary_value_value v encoder] encodes [v] with the given [encoder] *)

val encode_summary_value : Summary_types.summary_value -> Pbrt.Encoder.t -> unit
(** [encode_summary_value v encoder] encodes [v] with the given [encoder] *)

val encode_summary : Summary_types.summary -> Pbrt.Encoder.t -> unit
(** [encode_summary v encoder] encodes [v] with the given [encoder] *)


(** {2 Protobuf Decoding} *)

val decode_summary_description : Pbrt.Decoder.t -> Summary_types.summary_description
(** [decode_summary_description decoder] decodes a [summary_description] value from [decoder] *)

val decode_histogram_proto : Pbrt.Decoder.t -> Summary_types.histogram_proto
(** [decode_histogram_proto decoder] decodes a [histogram_proto] value from [decoder] *)

val decode_summary_metadata_plugin_data : Pbrt.Decoder.t -> Summary_types.summary_metadata_plugin_data
(** [decode_summary_metadata_plugin_data decoder] decodes a [summary_metadata_plugin_data] value from [decoder] *)

val decode_summary_metadata : Pbrt.Decoder.t -> Summary_types.summary_metadata
(** [decode_summary_metadata decoder] decodes a [summary_metadata] value from [decoder] *)

val decode_summary_image : Pbrt.Decoder.t -> Summary_types.summary_image
(** [decode_summary_image decoder] decodes a [summary_image] value from [decoder] *)

val decode_summary_audio : Pbrt.Decoder.t -> Summary_types.summary_audio
(** [decode_summary_audio decoder] decodes a [summary_audio] value from [decoder] *)

val decode_summary_value_value : Pbrt.Decoder.t -> Summary_types.summary_value_value
(** [decode_summary_value_value decoder] decodes a [summary_value_value] value from [decoder] *)

val decode_summary_value : Pbrt.Decoder.t -> Summary_types.summary_value
(** [decode_summary_value decoder] decodes a [summary_value] value from [decoder] *)

val decode_summary : Pbrt.Decoder.t -> Summary_types.summary
(** [decode_summary decoder] decodes a [summary] value from [decoder] *)
