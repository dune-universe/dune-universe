[@@@ocaml.warning "-27-30-39"]


let rec decode_data_type d = 
  match Pbrt.Decoder.int_as_varint d with
  | 0 -> (Types_types.Dt_invalid:Types_types.data_type)
  | 1 -> (Types_types.Dt_float:Types_types.data_type)
  | 2 -> (Types_types.Dt_double:Types_types.data_type)
  | 3 -> (Types_types.Dt_int32:Types_types.data_type)
  | 4 -> (Types_types.Dt_uint8:Types_types.data_type)
  | 5 -> (Types_types.Dt_int16:Types_types.data_type)
  | 6 -> (Types_types.Dt_int8:Types_types.data_type)
  | 7 -> (Types_types.Dt_string:Types_types.data_type)
  | 8 -> (Types_types.Dt_complex64:Types_types.data_type)
  | 9 -> (Types_types.Dt_int64:Types_types.data_type)
  | 10 -> (Types_types.Dt_bool:Types_types.data_type)
  | 11 -> (Types_types.Dt_qint8:Types_types.data_type)
  | 12 -> (Types_types.Dt_quint8:Types_types.data_type)
  | 13 -> (Types_types.Dt_qint32:Types_types.data_type)
  | 14 -> (Types_types.Dt_bfloat16:Types_types.data_type)
  | 15 -> (Types_types.Dt_qint16:Types_types.data_type)
  | 16 -> (Types_types.Dt_quint16:Types_types.data_type)
  | 17 -> (Types_types.Dt_uint16:Types_types.data_type)
  | 18 -> (Types_types.Dt_complex128:Types_types.data_type)
  | 19 -> (Types_types.Dt_half:Types_types.data_type)
  | 20 -> (Types_types.Dt_resource:Types_types.data_type)
  | 21 -> (Types_types.Dt_variant:Types_types.data_type)
  | 22 -> (Types_types.Dt_uint32:Types_types.data_type)
  | 23 -> (Types_types.Dt_uint64:Types_types.data_type)
  | 101 -> (Types_types.Dt_float_ref:Types_types.data_type)
  | 102 -> (Types_types.Dt_double_ref:Types_types.data_type)
  | 103 -> (Types_types.Dt_int32_ref:Types_types.data_type)
  | 104 -> (Types_types.Dt_uint8_ref:Types_types.data_type)
  | 105 -> (Types_types.Dt_int16_ref:Types_types.data_type)
  | 106 -> (Types_types.Dt_int8_ref:Types_types.data_type)
  | 107 -> (Types_types.Dt_string_ref:Types_types.data_type)
  | 108 -> (Types_types.Dt_complex64_ref:Types_types.data_type)
  | 109 -> (Types_types.Dt_int64_ref:Types_types.data_type)
  | 110 -> (Types_types.Dt_bool_ref:Types_types.data_type)
  | 111 -> (Types_types.Dt_qint8_ref:Types_types.data_type)
  | 112 -> (Types_types.Dt_quint8_ref:Types_types.data_type)
  | 113 -> (Types_types.Dt_qint32_ref:Types_types.data_type)
  | 114 -> (Types_types.Dt_bfloat16_ref:Types_types.data_type)
  | 115 -> (Types_types.Dt_qint16_ref:Types_types.data_type)
  | 116 -> (Types_types.Dt_quint16_ref:Types_types.data_type)
  | 117 -> (Types_types.Dt_uint16_ref:Types_types.data_type)
  | 118 -> (Types_types.Dt_complex128_ref:Types_types.data_type)
  | 119 -> (Types_types.Dt_half_ref:Types_types.data_type)
  | 120 -> (Types_types.Dt_resource_ref:Types_types.data_type)
  | 121 -> (Types_types.Dt_variant_ref:Types_types.data_type)
  | 122 -> (Types_types.Dt_uint32_ref:Types_types.data_type)
  | 123 -> (Types_types.Dt_uint64_ref:Types_types.data_type)
  | _ -> Pbrt.Decoder.malformed_variant "data_type"

let rec encode_data_type (v:Types_types.data_type) encoder =
  match v with
  | Types_types.Dt_invalid -> Pbrt.Encoder.int_as_varint (0) encoder
  | Types_types.Dt_float -> Pbrt.Encoder.int_as_varint 1 encoder
  | Types_types.Dt_double -> Pbrt.Encoder.int_as_varint 2 encoder
  | Types_types.Dt_int32 -> Pbrt.Encoder.int_as_varint 3 encoder
  | Types_types.Dt_uint8 -> Pbrt.Encoder.int_as_varint 4 encoder
  | Types_types.Dt_int16 -> Pbrt.Encoder.int_as_varint 5 encoder
  | Types_types.Dt_int8 -> Pbrt.Encoder.int_as_varint 6 encoder
  | Types_types.Dt_string -> Pbrt.Encoder.int_as_varint 7 encoder
  | Types_types.Dt_complex64 -> Pbrt.Encoder.int_as_varint 8 encoder
  | Types_types.Dt_int64 -> Pbrt.Encoder.int_as_varint 9 encoder
  | Types_types.Dt_bool -> Pbrt.Encoder.int_as_varint 10 encoder
  | Types_types.Dt_qint8 -> Pbrt.Encoder.int_as_varint 11 encoder
  | Types_types.Dt_quint8 -> Pbrt.Encoder.int_as_varint 12 encoder
  | Types_types.Dt_qint32 -> Pbrt.Encoder.int_as_varint 13 encoder
  | Types_types.Dt_bfloat16 -> Pbrt.Encoder.int_as_varint 14 encoder
  | Types_types.Dt_qint16 -> Pbrt.Encoder.int_as_varint 15 encoder
  | Types_types.Dt_quint16 -> Pbrt.Encoder.int_as_varint 16 encoder
  | Types_types.Dt_uint16 -> Pbrt.Encoder.int_as_varint 17 encoder
  | Types_types.Dt_complex128 -> Pbrt.Encoder.int_as_varint 18 encoder
  | Types_types.Dt_half -> Pbrt.Encoder.int_as_varint 19 encoder
  | Types_types.Dt_resource -> Pbrt.Encoder.int_as_varint 20 encoder
  | Types_types.Dt_variant -> Pbrt.Encoder.int_as_varint 21 encoder
  | Types_types.Dt_uint32 -> Pbrt.Encoder.int_as_varint 22 encoder
  | Types_types.Dt_uint64 -> Pbrt.Encoder.int_as_varint 23 encoder
  | Types_types.Dt_float_ref -> Pbrt.Encoder.int_as_varint 101 encoder
  | Types_types.Dt_double_ref -> Pbrt.Encoder.int_as_varint 102 encoder
  | Types_types.Dt_int32_ref -> Pbrt.Encoder.int_as_varint 103 encoder
  | Types_types.Dt_uint8_ref -> Pbrt.Encoder.int_as_varint 104 encoder
  | Types_types.Dt_int16_ref -> Pbrt.Encoder.int_as_varint 105 encoder
  | Types_types.Dt_int8_ref -> Pbrt.Encoder.int_as_varint 106 encoder
  | Types_types.Dt_string_ref -> Pbrt.Encoder.int_as_varint 107 encoder
  | Types_types.Dt_complex64_ref -> Pbrt.Encoder.int_as_varint 108 encoder
  | Types_types.Dt_int64_ref -> Pbrt.Encoder.int_as_varint 109 encoder
  | Types_types.Dt_bool_ref -> Pbrt.Encoder.int_as_varint 110 encoder
  | Types_types.Dt_qint8_ref -> Pbrt.Encoder.int_as_varint 111 encoder
  | Types_types.Dt_quint8_ref -> Pbrt.Encoder.int_as_varint 112 encoder
  | Types_types.Dt_qint32_ref -> Pbrt.Encoder.int_as_varint 113 encoder
  | Types_types.Dt_bfloat16_ref -> Pbrt.Encoder.int_as_varint 114 encoder
  | Types_types.Dt_qint16_ref -> Pbrt.Encoder.int_as_varint 115 encoder
  | Types_types.Dt_quint16_ref -> Pbrt.Encoder.int_as_varint 116 encoder
  | Types_types.Dt_uint16_ref -> Pbrt.Encoder.int_as_varint 117 encoder
  | Types_types.Dt_complex128_ref -> Pbrt.Encoder.int_as_varint 118 encoder
  | Types_types.Dt_half_ref -> Pbrt.Encoder.int_as_varint 119 encoder
  | Types_types.Dt_resource_ref -> Pbrt.Encoder.int_as_varint 120 encoder
  | Types_types.Dt_variant_ref -> Pbrt.Encoder.int_as_varint 121 encoder
  | Types_types.Dt_uint32_ref -> Pbrt.Encoder.int_as_varint 122 encoder
  | Types_types.Dt_uint64_ref -> Pbrt.Encoder.int_as_varint 123 encoder
