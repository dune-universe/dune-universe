[@@@ocaml.warning "-27-30-39"]


type data_type =
  | Dt_invalid 
  | Dt_float 
  | Dt_double 
  | Dt_int32 
  | Dt_uint8 
  | Dt_int16 
  | Dt_int8 
  | Dt_string 
  | Dt_complex64 
  | Dt_int64 
  | Dt_bool 
  | Dt_qint8 
  | Dt_quint8 
  | Dt_qint32 
  | Dt_bfloat16 
  | Dt_qint16 
  | Dt_quint16 
  | Dt_uint16 
  | Dt_complex128 
  | Dt_half 
  | Dt_resource 
  | Dt_variant 
  | Dt_uint32 
  | Dt_uint64 
  | Dt_float_ref 
  | Dt_double_ref 
  | Dt_int32_ref 
  | Dt_uint8_ref 
  | Dt_int16_ref 
  | Dt_int8_ref 
  | Dt_string_ref 
  | Dt_complex64_ref 
  | Dt_int64_ref 
  | Dt_bool_ref 
  | Dt_qint8_ref 
  | Dt_quint8_ref 
  | Dt_qint32_ref 
  | Dt_bfloat16_ref 
  | Dt_qint16_ref 
  | Dt_quint16_ref 
  | Dt_uint16_ref 
  | Dt_complex128_ref 
  | Dt_half_ref 
  | Dt_resource_ref 
  | Dt_variant_ref 
  | Dt_uint32_ref 
  | Dt_uint64_ref 

let rec default_data_type () = (Dt_invalid:data_type)
