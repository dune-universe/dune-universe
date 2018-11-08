[@@@ocaml.warning "-27-30-39"]

let rec pp_tensor_proto fmt (v:Tensor_types.tensor_proto) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "dtype" Types_pp.pp_data_type fmt v.Tensor_types.dtype;
    Pbrt.Pp.pp_record_field "tensor_shape" (Pbrt.Pp.pp_option Tensor_shape_pp.pp_tensor_shape_proto) fmt v.Tensor_types.tensor_shape;
    Pbrt.Pp.pp_record_field "version_number" Pbrt.Pp.pp_int32 fmt v.Tensor_types.version_number;
    Pbrt.Pp.pp_record_field "tensor_content" Pbrt.Pp.pp_bytes fmt v.Tensor_types.tensor_content;
    Pbrt.Pp.pp_record_field "half_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int32) fmt v.Tensor_types.half_val;
    Pbrt.Pp.pp_record_field "float_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.Tensor_types.float_val;
    Pbrt.Pp.pp_record_field "double_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.Tensor_types.double_val;
    Pbrt.Pp.pp_record_field "int_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int32) fmt v.Tensor_types.int_val;
    Pbrt.Pp.pp_record_field "string_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_bytes) fmt v.Tensor_types.string_val;
    Pbrt.Pp.pp_record_field "scomplex_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.Tensor_types.scomplex_val;
    Pbrt.Pp.pp_record_field "int64_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.Tensor_types.int64_val;
    Pbrt.Pp.pp_record_field "bool_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_bool) fmt v.Tensor_types.bool_val;
    Pbrt.Pp.pp_record_field "dcomplex_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.Tensor_types.dcomplex_val;
    Pbrt.Pp.pp_record_field "resource_handle_val" (Pbrt.Pp.pp_list Resource_handle_pp.pp_resource_handle_proto) fmt v.Tensor_types.resource_handle_val;
    Pbrt.Pp.pp_record_field "variant_val" (Pbrt.Pp.pp_list pp_variant_tensor_data_proto) fmt v.Tensor_types.variant_val;
    Pbrt.Pp.pp_record_field "uint32_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int32) fmt v.Tensor_types.uint32_val;
    Pbrt.Pp.pp_record_field "uint64_val" (Pbrt.Pp.pp_list Pbrt.Pp.pp_int64) fmt v.Tensor_types.uint64_val;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

and pp_variant_tensor_data_proto fmt (v:Tensor_types.variant_tensor_data_proto) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_name" Pbrt.Pp.pp_string fmt v.Tensor_types.type_name;
    Pbrt.Pp.pp_record_field "metadata" Pbrt.Pp.pp_bytes fmt v.Tensor_types.metadata;
    Pbrt.Pp.pp_record_field "tensors" (Pbrt.Pp.pp_list pp_tensor_proto) fmt v.Tensor_types.tensors;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
