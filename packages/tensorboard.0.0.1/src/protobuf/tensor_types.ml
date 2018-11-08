[@@@ocaml.warning "-27-30-39"]


type tensor_proto = {
  dtype : Types_types.data_type;
  tensor_shape : Tensor_shape_types.tensor_shape_proto option;
  version_number : int32;
  tensor_content : bytes;
  half_val : int32 list;
  float_val : float list;
  double_val : float list;
  int_val : int32 list;
  string_val : bytes list;
  scomplex_val : float list;
  int64_val : int64 list;
  bool_val : bool list;
  dcomplex_val : float list;
  resource_handle_val : Resource_handle_types.resource_handle_proto list;
  variant_val : variant_tensor_data_proto list;
  uint32_val : int32 list;
  uint64_val : int64 list;
}

and variant_tensor_data_proto = {
  type_name : string;
  metadata : bytes;
  tensors : tensor_proto list;
}

let rec default_tensor_proto 
  ?dtype:((dtype:Types_types.data_type) = Types_types.default_data_type ())
  ?tensor_shape:((tensor_shape:Tensor_shape_types.tensor_shape_proto option) = None)
  ?version_number:((version_number:int32) = 0l)
  ?tensor_content:((tensor_content:bytes) = Bytes.create 0)
  ?half_val:((half_val:int32 list) = [])
  ?float_val:((float_val:float list) = [])
  ?double_val:((double_val:float list) = [])
  ?int_val:((int_val:int32 list) = [])
  ?string_val:((string_val:bytes list) = [])
  ?scomplex_val:((scomplex_val:float list) = [])
  ?int64_val:((int64_val:int64 list) = [])
  ?bool_val:((bool_val:bool list) = [])
  ?dcomplex_val:((dcomplex_val:float list) = [])
  ?resource_handle_val:((resource_handle_val:Resource_handle_types.resource_handle_proto list) = [])
  ?variant_val:((variant_val:variant_tensor_data_proto list) = [])
  ?uint32_val:((uint32_val:int32 list) = [])
  ?uint64_val:((uint64_val:int64 list) = [])
  () : tensor_proto  = {
  dtype;
  tensor_shape;
  version_number;
  tensor_content;
  half_val;
  float_val;
  double_val;
  int_val;
  string_val;
  scomplex_val;
  int64_val;
  bool_val;
  dcomplex_val;
  resource_handle_val;
  variant_val;
  uint32_val;
  uint64_val;
}

and default_variant_tensor_data_proto 
  ?type_name:((type_name:string) = "")
  ?metadata:((metadata:bytes) = Bytes.create 0)
  ?tensors:((tensors:tensor_proto list) = [])
  () : variant_tensor_data_proto  = {
  type_name;
  metadata;
  tensors;
}
