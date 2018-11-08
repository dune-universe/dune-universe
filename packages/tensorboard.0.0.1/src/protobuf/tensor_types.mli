(** tensor.proto Types *)



(** {2 Types} *)

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


(** {2 Default values} *)

val default_tensor_proto : 
  ?dtype:Types_types.data_type ->
  ?tensor_shape:Tensor_shape_types.tensor_shape_proto option ->
  ?version_number:int32 ->
  ?tensor_content:bytes ->
  ?half_val:int32 list ->
  ?float_val:float list ->
  ?double_val:float list ->
  ?int_val:int32 list ->
  ?string_val:bytes list ->
  ?scomplex_val:float list ->
  ?int64_val:int64 list ->
  ?bool_val:bool list ->
  ?dcomplex_val:float list ->
  ?resource_handle_val:Resource_handle_types.resource_handle_proto list ->
  ?variant_val:variant_tensor_data_proto list ->
  ?uint32_val:int32 list ->
  ?uint64_val:int64 list ->
  unit ->
  tensor_proto
(** [default_tensor_proto ()] is the default value for type [tensor_proto] *)

val default_variant_tensor_data_proto : 
  ?type_name:string ->
  ?metadata:bytes ->
  ?tensors:tensor_proto list ->
  unit ->
  variant_tensor_data_proto
(** [default_variant_tensor_data_proto ()] is the default value for type [variant_tensor_data_proto] *)
