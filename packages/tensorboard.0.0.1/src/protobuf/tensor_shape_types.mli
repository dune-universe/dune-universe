(** tensor_shape.proto Types *)



(** {2 Types} *)

type tensor_shape_proto_dim = {
  size : int64;
  name : string;
}

type tensor_shape_proto = {
  dim : tensor_shape_proto_dim list;
  unknown_rank : bool;
}


(** {2 Default values} *)

val default_tensor_shape_proto_dim : 
  ?size:int64 ->
  ?name:string ->
  unit ->
  tensor_shape_proto_dim
(** [default_tensor_shape_proto_dim ()] is the default value for type [tensor_shape_proto_dim] *)

val default_tensor_shape_proto : 
  ?dim:tensor_shape_proto_dim list ->
  ?unknown_rank:bool ->
  unit ->
  tensor_shape_proto
(** [default_tensor_shape_proto ()] is the default value for type [tensor_shape_proto] *)
