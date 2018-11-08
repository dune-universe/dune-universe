(** tensor_shape.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_tensor_shape_proto_dim : Format.formatter -> Tensor_shape_types.tensor_shape_proto_dim -> unit 
(** [pp_tensor_shape_proto_dim v] formats v *)

val pp_tensor_shape_proto : Format.formatter -> Tensor_shape_types.tensor_shape_proto -> unit 
(** [pp_tensor_shape_proto v] formats v *)
