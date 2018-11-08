(** tensor.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_tensor_proto : Format.formatter -> Tensor_types.tensor_proto -> unit 
(** [pp_tensor_proto v] formats v *)

val pp_variant_tensor_data_proto : Format.formatter -> Tensor_types.variant_tensor_data_proto -> unit 
(** [pp_variant_tensor_data_proto v] formats v *)
