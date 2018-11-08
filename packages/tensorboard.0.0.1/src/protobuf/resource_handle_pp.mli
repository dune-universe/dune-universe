(** resource_handle.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_resource_handle_proto : Format.formatter -> Resource_handle_types.resource_handle_proto -> unit 
(** [pp_resource_handle_proto v] formats v *)
