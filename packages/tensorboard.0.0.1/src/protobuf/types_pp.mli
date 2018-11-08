(** types.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_data_type : Format.formatter -> Types_types.data_type -> unit 
(** [pp_data_type v] formats v *)
