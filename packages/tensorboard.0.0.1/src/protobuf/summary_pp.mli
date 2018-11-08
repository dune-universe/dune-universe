(** summary.proto Pretty Printing *)


(** {2 Formatters} *)

val pp_summary_description : Format.formatter -> Summary_types.summary_description -> unit 
(** [pp_summary_description v] formats v *)

val pp_histogram_proto : Format.formatter -> Summary_types.histogram_proto -> unit 
(** [pp_histogram_proto v] formats v *)

val pp_summary_metadata_plugin_data : Format.formatter -> Summary_types.summary_metadata_plugin_data -> unit 
(** [pp_summary_metadata_plugin_data v] formats v *)

val pp_summary_metadata : Format.formatter -> Summary_types.summary_metadata -> unit 
(** [pp_summary_metadata v] formats v *)

val pp_summary_image : Format.formatter -> Summary_types.summary_image -> unit 
(** [pp_summary_image v] formats v *)

val pp_summary_audio : Format.formatter -> Summary_types.summary_audio -> unit 
(** [pp_summary_audio v] formats v *)

val pp_summary_value_value : Format.formatter -> Summary_types.summary_value_value -> unit 
(** [pp_summary_value_value v] formats v *)

val pp_summary_value : Format.formatter -> Summary_types.summary_value -> unit 
(** [pp_summary_value v] formats v *)

val pp_summary : Format.formatter -> Summary_types.summary -> unit 
(** [pp_summary v] formats v *)
