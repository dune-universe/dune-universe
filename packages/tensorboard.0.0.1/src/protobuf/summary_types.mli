(** summary.proto Types *)



(** {2 Types} *)

type summary_description = {
  type_hint : string;
}

type histogram_proto = {
  min : float;
  max : float;
  num : float;
  sum : float;
  sum_squares : float;
  bucket_limit : float list;
  bucket : float list;
}

type summary_metadata_plugin_data = {
  plugin_name : string;
  content : bytes;
}

type summary_metadata = {
  plugin_data : summary_metadata_plugin_data option;
  display_name : string;
  summary_description : string;
}

type summary_image = {
  height : int32;
  width : int32;
  colorspace : int32;
  encoded_image_string : bytes;
}

type summary_audio = {
  sample_rate : float;
  num_channels : int64;
  length_frames : int64;
  encoded_audio_string : bytes;
  content_type : string;
}

type summary_value_value =
  | Simple_value of float
  | Obsolete_old_style_histogram of bytes
  | Image of summary_image
  | Histo of histogram_proto
  | Audio of summary_audio
  | Tensor of Tensor_types.tensor_proto

and summary_value = {
  node_name : string;
  tag : string;
  metadata : summary_metadata option;
  value : summary_value_value;
}

type summary = {
  value : summary_value list;
}


(** {2 Default values} *)

val default_summary_description : 
  ?type_hint:string ->
  unit ->
  summary_description
(** [default_summary_description ()] is the default value for type [summary_description] *)

val default_histogram_proto : 
  ?min:float ->
  ?max:float ->
  ?num:float ->
  ?sum:float ->
  ?sum_squares:float ->
  ?bucket_limit:float list ->
  ?bucket:float list ->
  unit ->
  histogram_proto
(** [default_histogram_proto ()] is the default value for type [histogram_proto] *)

val default_summary_metadata_plugin_data : 
  ?plugin_name:string ->
  ?content:bytes ->
  unit ->
  summary_metadata_plugin_data
(** [default_summary_metadata_plugin_data ()] is the default value for type [summary_metadata_plugin_data] *)

val default_summary_metadata : 
  ?plugin_data:summary_metadata_plugin_data option ->
  ?display_name:string ->
  ?summary_description:string ->
  unit ->
  summary_metadata
(** [default_summary_metadata ()] is the default value for type [summary_metadata] *)

val default_summary_image : 
  ?height:int32 ->
  ?width:int32 ->
  ?colorspace:int32 ->
  ?encoded_image_string:bytes ->
  unit ->
  summary_image
(** [default_summary_image ()] is the default value for type [summary_image] *)

val default_summary_audio : 
  ?sample_rate:float ->
  ?num_channels:int64 ->
  ?length_frames:int64 ->
  ?encoded_audio_string:bytes ->
  ?content_type:string ->
  unit ->
  summary_audio
(** [default_summary_audio ()] is the default value for type [summary_audio] *)

val default_summary_value_value : unit -> summary_value_value
(** [default_summary_value_value ()] is the default value for type [summary_value_value] *)

val default_summary_value : 
  ?node_name:string ->
  ?tag:string ->
  ?metadata:summary_metadata option ->
  ?value:summary_value_value ->
  unit ->
  summary_value
(** [default_summary_value ()] is the default value for type [summary_value] *)

val default_summary : 
  ?value:summary_value list ->
  unit ->
  summary
(** [default_summary ()] is the default value for type [summary] *)
