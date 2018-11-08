[@@@ocaml.warning "-27-30-39"]


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

let rec default_summary_description 
  ?type_hint:((type_hint:string) = "")
  () : summary_description  = {
  type_hint;
}

let rec default_histogram_proto 
  ?min:((min:float) = 0.)
  ?max:((max:float) = 0.)
  ?num:((num:float) = 0.)
  ?sum:((sum:float) = 0.)
  ?sum_squares:((sum_squares:float) = 0.)
  ?bucket_limit:((bucket_limit:float list) = [])
  ?bucket:((bucket:float list) = [])
  () : histogram_proto  = {
  min;
  max;
  num;
  sum;
  sum_squares;
  bucket_limit;
  bucket;
}

let rec default_summary_metadata_plugin_data 
  ?plugin_name:((plugin_name:string) = "")
  ?content:((content:bytes) = Bytes.create 0)
  () : summary_metadata_plugin_data  = {
  plugin_name;
  content;
}

let rec default_summary_metadata 
  ?plugin_data:((plugin_data:summary_metadata_plugin_data option) = None)
  ?display_name:((display_name:string) = "")
  ?summary_description:((summary_description:string) = "")
  () : summary_metadata  = {
  plugin_data;
  display_name;
  summary_description;
}

let rec default_summary_image 
  ?height:((height:int32) = 0l)
  ?width:((width:int32) = 0l)
  ?colorspace:((colorspace:int32) = 0l)
  ?encoded_image_string:((encoded_image_string:bytes) = Bytes.create 0)
  () : summary_image  = {
  height;
  width;
  colorspace;
  encoded_image_string;
}

let rec default_summary_audio 
  ?sample_rate:((sample_rate:float) = 0.)
  ?num_channels:((num_channels:int64) = 0L)
  ?length_frames:((length_frames:int64) = 0L)
  ?encoded_audio_string:((encoded_audio_string:bytes) = Bytes.create 0)
  ?content_type:((content_type:string) = "")
  () : summary_audio  = {
  sample_rate;
  num_channels;
  length_frames;
  encoded_audio_string;
  content_type;
}

let rec default_summary_value_value () : summary_value_value = Simple_value (0.)

and default_summary_value 
  ?node_name:((node_name:string) = "")
  ?tag:((tag:string) = "")
  ?metadata:((metadata:summary_metadata option) = None)
  ?value:((value:summary_value_value) = Simple_value (0.))
  () : summary_value  = {
  node_name;
  tag;
  metadata;
  value;
}

let rec default_summary 
  ?value:((value:summary_value list) = [])
  () : summary  = {
  value;
}
