[@@@ocaml.warning "-27-30-39"]

let rec pp_summary_description fmt (v:Summary_types.summary_description) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "type_hint" Pbrt.Pp.pp_string fmt v.Summary_types.type_hint;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_histogram_proto fmt (v:Summary_types.histogram_proto) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "min" Pbrt.Pp.pp_float fmt v.Summary_types.min;
    Pbrt.Pp.pp_record_field "max" Pbrt.Pp.pp_float fmt v.Summary_types.max;
    Pbrt.Pp.pp_record_field "num" Pbrt.Pp.pp_float fmt v.Summary_types.num;
    Pbrt.Pp.pp_record_field "sum" Pbrt.Pp.pp_float fmt v.Summary_types.sum;
    Pbrt.Pp.pp_record_field "sum_squares" Pbrt.Pp.pp_float fmt v.Summary_types.sum_squares;
    Pbrt.Pp.pp_record_field "bucket_limit" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.Summary_types.bucket_limit;
    Pbrt.Pp.pp_record_field "bucket" (Pbrt.Pp.pp_list Pbrt.Pp.pp_float) fmt v.Summary_types.bucket;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_metadata_plugin_data fmt (v:Summary_types.summary_metadata_plugin_data) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "plugin_name" Pbrt.Pp.pp_string fmt v.Summary_types.plugin_name;
    Pbrt.Pp.pp_record_field "content" Pbrt.Pp.pp_bytes fmt v.Summary_types.content;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_metadata fmt (v:Summary_types.summary_metadata) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "plugin_data" (Pbrt.Pp.pp_option pp_summary_metadata_plugin_data) fmt v.Summary_types.plugin_data;
    Pbrt.Pp.pp_record_field "display_name" Pbrt.Pp.pp_string fmt v.Summary_types.display_name;
    Pbrt.Pp.pp_record_field "summary_description" Pbrt.Pp.pp_string fmt v.Summary_types.summary_description;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_image fmt (v:Summary_types.summary_image) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "height" Pbrt.Pp.pp_int32 fmt v.Summary_types.height;
    Pbrt.Pp.pp_record_field "width" Pbrt.Pp.pp_int32 fmt v.Summary_types.width;
    Pbrt.Pp.pp_record_field "colorspace" Pbrt.Pp.pp_int32 fmt v.Summary_types.colorspace;
    Pbrt.Pp.pp_record_field "encoded_image_string" Pbrt.Pp.pp_bytes fmt v.Summary_types.encoded_image_string;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_audio fmt (v:Summary_types.summary_audio) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "sample_rate" Pbrt.Pp.pp_float fmt v.Summary_types.sample_rate;
    Pbrt.Pp.pp_record_field "num_channels" Pbrt.Pp.pp_int64 fmt v.Summary_types.num_channels;
    Pbrt.Pp.pp_record_field "length_frames" Pbrt.Pp.pp_int64 fmt v.Summary_types.length_frames;
    Pbrt.Pp.pp_record_field "encoded_audio_string" Pbrt.Pp.pp_bytes fmt v.Summary_types.encoded_audio_string;
    Pbrt.Pp.pp_record_field "content_type" Pbrt.Pp.pp_string fmt v.Summary_types.content_type;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary_value_value fmt (v:Summary_types.summary_value_value) =
  match v with
  | Summary_types.Simple_value x -> Format.fprintf fmt "@[Simple_value(%a)@]" Pbrt.Pp.pp_float x
  | Summary_types.Obsolete_old_style_histogram x -> Format.fprintf fmt "@[Obsolete_old_style_histogram(%a)@]" Pbrt.Pp.pp_bytes x
  | Summary_types.Image x -> Format.fprintf fmt "@[Image(%a)@]" pp_summary_image x
  | Summary_types.Histo x -> Format.fprintf fmt "@[Histo(%a)@]" pp_histogram_proto x
  | Summary_types.Audio x -> Format.fprintf fmt "@[Audio(%a)@]" pp_summary_audio x
  | Summary_types.Tensor x -> Format.fprintf fmt "@[Tensor(%a)@]" Tensor_pp.pp_tensor_proto x

and pp_summary_value fmt (v:Summary_types.summary_value) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "node_name" Pbrt.Pp.pp_string fmt v.Summary_types.node_name;
    Pbrt.Pp.pp_record_field "tag" Pbrt.Pp.pp_string fmt v.Summary_types.tag;
    Pbrt.Pp.pp_record_field "metadata" (Pbrt.Pp.pp_option pp_summary_metadata) fmt v.Summary_types.metadata;
    Pbrt.Pp.pp_record_field "value" pp_summary_value_value fmt v.Summary_types.value;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()

let rec pp_summary fmt (v:Summary_types.summary) = 
  let pp_i fmt () =
    Format.pp_open_vbox fmt 1;
    Pbrt.Pp.pp_record_field "value" (Pbrt.Pp.pp_list pp_summary_value) fmt v.Summary_types.value;
    Format.pp_close_box fmt ()
  in
  Pbrt.Pp.pp_brk pp_i fmt ()
