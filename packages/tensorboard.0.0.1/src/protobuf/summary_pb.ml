[@@@ocaml.warning "-27-30-39"]

type summary_description_mutable = {
  mutable type_hint : string;
}

let default_summary_description_mutable () : summary_description_mutable = {
  type_hint = "";
}

type histogram_proto_mutable = {
  mutable min : float;
  mutable max : float;
  mutable num : float;
  mutable sum : float;
  mutable sum_squares : float;
  mutable bucket_limit : float list;
  mutable bucket : float list;
}

let default_histogram_proto_mutable () : histogram_proto_mutable = {
  min = 0.;
  max = 0.;
  num = 0.;
  sum = 0.;
  sum_squares = 0.;
  bucket_limit = [];
  bucket = [];
}

type summary_metadata_plugin_data_mutable = {
  mutable plugin_name : string;
  mutable content : bytes;
}

let default_summary_metadata_plugin_data_mutable () : summary_metadata_plugin_data_mutable = {
  plugin_name = "";
  content = Bytes.create 0;
}

type summary_metadata_mutable = {
  mutable plugin_data : Summary_types.summary_metadata_plugin_data option;
  mutable display_name : string;
  mutable summary_description : string;
}

let default_summary_metadata_mutable () : summary_metadata_mutable = {
  plugin_data = None;
  display_name = "";
  summary_description = "";
}

type summary_image_mutable = {
  mutable height : int32;
  mutable width : int32;
  mutable colorspace : int32;
  mutable encoded_image_string : bytes;
}

let default_summary_image_mutable () : summary_image_mutable = {
  height = 0l;
  width = 0l;
  colorspace = 0l;
  encoded_image_string = Bytes.create 0;
}

type summary_audio_mutable = {
  mutable sample_rate : float;
  mutable num_channels : int64;
  mutable length_frames : int64;
  mutable encoded_audio_string : bytes;
  mutable content_type : string;
}

let default_summary_audio_mutable () : summary_audio_mutable = {
  sample_rate = 0.;
  num_channels = 0L;
  length_frames = 0L;
  encoded_audio_string = Bytes.create 0;
  content_type = "";
}

type summary_value_mutable = {
  mutable node_name : string;
  mutable tag : string;
  mutable metadata : Summary_types.summary_metadata option;
  mutable value : Summary_types.summary_value_value;
}

let default_summary_value_mutable () : summary_value_mutable = {
  node_name = "";
  tag = "";
  metadata = None;
  value = Summary_types.Simple_value (0.);
}

type summary_mutable = {
  mutable value : Summary_types.summary_value list;
}

let default_summary_mutable () : summary_mutable = {
  value = [];
}


let rec decode_summary_description d =
  let v = default_summary_description_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.type_hint <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_description), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.type_hint = v.type_hint;
  } : Summary_types.summary_description)

let rec decode_histogram_proto d =
  let v = default_histogram_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.bucket <- List.rev v.bucket;
      v.bucket_limit <- List.rev v.bucket_limit;
    ); continue__ := false
    | Some (1, Pbrt.Bits64) -> begin
      v.min <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_proto), field(1)" pk
    | Some (2, Pbrt.Bits64) -> begin
      v.max <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_proto), field(2)" pk
    | Some (3, Pbrt.Bits64) -> begin
      v.num <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_proto), field(3)" pk
    | Some (4, Pbrt.Bits64) -> begin
      v.sum <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_proto), field(4)" pk
    | Some (5, Pbrt.Bits64) -> begin
      v.sum_squares <- Pbrt.Decoder.float_as_bits64 d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.bucket_limit <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_proto), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.bucket <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(histogram_proto), field(7)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.min = v.min;
    Summary_types.max = v.max;
    Summary_types.num = v.num;
    Summary_types.sum = v.sum;
    Summary_types.sum_squares = v.sum_squares;
    Summary_types.bucket_limit = v.bucket_limit;
    Summary_types.bucket = v.bucket;
  } : Summary_types.histogram_proto)

let rec decode_summary_metadata_plugin_data d =
  let v = default_summary_metadata_plugin_data_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.plugin_name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_metadata_plugin_data), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.content <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_metadata_plugin_data), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.plugin_name = v.plugin_name;
    Summary_types.content = v.content;
  } : Summary_types.summary_metadata_plugin_data)

let rec decode_summary_metadata d =
  let v = default_summary_metadata_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.plugin_data <- Some (decode_summary_metadata_plugin_data (Pbrt.Decoder.nested d));
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_metadata), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.display_name <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_metadata), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.summary_description <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_metadata), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.plugin_data = v.plugin_data;
    Summary_types.display_name = v.display_name;
    Summary_types.summary_description = v.summary_description;
  } : Summary_types.summary_metadata)

let rec decode_summary_image d =
  let v = default_summary_image_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.height <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_image), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.width <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_image), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.colorspace <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_image), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.encoded_image_string <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_image), field(4)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.height = v.height;
    Summary_types.width = v.width;
    Summary_types.colorspace = v.colorspace;
    Summary_types.encoded_image_string = v.encoded_image_string;
  } : Summary_types.summary_image)

let rec decode_summary_audio d =
  let v = default_summary_audio_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bits32) -> begin
      v.sample_rate <- Pbrt.Decoder.float_as_bits32 d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_audio), field(1)" pk
    | Some (2, Pbrt.Varint) -> begin
      v.num_channels <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_audio), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.length_frames <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_audio), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.encoded_audio_string <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_audio), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.content_type <- Pbrt.Decoder.string d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_audio), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.sample_rate = v.sample_rate;
    Summary_types.num_channels = v.num_channels;
    Summary_types.length_frames = v.length_frames;
    Summary_types.encoded_audio_string = v.encoded_audio_string;
    Summary_types.content_type = v.content_type;
  } : Summary_types.summary_audio)

let rec decode_summary_value_value d = 
  let rec loop () = 
    let ret:Summary_types.summary_value_value = match Pbrt.Decoder.key d with
      | None -> Pbrt.Decoder.malformed_variant "summary_value_value"
      | Some (2, _) -> Summary_types.Simple_value (Pbrt.Decoder.float_as_bits32 d)
      | Some (3, _) -> Summary_types.Obsolete_old_style_histogram (Pbrt.Decoder.bytes d)
      | Some (4, _) -> Summary_types.Image (decode_summary_image (Pbrt.Decoder.nested d))
      | Some (5, _) -> Summary_types.Histo (decode_histogram_proto (Pbrt.Decoder.nested d))
      | Some (6, _) -> Summary_types.Audio (decode_summary_audio (Pbrt.Decoder.nested d))
      | Some (8, _) -> Summary_types.Tensor (Tensor_pb.decode_tensor_proto (Pbrt.Decoder.nested d))
      | Some (n, payload_kind) -> (
        Pbrt.Decoder.skip d payload_kind; 
        loop () 
      )
    in
    ret
  in
  loop ()

and decode_summary_value d =
  let v = default_summary_value_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (7, Pbrt.Bytes) -> begin
      v.node_name <- Pbrt.Decoder.string d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(7)" pk
    | Some (1, Pbrt.Bytes) -> begin
      v.tag <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(1)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.metadata <- Some (decode_summary_metadata (Pbrt.Decoder.nested d));
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(9)" pk
    | Some (2, Pbrt.Bits32) -> begin
      v.value <- Summary_types.Simple_value (Pbrt.Decoder.float_as_bits32 d);
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.value <- Summary_types.Obsolete_old_style_histogram (Pbrt.Decoder.bytes d);
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.value <- Summary_types.Image (decode_summary_image (Pbrt.Decoder.nested d));
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.value <- Summary_types.Histo (decode_histogram_proto (Pbrt.Decoder.nested d));
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.value <- Summary_types.Audio (decode_summary_audio (Pbrt.Decoder.nested d));
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(6)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.value <- Summary_types.Tensor (Tensor_pb.decode_tensor_proto (Pbrt.Decoder.nested d));
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary_value), field(8)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.node_name = v.node_name;
    Summary_types.tag = v.tag;
    Summary_types.metadata = v.metadata;
    Summary_types.value = v.value;
  } : Summary_types.summary_value)

let rec decode_summary d =
  let v = default_summary_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.value <- List.rev v.value;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.value <- (decode_summary_value (Pbrt.Decoder.nested d)) :: v.value;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(summary), field(1)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Summary_types.value = v.value;
  } : Summary_types.summary)

let rec encode_summary_description (v:Summary_types.summary_description) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Summary_types.type_hint encoder;
  ()

let rec encode_histogram_proto (v:Summary_types.histogram_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Summary_types.min encoder;
  Pbrt.Encoder.key (2, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Summary_types.max encoder;
  Pbrt.Encoder.key (3, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Summary_types.num encoder;
  Pbrt.Encoder.key (4, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Summary_types.sum encoder;
  Pbrt.Encoder.key (5, Pbrt.Bits64) encoder; 
  Pbrt.Encoder.float_as_bits64 v.Summary_types.sum_squares encoder;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) v.Summary_types.bucket_limit;
  ) encoder;
  Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) v.Summary_types.bucket;
  ) encoder;
  ()

let rec encode_summary_metadata_plugin_data (v:Summary_types.summary_metadata_plugin_data) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Summary_types.plugin_name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Summary_types.content encoder;
  ()

let rec encode_summary_metadata (v:Summary_types.summary_metadata) encoder = 
  begin match v.Summary_types.plugin_data with
  | Some x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_metadata_plugin_data x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Summary_types.display_name encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Summary_types.summary_description encoder;
  ()

let rec encode_summary_image (v:Summary_types.summary_image) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Summary_types.height encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Summary_types.width encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Summary_types.colorspace encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Summary_types.encoded_image_string encoder;
  ()

let rec encode_summary_audio (v:Summary_types.summary_audio) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bits32) encoder; 
  Pbrt.Encoder.float_as_bits32 v.Summary_types.sample_rate encoder;
  Pbrt.Encoder.key (2, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Summary_types.num_channels encoder;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Summary_types.length_frames encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Summary_types.encoded_audio_string encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Summary_types.content_type encoder;
  ()

let rec encode_summary_value_value (v:Summary_types.summary_value_value) encoder = 
  begin match v with
  | Summary_types.Simple_value x ->
    Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | Summary_types.Obsolete_old_style_histogram x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  | Summary_types.Image x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_image x) encoder;
  | Summary_types.Histo x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_histogram_proto x) encoder;
  | Summary_types.Audio x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_audio x) encoder;
  | Summary_types.Tensor x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Tensor_pb.encode_tensor_proto x) encoder;
  end

and encode_summary_value (v:Summary_types.summary_value) encoder = 
  Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Summary_types.node_name encoder;
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Summary_types.tag encoder;
  begin match v.Summary_types.metadata with
  | Some x -> 
    Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_metadata x) encoder;
  | None -> ();
  end;
  begin match v.Summary_types.value with
  | Summary_types.Simple_value x ->
    Pbrt.Encoder.key (2, Pbrt.Bits32) encoder; 
    Pbrt.Encoder.float_as_bits32 x encoder;
  | Summary_types.Obsolete_old_style_histogram x ->
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  | Summary_types.Image x ->
    Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_image x) encoder;
  | Summary_types.Histo x ->
    Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_histogram_proto x) encoder;
  | Summary_types.Audio x ->
    Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_audio x) encoder;
  | Summary_types.Tensor x ->
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Tensor_pb.encode_tensor_proto x) encoder;
  end;
  ()

let rec encode_summary (v:Summary_types.summary) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_summary_value x) encoder;
  ) v.Summary_types.value;
  ()
