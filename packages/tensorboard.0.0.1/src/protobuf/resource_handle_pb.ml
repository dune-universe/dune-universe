[@@@ocaml.warning "-27-30-39"]

type resource_handle_proto_mutable = {
  mutable device : string;
  mutable container : string;
  mutable name : string;
  mutable hash_code : int64;
  mutable maybe_type_name : string;
}

let default_resource_handle_proto_mutable () : resource_handle_proto_mutable = {
  device = "";
  container = "";
  name = "";
  hash_code = 0L;
  maybe_type_name = "";
}


let rec decode_resource_handle_proto d =
  let v = default_resource_handle_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.device <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_handle_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.container <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_handle_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_handle_proto), field(3)" pk
    | Some (4, Pbrt.Varint) -> begin
      v.hash_code <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_handle_proto), field(4)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.maybe_type_name <- Pbrt.Decoder.string d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(resource_handle_proto), field(5)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Resource_handle_types.device = v.device;
    Resource_handle_types.container = v.container;
    Resource_handle_types.name = v.name;
    Resource_handle_types.hash_code = v.hash_code;
    Resource_handle_types.maybe_type_name = v.maybe_type_name;
  } : Resource_handle_types.resource_handle_proto)

let rec encode_resource_handle_proto (v:Resource_handle_types.resource_handle_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Resource_handle_types.device encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Resource_handle_types.container encoder;
  Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Resource_handle_types.name encoder;
  Pbrt.Encoder.key (4, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Resource_handle_types.hash_code encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Resource_handle_types.maybe_type_name encoder;
  ()
