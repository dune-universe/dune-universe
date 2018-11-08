[@@@ocaml.warning "-27-30-39"]

type tensor_shape_proto_dim_mutable = {
  mutable size : int64;
  mutable name : string;
}

let default_tensor_shape_proto_dim_mutable () : tensor_shape_proto_dim_mutable = {
  size = 0L;
  name = "";
}

type tensor_shape_proto_mutable = {
  mutable dim : Tensor_shape_types.tensor_shape_proto_dim list;
  mutable unknown_rank : bool;
}

let default_tensor_shape_proto_mutable () : tensor_shape_proto_mutable = {
  dim = [];
  unknown_rank = false;
}


let rec decode_tensor_shape_proto_dim d =
  let v = default_tensor_shape_proto_dim_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.size <- Pbrt.Decoder.int64_as_varint d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto_dim), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.name <- Pbrt.Decoder.string d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto_dim), field(2)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Tensor_shape_types.size = v.size;
    Tensor_shape_types.name = v.name;
  } : Tensor_shape_types.tensor_shape_proto_dim)

let rec decode_tensor_shape_proto d =
  let v = default_tensor_shape_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.dim <- List.rev v.dim;
    ); continue__ := false
    | Some (2, Pbrt.Bytes) -> begin
      v.dim <- (decode_tensor_shape_proto_dim (Pbrt.Decoder.nested d)) :: v.dim;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.unknown_rank <- Pbrt.Decoder.bool d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_shape_proto), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Tensor_shape_types.dim = v.dim;
    Tensor_shape_types.unknown_rank = v.unknown_rank;
  } : Tensor_shape_types.tensor_shape_proto)

let rec encode_tensor_shape_proto_dim (v:Tensor_shape_types.tensor_shape_proto_dim) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int64_as_varint v.Tensor_shape_types.size encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Tensor_shape_types.name encoder;
  ()

let rec encode_tensor_shape_proto (v:Tensor_shape_types.tensor_shape_proto) encoder = 
  List.iter (fun x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_shape_proto_dim x) encoder;
  ) v.Tensor_shape_types.dim;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.bool v.Tensor_shape_types.unknown_rank encoder;
  ()
