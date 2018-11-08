[@@@ocaml.warning "-27-30-39"]

type tensor_proto_mutable = {
  mutable dtype : Types_types.data_type;
  mutable tensor_shape : Tensor_shape_types.tensor_shape_proto option;
  mutable version_number : int32;
  mutable tensor_content : bytes;
  mutable half_val : int32 list;
  mutable float_val : float list;
  mutable double_val : float list;
  mutable int_val : int32 list;
  mutable string_val : bytes list;
  mutable scomplex_val : float list;
  mutable int64_val : int64 list;
  mutable bool_val : bool list;
  mutable dcomplex_val : float list;
  mutable resource_handle_val : Resource_handle_types.resource_handle_proto list;
  mutable variant_val : Tensor_types.variant_tensor_data_proto list;
  mutable uint32_val : int32 list;
  mutable uint64_val : int64 list;
}

let default_tensor_proto_mutable () : tensor_proto_mutable = {
  dtype = Types_types.default_data_type ();
  tensor_shape = None;
  version_number = 0l;
  tensor_content = Bytes.create 0;
  half_val = [];
  float_val = [];
  double_val = [];
  int_val = [];
  string_val = [];
  scomplex_val = [];
  int64_val = [];
  bool_val = [];
  dcomplex_val = [];
  resource_handle_val = [];
  variant_val = [];
  uint32_val = [];
  uint64_val = [];
}

type variant_tensor_data_proto_mutable = {
  mutable type_name : string;
  mutable metadata : bytes;
  mutable tensors : Tensor_types.tensor_proto list;
}

let default_variant_tensor_data_proto_mutable () : variant_tensor_data_proto_mutable = {
  type_name = "";
  metadata = Bytes.create 0;
  tensors = [];
}


let rec decode_tensor_proto d =
  let v = default_tensor_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.uint64_val <- List.rev v.uint64_val;
      v.uint32_val <- List.rev v.uint32_val;
      v.variant_val <- List.rev v.variant_val;
      v.resource_handle_val <- List.rev v.resource_handle_val;
      v.dcomplex_val <- List.rev v.dcomplex_val;
      v.bool_val <- List.rev v.bool_val;
      v.int64_val <- List.rev v.int64_val;
      v.scomplex_val <- List.rev v.scomplex_val;
      v.string_val <- List.rev v.string_val;
      v.int_val <- List.rev v.int_val;
      v.double_val <- List.rev v.double_val;
      v.float_val <- List.rev v.float_val;
      v.half_val <- List.rev v.half_val;
    ); continue__ := false
    | Some (1, Pbrt.Varint) -> begin
      v.dtype <- Types_pb.decode_data_type d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.tensor_shape <- Some (Tensor_shape_pb.decode_tensor_shape_proto (Pbrt.Decoder.nested d));
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(2)" pk
    | Some (3, Pbrt.Varint) -> begin
      v.version_number <- Pbrt.Decoder.int32_as_varint d;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(3)" pk
    | Some (4, Pbrt.Bytes) -> begin
      v.tensor_content <- Pbrt.Decoder.bytes d;
    end
    | Some (4, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(4)" pk
    | Some (13, Pbrt.Bytes) -> begin
      v.half_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int32_as_varint d)::l) [] d;
    end
    | Some (13, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(13)" pk
    | Some (5, Pbrt.Bytes) -> begin
      v.float_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits32 d)::l) [] d;
    end
    | Some (5, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(5)" pk
    | Some (6, Pbrt.Bytes) -> begin
      v.double_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (6, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(6)" pk
    | Some (7, Pbrt.Bytes) -> begin
      v.int_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int32_as_varint d)::l) [] d;
    end
    | Some (7, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(7)" pk
    | Some (8, Pbrt.Bytes) -> begin
      v.string_val <- (Pbrt.Decoder.bytes d) :: v.string_val;
    end
    | Some (8, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(8)" pk
    | Some (9, Pbrt.Bytes) -> begin
      v.scomplex_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits32 d)::l) [] d;
    end
    | Some (9, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(9)" pk
    | Some (10, Pbrt.Bytes) -> begin
      v.int64_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (10, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(10)" pk
    | Some (11, Pbrt.Bytes) -> begin
      v.bool_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.bool d)::l) [] d;
    end
    | Some (11, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(11)" pk
    | Some (12, Pbrt.Bytes) -> begin
      v.dcomplex_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.float_as_bits64 d)::l) [] d;
    end
    | Some (12, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(12)" pk
    | Some (14, Pbrt.Bytes) -> begin
      v.resource_handle_val <- (Resource_handle_pb.decode_resource_handle_proto (Pbrt.Decoder.nested d)) :: v.resource_handle_val;
    end
    | Some (14, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(14)" pk
    | Some (15, Pbrt.Bytes) -> begin
      v.variant_val <- (decode_variant_tensor_data_proto (Pbrt.Decoder.nested d)) :: v.variant_val;
    end
    | Some (15, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(15)" pk
    | Some (16, Pbrt.Bytes) -> begin
      v.uint32_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int32_as_varint d)::l) [] d;
    end
    | Some (16, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(16)" pk
    | Some (17, Pbrt.Bytes) -> begin
      v.uint64_val <- Pbrt.Decoder.packed_fold (fun l d -> (Pbrt.Decoder.int64_as_varint d)::l) [] d;
    end
    | Some (17, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(tensor_proto), field(17)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Tensor_types.dtype = v.dtype;
    Tensor_types.tensor_shape = v.tensor_shape;
    Tensor_types.version_number = v.version_number;
    Tensor_types.tensor_content = v.tensor_content;
    Tensor_types.half_val = v.half_val;
    Tensor_types.float_val = v.float_val;
    Tensor_types.double_val = v.double_val;
    Tensor_types.int_val = v.int_val;
    Tensor_types.string_val = v.string_val;
    Tensor_types.scomplex_val = v.scomplex_val;
    Tensor_types.int64_val = v.int64_val;
    Tensor_types.bool_val = v.bool_val;
    Tensor_types.dcomplex_val = v.dcomplex_val;
    Tensor_types.resource_handle_val = v.resource_handle_val;
    Tensor_types.variant_val = v.variant_val;
    Tensor_types.uint32_val = v.uint32_val;
    Tensor_types.uint64_val = v.uint64_val;
  } : Tensor_types.tensor_proto)

and decode_variant_tensor_data_proto d =
  let v = default_variant_tensor_data_proto_mutable () in
  let continue__= ref true in
  while !continue__ do
    match Pbrt.Decoder.key d with
    | None -> (
      v.tensors <- List.rev v.tensors;
    ); continue__ := false
    | Some (1, Pbrt.Bytes) -> begin
      v.type_name <- Pbrt.Decoder.string d;
    end
    | Some (1, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(variant_tensor_data_proto), field(1)" pk
    | Some (2, Pbrt.Bytes) -> begin
      v.metadata <- Pbrt.Decoder.bytes d;
    end
    | Some (2, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(variant_tensor_data_proto), field(2)" pk
    | Some (3, Pbrt.Bytes) -> begin
      v.tensors <- (decode_tensor_proto (Pbrt.Decoder.nested d)) :: v.tensors;
    end
    | Some (3, pk) -> 
      Pbrt.Decoder.unexpected_payload "Message(variant_tensor_data_proto), field(3)" pk
    | Some (_, payload_kind) -> Pbrt.Decoder.skip d payload_kind
  done;
  ({
    Tensor_types.type_name = v.type_name;
    Tensor_types.metadata = v.metadata;
    Tensor_types.tensors = v.tensors;
  } : Tensor_types.variant_tensor_data_proto)

let rec encode_tensor_proto (v:Tensor_types.tensor_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Varint) encoder; 
  Types_pb.encode_data_type v.Tensor_types.dtype encoder;
  begin match v.Tensor_types.tensor_shape with
  | Some x -> 
    Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Tensor_shape_pb.encode_tensor_shape_proto x) encoder;
  | None -> ();
  end;
  Pbrt.Encoder.key (3, Pbrt.Varint) encoder; 
  Pbrt.Encoder.int32_as_varint v.Tensor_types.version_number encoder;
  Pbrt.Encoder.key (4, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Tensor_types.tensor_content encoder;
  Pbrt.Encoder.key (13, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int32_as_varint x encoder;
    ) v.Tensor_types.half_val;
  ) encoder;
  Pbrt.Encoder.key (5, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits32 x encoder;
    ) v.Tensor_types.float_val;
  ) encoder;
  Pbrt.Encoder.key (6, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) v.Tensor_types.double_val;
  ) encoder;
  Pbrt.Encoder.key (7, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int32_as_varint x encoder;
    ) v.Tensor_types.int_val;
  ) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (8, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.bytes x encoder;
  ) v.Tensor_types.string_val;
  Pbrt.Encoder.key (9, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits32 x encoder;
    ) v.Tensor_types.scomplex_val;
  ) encoder;
  Pbrt.Encoder.key (10, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Tensor_types.int64_val;
  ) encoder;
  Pbrt.Encoder.key (11, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.bool x encoder;
    ) v.Tensor_types.bool_val;
  ) encoder;
  Pbrt.Encoder.key (12, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.float_as_bits64 x encoder;
    ) v.Tensor_types.dcomplex_val;
  ) encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (14, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (Resource_handle_pb.encode_resource_handle_proto x) encoder;
  ) v.Tensor_types.resource_handle_val;
  List.iter (fun x -> 
    Pbrt.Encoder.key (15, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_variant_tensor_data_proto x) encoder;
  ) v.Tensor_types.variant_val;
  Pbrt.Encoder.key (16, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int32_as_varint x encoder;
    ) v.Tensor_types.uint32_val;
  ) encoder;
  Pbrt.Encoder.key (17, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.nested (fun encoder ->
    List.iter (fun x -> 
      Pbrt.Encoder.int64_as_varint x encoder;
    ) v.Tensor_types.uint64_val;
  ) encoder;
  ()

and encode_variant_tensor_data_proto (v:Tensor_types.variant_tensor_data_proto) encoder = 
  Pbrt.Encoder.key (1, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.string v.Tensor_types.type_name encoder;
  Pbrt.Encoder.key (2, Pbrt.Bytes) encoder; 
  Pbrt.Encoder.bytes v.Tensor_types.metadata encoder;
  List.iter (fun x -> 
    Pbrt.Encoder.key (3, Pbrt.Bytes) encoder; 
    Pbrt.Encoder.nested (encode_tensor_proto x) encoder;
  ) v.Tensor_types.tensors;
  ()
