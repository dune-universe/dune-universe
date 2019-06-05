module Base64 = Nocrypto.Base64

let encode_blob cstruct = Cstruct.to_string @@ Base64.encode cstruct

let encode message = encode_blob @@ Cstruct.of_string message

let decode_as_blob encoded =
  let nullable = Base64.decode @@ Cstruct.of_string encoded in
  match nullable with
  | Some value ->
      value
  | None ->
      failwith "Failed to decode Base 64 blob!"

let decode encoded = Cstruct.to_string @@ decode_as_blob encoded
