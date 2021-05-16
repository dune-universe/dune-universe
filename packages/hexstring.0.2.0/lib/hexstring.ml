(* encoding bytearray -> hexstring *)

let encode (bytearray : bytes) : string =
  let rec encode_rec fold bytearray =
    let length = Bytes.length bytearray in
    if length = 0 then fold
    else
      let c = Bytes.get bytearray 0 in
      let encoding = int_of_char c |> Format.sprintf "%02x" in
      let fold = fold ^ encoding in
      let bytearray = Bytes.sub bytearray 1 (length - 1) in
      encode_rec fold bytearray
  in
  encode_rec "" bytearray

let%test "encoding" =
  let bytearray = Bytes.of_string "\x01\x02\x03\x04" in
  let hexstring = encode bytearray in
  hexstring = "01020304"

(* helper to decode a byte *)

let decode_1char = function
  | '0' -> Ok 0
  | '1' -> Ok 1
  | '2' -> Ok 2
  | '3' -> Ok 3
  | '4' -> Ok 4
  | '5' -> Ok 5
  | '6' -> Ok 6
  | '7' -> Ok 7
  | '8' -> Ok 8
  | '9' -> Ok 9
  | 'a' -> Ok 10
  | 'b' -> Ok 11
  | 'c' -> Ok 12
  | 'd' -> Ok 13
  | 'e' -> Ok 14
  | 'f' -> Ok 15
  | _ -> Error "invalid character"

let decode_2chars ((c1, c2) : char * char) : (char, string) result =
  let fst = decode_1char c1 in
  let snd = decode_1char c2 in
  match (fst, snd) with
  | Error _, _ | _, Error _ -> Error "nope"
  | Ok fst, Ok snd ->
      let res = (fst lsl 4) lxor snd in
      Ok (char_of_int res)

let%test "decoding two chars 01" =
  let d = decode_2chars ('0', '1') in
  d = Ok '\x01'

let%test "decoding two chars ff" =
  let d = decode_2chars ('f', 'f') in
  d = Ok '\xff'

let%test "decoding two erroneous chars" =
  let d = decode_2chars ('z', 'f') in
  Result.is_error d

(* decoding hexstring -> bytearray *)

let rec parse_string (res : bytes) (offset : int) (ss : string) =
  match String.length ss with
  | 0 -> Ok res
  | _ -> (
      let byte = Str.first_chars ss 2 in
      let rest = Str.string_after ss 2 in
      let c1 = byte.[0] in
      let c2 = byte.[1] in
      match decode_2chars (c1, c2) with
      | Error err -> Error err
      | Ok c ->
          Bytes.set res offset c;
          parse_string res (offset + 1) rest)

let decode hexstring =
  match String.length hexstring with
  | len when len mod 2 <> 0 -> Error "length must be a multiple of 2"
  | len ->
      let res = Bytes.make (len / 2) '\x00' in
      parse_string res 0 hexstring

let%test "decoding empty hexstring" =
  let d = decode "" in
  d = Ok Bytes.empty

let%test "decoding bad-length hexstring" =
  let d = decode "1" in
  Result.is_error d

let%test "decoding bad-char hexstring" =
  let d = decode "120z" in
  Result.is_error d

let%test "decoding valid hexstring" =
  let d = decode "01020304" in
  d = Ok (Bytes.of_string "\x01\x02\x03\x04")
