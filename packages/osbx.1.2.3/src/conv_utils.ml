open Stdint
open Nocrypto.Hash

type date_time_mode = [ `UTC | `Local ]

type case = [ `Upper | `Lower ]

let uint64_to_bytes (v:uint64) : bytes =
  let buf = Bytes.create 8 in
  Uint64.to_bytes_big_endian v buf 0;
  buf
;;

let uint32_to_bytes (v:uint32) : bytes =
  let buf = Bytes.create 4 in
  Uint32.to_bytes_big_endian v buf 0;
  buf
;;

let uint16_to_bytes (v:uint16) : bytes =
  let buf = Bytes.create 2 in
  Uint16.to_bytes_big_endian v buf 0;
  buf
;;

let uint8_to_bytes (v:uint8) : bytes =
  let buf = Bytes.create 1 in
  Uint8.to_bytes_big_endian v buf 0;
  buf
;;

let string_to_bytes (str:string) : bytes =
  Bytes.of_string str
;;

let bytes_to_hex_string ~(case:case) (data:bytes) : string =
  let `Hex str = Hex.of_cstruct (Cstruct.of_bytes data) in
  match case with
  | `Upper -> String.uppercase_ascii str
  | `Lower -> String.lowercase_ascii str
;;

let bytes_to_hex_string_uid  (data:bytes) : string =
  bytes_to_hex_string ~case:`Upper data
;;

let bytes_to_hex_string_hash (data:bytes) : string =
  bytes_to_hex_string ~case:`Lower data
;;

let hex_string_to_bytes (str:string) : (bytes, string) result =
  let open Hex in
  try
    let hex = `Hex str in
    Ok (Hex.to_string hex)
  with
  | Invalid_argument _ -> Error "Invalid hex string"
;;

let sha256_hash_state_to_bytes (hash_state:SHA256.t) : bytes =
  Cstruct.to_string (SHA256.get hash_state)
;;

let uint64_seconds_to_date_time_string (seconds:uint64) (mode:date_time_mode) : string =
  let seconds = Uint64.to_float seconds in
  let time =
    match mode with
    | `UTC   -> Unix.gmtime    seconds
    | `Local -> Unix.localtime seconds in
  let { tm_sec  = seconds
      ; tm_min  = minutes
      ; tm_hour = hours
      ; tm_mday = day
      ; tm_mon  = month
      ; tm_year = year
      ; _
      } : Unix.tm = time in
  Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
    (year  + 1900)
    (month + 1)
    day
    hours
    minutes
    seconds
;;
