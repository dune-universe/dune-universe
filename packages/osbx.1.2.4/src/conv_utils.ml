open Stdint

type date_time_mode = [ `UTC | `Local ]

type case = [ `Upper | `Lower ]

let uint64_to_string (v:uint64) : string =
  let buf = Bytes.create 8 in
  Uint64.to_bytes_big_endian v buf 0;
  Bytes.to_string buf
;;

let uint32_to_string (v:uint32) : string =
  let buf = Bytes.create 4 in
  Uint32.to_bytes_big_endian v buf 0;
  Bytes.to_string buf
;;

let uint16_to_string (v:uint16) : string =
  let buf = Bytes.create 2 in
  Uint16.to_bytes_big_endian v buf 0;
  Bytes.to_string buf
;;

let uint8_to_string (v:uint8) : string =
  let buf = Bytes.create 1 in
  Uint8.to_bytes_big_endian v buf 0;
  Bytes.to_string buf
;;

let string_to_hex_string ~(case:case) (data:string) : string =
  let `Hex str = Hex.of_string data in
  match case with
  | `Upper -> String.uppercase_ascii str
  | `Lower -> String.lowercase_ascii str
;;

let string_to_hex_string_uid  (data:string) : string =
  string_to_hex_string ~case:`Upper data
;;

let string_to_hex_string_hash (data:string) : string =
  string_to_hex_string ~case:`Lower data
;;

let hex_string_to_string (str:string) : (string, string) result =
  try
    let hex = `Hex str in
    Ok (Hex.to_string hex)
  with
  | Invalid_argument _ -> Error "Invalid hex string"
;;

(*let sha256_hash_state_to_string (hash_state:SHA256.t) : bytes =
  Cstruct.to_string (SHA256.get hash_state)
  ;;
*)

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
