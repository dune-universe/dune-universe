open Stdint

type version = [ `V1 | `V2 | `V3 ]

module Common_param = struct
  let file_uid_len   : int    = 6
  let signature      : string = "SBx"
  let header_size    : int    = 16
  let max_blocks_num : int64  = Int64.sub (Int64.shift_left 0x1L 32) 1L  (* 2 ** 32 - 1 *)
end

module Param_for_v1 = struct
  let block_size   : int   = 512
  let data_size    : int   = block_size - Common_param.header_size
end

module Param_for_v2 = struct
  let block_size   : int   = 128
  let data_size    : int   = block_size - Common_param.header_size
end

module Param_for_v3 = struct
  let block_size   : int   = 4096
  let data_size    : int   = block_size - Common_param.header_size
end

module Parser = struct
  open Angstrom

  let v1_p  : version Angstrom.t =
    char '\x01' *> return `V1
  ;;

  let v2_p  : version Angstrom.t =
    char '\x02' *> return `V2
  ;;

  let v3_p  : version Angstrom.t =
    char '\x03' *> return `V3
  ;;

  let ver_p : version Angstrom.t =
    choice [ v1_p
           ; v2_p
           ; v3_p
           ]
  ;;
end

let sbx_file_uid_len = Common_param.file_uid_len;;

let sbx_signature    = Common_param.signature;;

let sbx_header_size  = Common_param.header_size;;

let ver_to_int          (ver:version) : int =
  match ver with
  | `V1 -> 1
  | `V2 -> 2
  | `V3 -> 3
;;

let ver_to_uint8        (ver:version) : uint8 =
  Uint8.of_int (ver_to_int ver)
;;

let ver_to_uint16       (ver:version) : uint16 =
  Uint16.of_int (ver_to_int ver)
;;

let ver_to_string       (ver:version) : string =
  Conv_utils.uint8_to_string (ver_to_uint8 ver)
;;

let ver_to_human_string (ver:version) : string =
  string_of_int (ver_to_int ver)
;;

let ver_to_block_size   (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.block_size
  | `V2 -> Param_for_v2.block_size
  | `V3 -> Param_for_v3.block_size
;;

let ver_to_data_size    (ver:version) : int =
  match ver with
  | `V1 -> Param_for_v1.data_size
  | `V2 -> Param_for_v2.data_size
  | `V3 -> Param_for_v3.data_size
;;

let ver_to_max_file_size (ver:version) : int64 =
  let open Int64 in
  mul (of_int (ver_to_data_size ver)) Common_param.max_blocks_num
;;

let string_to_ver       (str:string)  : (version, string) result =
  match str with
  | "1" -> Ok `V1
  | "2" -> Ok `V2
  | "3" -> Ok `V3
  | _   -> Error "Invalid version string"
;;
