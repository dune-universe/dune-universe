open Sbx_block
open Stream_file
open Sbx_specs

let bytes_to_block ?(raw_header:Header.raw_header option) (chunk:bytes) : Block.t option =
  try
    Some (Block.of_bytes ?raw_header chunk)
  with
  | Header.Invalid_bytes
  | Metadata.Invalid_bytes
  | Block.Invalid_bytes
  | Block.Invalid_size     -> None
;;

let patch_block_bytes_if_needed (in_file:in_channel) ~(raw_header:Header.raw_header) ~(chunk:bytes) : bytes =
  let open Read_chunk in
  let ideal_len   = ver_to_block_size raw_header.version in
  let missing_len = ideal_len - (Bytes.length chunk) in
  if missing_len > 0 then
    match read in_file ~len:missing_len with
    | None                           -> chunk (* can't do anything, just give back the original piece *)
    | Some { chunk = missing_chunk } -> Bytes.concat "" [chunk; missing_chunk]
  else
    chunk
;;

let try_get_block_and_bytes_from_in_channel ?(fixed_ver:Sbx_specs.version option) ?(raw_header_pred:(Header.raw_header -> bool) = (fun _ -> true)) (in_file:in_channel) : int64 * ((Block.t * bytes) option) =
  let open Read_chunk in
  let len = 
    match fixed_ver with
    | Some v -> Sbx_specs.ver_to_block_size v
    | None   -> Param.Common.block_scan_alignment in
  match read in_file ~len with
  | None           -> (0L, None)
  | Some { chunk } ->
    let read_len : int64 = Int64.of_int (Bytes.length chunk) in
    try
      let raw_header_bytes : bytes             = Misc_utils.get_bytes chunk ~pos:0 ~len:16 in
      let raw_header       : Header.raw_header = Header.of_bytes raw_header_bytes in
      if raw_header_pred raw_header then
        let chunk            : bytes                    =
          match fixed_ver with
          | Some _ -> chunk
          | None   -> patch_block_bytes_if_needed in_file ~raw_header ~chunk in
        let read_len         : int64                    = Int64.of_int (Bytes.length chunk) in
        let block            : Block.t option           = bytes_to_block ~raw_header chunk in
        let block_and_bytes  : (Block.t * bytes) option =
          match block with
          | None   -> None
          | Some b -> Some (b, chunk) in
        (read_len, block_and_bytes)
      else
        (read_len, None)
    with
    | Misc_utils.Invalid_range -> (read_len, None)
    | Header.Invalid_bytes     -> (read_len, None)
;;

let try_get_block_from_in_channel ?(fixed_ver:Sbx_specs.version option) ?(raw_header_pred:(Header.raw_header -> bool) option) (in_file:in_channel) : int64 * (Block.t option) =
  let (read_len, block_and_bytes) = try_get_block_and_bytes_from_in_channel ?fixed_ver ?raw_header_pred in_file in
  let block =
    match block_and_bytes with
    | None        -> None
    | Some (b, _) -> Some b in
  (read_len, block)
;;
