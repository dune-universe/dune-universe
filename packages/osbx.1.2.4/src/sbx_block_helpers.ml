open Sbx_block

let block_to_filename (block:Block.t) : string option =
  let metadata_list  = Metadata.dedup (Block.block_to_meta block) in
  match List.filter (function | Metadata.FNM _ -> true | _ -> false) metadata_list with
  | [ ]       -> None
  | [FNM str] -> Some str
  | [_]       -> assert false
  | _ :: _    -> assert false
;;

let try_block_to_filename (block:Block.t option) : string option =
  match block with
  | Some block -> block_to_filename block
  | None       -> None
;;

let block_type_to_raw_header_pred (block_type:Block.block_type) : (Header.raw_header -> bool) =
  match block_type with
  | `Meta -> Header.raw_header_is_meta
  | `Data -> Header.raw_header_is_data
  | `Any  -> (fun _ -> true)
;;

let file_uid_to_raw_header_pred (uid:string option) : (Header.raw_header -> bool) =
  match uid with
  | Some uid -> (fun header -> header.file_uid = uid)
  | None     -> (fun _ -> true)
;;
