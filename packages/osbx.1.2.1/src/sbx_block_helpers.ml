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
