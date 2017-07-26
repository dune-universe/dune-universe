open Cmdliner
open Show
open Stdint
open Sbx_block

exception Packaged_exn of string

let string_option_to_string (x:'a option) : string =
  match x with
  | Some n -> n
  | None   -> "N/A"
;;

let uint64_option_to_string (x:'a option) : string =
  match x with
  | Some n -> Printf.sprintf "%Ld" (Uint64.to_int64 n)
  | None   -> "N/A"
;;

let uint64_seconds_option_to_string (x:uint64 option) (mode:Conv_utils.date_time_mode): string =
  match x with
  | Some n -> Conv_utils.uint64_seconds_to_date_time_string n mode
  | None   -> "N/A"
;;

let print_meta (block:Block.t) : unit =
  if Block.is_data block then
    assert false
  else
    let open Metadata in
    let uid : string  = Conv_utils.bytes_to_hex_string (Block.block_to_file_uid block) in
    let metadata_list = dedup (Block.block_to_meta block) in
    let fnm : string option =
      match Misc_utils.list_find_option (function | FNM _ -> true | _ -> false) metadata_list with
      | Some (FNM v) -> Some v
      | None         -> None
      | _            -> assert false in
    let snm : string option =
      match Misc_utils.list_find_option (function | SNM _ -> true | _ -> false) metadata_list with
      | Some (SNM v) -> Some v
      | None         -> None
      | _            -> assert false in
    let fsz : uint64 option =
      match Misc_utils.list_find_option (function | FSZ _ -> true | _ -> false) metadata_list with
      | Some (FSZ v) -> Some v
      | None         -> None
      | _            -> assert false in
    let fdt : uint64 option =
      match Misc_utils.list_find_option (function | FDT _ -> true | _ -> false) metadata_list with
      | Some (FDT v) -> Some v
      | None         -> None
      | _            -> assert false in
    let sdt : uint64 option =
      match Misc_utils.list_find_option (function | SDT _ -> true | _ -> false) metadata_list with
      | Some (SDT v) -> Some v
      | None         -> None
      | _            -> assert false in
    let hsh : string option =
      match Misc_utils.list_find_option (function | HSH _ -> true | _ -> false) metadata_list with
      | Some (HSH v) ->
        Some (Printf.sprintf "%s - %s"
                (Multihash.hash_bytes_to_hash_type_string v)
                (Conv_utils.bytes_to_hex_string (Multihash.hash_bytes_to_raw_hash v))
             )
      | None         -> None
      | _            -> assert false in
    Printf.printf "File UID                     : %s\n"
      uid;
    Printf.printf "File name                    : %s\n"
      (string_option_to_string fnm);
    Printf.printf "Sbx container name           : %s\n"
      (string_option_to_string snm);
    Printf.printf "File size                    : %s\n"
      (uint64_option_to_string fsz);
    Printf.printf "File modification time UTC   : %s\n"
      (uint64_seconds_option_to_string fdt `UTC);
    Printf.printf "Sbx encoding time      UTC   : %s\n"
      (uint64_seconds_option_to_string sdt `UTC);
    Printf.printf "File modification time local : %s\n"
      (uint64_seconds_option_to_string fdt `Local);
    Printf.printf "Sbx encoding time      local : %s\n"
      (uint64_seconds_option_to_string sdt `Local);
    Printf.printf "Hash                         : %s\n"
      (string_option_to_string hsh)
;;

let rec print_meta_blocks ?(cur:int = 0) (lst:Block.t list) : unit =
  match lst with
  | []      -> ()
  | b :: bs ->
    begin
      Printf.printf "Metadata block number : %d\n" cur;
      Printf.printf "========================================\n";
      print_meta b;
      print_newline ();
      print_meta_blocks ~cur:(cur + 1) bs
    end
;;

let show (find_all:bool) (in_filename:string) : unit =
  try
    if not find_all then
      match Process.fetch_single_meta ~in_filename with
      | Ok res    ->
        begin
          match res with
          | Some block -> print_meta block
          | None       -> Printf.printf "No metadata blocks found\n"
        end
      | Error str -> raise (Packaged_exn str)
    else
      match Process.fetch_multi_meta ~in_filename with
      | Ok res    ->
        begin
          match res with
          | []         -> Printf.printf "No metadata blocks found\n"
          | lst        ->
            begin
              Printf.printf "Showing first up to %Ld metadata blocks\n" Param.Show.meta_list_max_length;
              print_newline ();
              print_meta_blocks lst
            end
        end
      | Error str -> raise (Packaged_exn str)
  with
  | Packaged_exn str -> Printf.printf "%s\n" str
;;

let find_all =
  let doc = "Try to find all metadata blocks" in
  Arg.(value & flag & info ["find-all"] ~doc)
;;

let in_file =
  let doc = "Sbx container to seach for metadata" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;
