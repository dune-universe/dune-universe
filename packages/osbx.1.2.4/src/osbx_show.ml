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

let print_meta ((block, pos):Block.t * int64) : unit =
  if Block.is_data block then
    assert false
  else
    let open Metadata in
    let uid : string  = Conv_utils.string_to_hex_string_uid (Block.block_to_file_uid block) in
    let ver : string  = Sbx_specs.ver_to_human_string (Block.block_to_ver block) in
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
                (Conv_utils.string_to_hex_string_hash (Multihash.hash_bytes_to_raw_hash v))
             )
      | None         -> None
      | _            -> assert false in
    Printf.printf "Found at byte          : %Ld - 0x%LX\n"
      pos pos;
    print_newline ();
    Printf.printf "File UID               : %s\n"
      uid;
    Printf.printf "File name              : %s\n"
      (string_option_to_string fnm);
    Printf.printf "Sbx container name     : %s\n"
      (string_option_to_string snm);
    Printf.printf "Sbx container version  : %s\n"
      ver;
    Printf.printf "File size              : %s\n"
      (uint64_option_to_string fsz);
    Printf.printf "File modification time : %s (UTC)  %s (Local)\n"
      (uint64_seconds_option_to_string fdt `UTC)
      (uint64_seconds_option_to_string fdt `Local);
    Printf.printf "Sbx encoding time      : %s (UTC)  %s (Local)\n"
      (uint64_seconds_option_to_string sdt `UTC)
      (uint64_seconds_option_to_string sdt `Local);
    Printf.printf "Hash                   : %s\n"
      (string_option_to_string hsh)
;;

let rec print_meta_blocks ?(cur:int64 = 0L) (lst:(Block.t * int64) list) : unit =
  match lst with
  | []              -> ()
  | b_and_pos :: tl ->
    begin
      Printf.printf "Metadata block number : %Ld\n" cur;
      Printf.printf "========================================\n";
      print_meta b_and_pos;
      print_newline ();
      print_meta_blocks ~cur:(Int64.succ cur) tl
    end
;;

let show (silent:Progress_report.silence_level) (find_max:int64 option) (from_byte:int64 option) (to_byte:int64 option) (force_misalign:bool) (in_filename:string) : unit =
  Dynamic_param.Show.set_meta_list_max_length_possibly find_max;
  Dynamic_param.Common.set_silence_settings silent;
  try
    match find_max with
    | Some n when n <= 0L ->
      ()
    | None                ->
      begin
        match Process.fetch_single_meta ~from_byte ~to_byte ~force_misalign ~in_filename with
        | Ok res    ->
          begin
            match res with
            | Some x -> print_meta x
            | None   -> Printf.printf "No metadata blocks found\n"
          end
        | Error str -> raise (Packaged_exn str)
      end
    | _                   ->
      match Process.fetch_multi_meta ~from_byte ~to_byte ~force_misalign ~in_filename with
      | Ok res    ->
        begin
          match res with
          | []         -> Printf.printf "No metadata blocks found\n"
          | lst        ->
            begin
              Printf.printf "Showing first up to %Ld metadata blocks\n" !Dynamic_param.Show.meta_list_max_length;
              print_newline ();
              print_meta_blocks lst
            end
        end
      | Error str -> raise (Packaged_exn str)
  with
  | Packaged_exn str -> Printf.printf "%s\n" str
;;

let find_max =
  let doc = "Find first up to $(docv)(defaults to 1) metadata blocks.
  If the default is used(this option not specified), total block number and block number indicators are not shown.
  If a number is provided, then all the indicators are shown, regardless of the value of $(docv).
  Negative values are treated as 0." in
  Arg.(value & opt (some int64) None & info ["find-max"] ~docv:"FIND-MAX" ~doc)
;;

let in_file =
  let doc = "Sbx container to seach for metadata" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;
