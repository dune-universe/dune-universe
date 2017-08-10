open Stdint
open Sbx_specs
open Sbx_block
open Stream_file

let (<+>) = Int64.add;;

let (<->) = Int64.sub;;

let (<*>) = Int64.mul;;

module Stats = struct
  type t = { block_size            : int
           ; blocks_processed      : int64
           ; meta_blocks_decoded   : int64
           ; data_blocks_decoded   : int64
           ; blocks_failed         : int64
           ; failed_block_pos_list : int64 list
           ; recorded_hash         : Multihash.hash_bytes option
           ; output_file_hash      : Multihash.hash_bytes option
           ; start_time            : float
           }

  type scan_stats = { bytes_processed : int64
                    ; start_time      : float
                    }

  type hash_stats = { bytes_processed : int64
                    ; start_time      : float
                    }

  let make_blank_stats ~(ver:version) : t =
    { block_size            = ver_to_block_size ver
    ; blocks_processed      = 0L
    ; meta_blocks_decoded   = 0L
    ; data_blocks_decoded   = 0L
    ; blocks_failed         = 0L
    ; failed_block_pos_list = []
    ; recorded_hash         = None
    ; output_file_hash      = None
    ; start_time            = Sys.time ()
    }
  ;;

  let add_okay_meta_block (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_decoded   = stats.meta_blocks_decoded   <+> 1L
    ; data_blocks_decoded   = stats.data_blocks_decoded
    ; blocks_failed         = stats.blocks_failed
    ; failed_block_pos_list = stats.failed_block_pos_list
    ; recorded_hash         = stats.recorded_hash
    ; output_file_hash      = stats.output_file_hash
    ; start_time            = stats.start_time
    }
  ;;

  let add_okay_data_block (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_decoded   = stats.meta_blocks_decoded
    ; data_blocks_decoded   = stats.data_blocks_decoded   <+> 1L
    ; blocks_failed         = stats.blocks_failed
    ; failed_block_pos_list = stats.failed_block_pos_list
    ; recorded_hash         = stats.recorded_hash
    ; output_file_hash      = stats.output_file_hash
    ; start_time            = stats.start_time
    }
  ;;

  let add_failed_block (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_decoded   = stats.meta_blocks_decoded
    ; data_blocks_decoded   = stats.data_blocks_decoded
    ; blocks_failed         = stats.blocks_failed         <+> 1L
    ; failed_block_pos_list =
        if stats.blocks_failed < !Param.Decode.failure_list_max_length then
          stats.blocks_processed :: stats.failed_block_pos_list
        else
          stats.failed_block_pos_list
    ; recorded_hash         = stats.recorded_hash
    ; output_file_hash      = stats.output_file_hash
    ; start_time            = stats.start_time
    }
  ;;

  let add_hashes ~(recorded_hash:Multihash.hash_bytes option) ~(output_file_hash:Multihash.hash_bytes option) (stats:t) : t =
    { block_size            = stats.block_size
    ; blocks_processed      = stats.blocks_processed
    ; meta_blocks_decoded   = stats.meta_blocks_decoded
    ; data_blocks_decoded   = stats.data_blocks_decoded
    ; blocks_failed         = stats.blocks_failed
    ; failed_block_pos_list = stats.failed_block_pos_list
    ; recorded_hash         =
        begin
          match (stats.recorded_hash, recorded_hash) with
          | (Some hsh, Some _)   -> Some hsh
          | (None,     Some hsh) -> Some hsh
          | (Some hsh, None)     -> Some hsh
          | (None,     None)     -> None
        end
    ; output_file_hash      =
        begin
          match (stats.output_file_hash, output_file_hash) with
          | (Some hsh, Some _)   -> Some hsh
          | (None,     Some hsh) -> Some hsh
          | (Some hsh, None)     -> Some hsh
          | (None,     None)     -> None
        end
    ; start_time            = stats.start_time
    }
  ;;

  let make_blank_scan_stats () : scan_stats =
    { bytes_processed = 0L
    ; start_time      = Sys.time ()
    }
  ;;

  let add_bytes_scanned (stats:scan_stats) ~(num:int64) : scan_stats =
    { bytes_processed = stats.bytes_processed <+> num
    ; start_time      = stats.start_time
    }
  ;;

  let make_blank_hash_stats () : hash_stats =
    { bytes_processed = 0L
    ; start_time      = Sys.time ()
    }
  ;;

  let add_bytes_hashed (stats:hash_stats) ~(num:int64) : hash_stats =
    { bytes_processed = stats.bytes_processed <+> num
    ; start_time      = stats.start_time
    }
  ;;

  let print_failed_pos (block_size:int) (pos_list:int64 list) : unit =
    let block_size = Int64.of_int block_size in
    List.iter (fun x -> Printf.printf "Failed to decode block %Ld, at byte %Ld\n" x (block_size <*> x)) (List.rev pos_list)
  ;;

  let print_stats (stats:t) : unit =
    Printf.printf "Block size used in decoding                      : %d\n"  stats.block_size;
    Printf.printf "Number of blocks processed                       : %Ld\n" stats.blocks_processed;
    Printf.printf "Number of blocks successfully decoded (metadata) : %Ld\n" stats.meta_blocks_decoded;
    Printf.printf "Number of blocks successfully decoded (data)     : %Ld\n" stats.data_blocks_decoded;
    Printf.printf "Number of blocks failed to decode                : %Ld\n" stats.blocks_failed;
    let (hour, minute, second) = Progress_report.Helper.seconds_to_hms (int_of_float (Sys.time() -. stats.start_time)) in
    Printf.printf "Time elapsed                                     : %02d:%02d:%02d\n" hour minute second;
    Printf.printf "Recorded hash                                    : %s\n"
      (match stats.recorded_hash with
       | Some hsh ->
         Printf.sprintf "%s - %s"
           (Multihash.hash_bytes_to_hash_type_string hsh)
           (Conv_utils.bytes_to_hex_string (Multihash.hash_bytes_to_raw_hash hsh))
       | None     ->
         "N/A"
      );
    Printf.printf "Hash of the output file                          : %s\n"
      (match (stats.output_file_hash, stats.recorded_hash) with
       | (Some hsh, _     )     ->
         Printf.sprintf "%s - %s"
           (Multihash.hash_bytes_to_hash_type_string hsh)
           (Conv_utils.bytes_to_hex_string (Multihash.hash_bytes_to_raw_hash hsh))
       | (None    , Some _)     ->
         "N/A - recorded hash type is not supported by osbx"
       | (None    , None  )     ->
         "N/A"
      );
    if stats.meta_blocks_decoded = 0L then
      begin
        print_newline ();
        Printf.printf "Warning : No metadata blocks were found in the sbx container\n";
        Printf.printf "          It is likely that the output file is not of the correct size\n";
        Printf.printf "          and has data padding bytes attached at the end of it\n";
        print_newline ();
      end;
    begin
      match (stats.recorded_hash, stats.output_file_hash) with
      | (Some recorded_hash, Some output_file_hash) ->
        if Multihash.hash_bytes_equal recorded_hash output_file_hash then
          Printf.printf "The output file hash matches the recorded hash\n"
        else
          Printf.printf "The output file hash does NOT match the recorded hash\n"
      | (Some _,             None)                  ->
        Printf.printf "No hash is available for output file\n"
      | (None,               Some _)                ->
        Printf.printf "No recorded hash is available\n"
      | (None,               None)                  ->
        Printf.printf "Neither recorded hash nor output file hash is available\n";
    end;
    Printf.printf "First up to %Ld failing positions (block and bytes index start at 0)\n" !Param.Decode.failure_list_max_length;
    print_failed_pos stats.block_size stats.failed_block_pos_list
  ;;
end

type stats      = Stats.t

type scan_stats = Stats.scan_stats

type hash_stats = Stats.hash_stats

module Progress = struct
  let { print_progress            = report_scan
      ; print_newline_if_not_done = report_scan_print_newline_if_not_done
      }
    : (unit, scan_stats, in_channel) Progress_report.progress_print_functions =
    Progress_report.gen_print_generic
      ~header:"Scan progress"
      ~silence_settings:Param.Common.silence_settings
      ~display_while_active:Param.Decode.Scan_progress.display_while_active
      ~display_on_finish:Param.Decode.Scan_progress.display_on_finish
      ~display_on_finish_early:Param.Decode.Scan_progress.display_on_finish_early
      ~unit:"bytes"
      ~print_interval:Param.Decode.progress_report_interval
      ~eval_start_time:Sys.time
      ~eval_units_so_far:(fun (stats:scan_stats) -> stats.bytes_processed)
      ~eval_total_units:(fun in_file -> LargeFile.in_channel_length in_file)
  ;;

  let { print_progress            = report_hash
      ; print_newline_if_not_done = report_hash_print_newline_if_not_done
      }
    : (unit, hash_stats, in_channel) Progress_report.progress_print_functions =
    Progress_report.gen_print_generic
      ~header:"Hash progress"
      ~silence_settings:Param.Common.silence_settings
      ~display_while_active:Param.Decode.Hash_progress.display_while_active
      ~display_on_finish:Param.Decode.Hash_progress.display_on_finish
      ~display_on_finish_early:Param.Decode.Hash_progress.display_on_finish_early
      ~unit:"bytes"
      ~print_interval:Param.Decode.progress_report_interval
      ~eval_start_time:Sys.time
      ~eval_units_so_far:(fun (stats:hash_stats) -> stats.bytes_processed)
      ~eval_total_units:(fun in_file -> LargeFile.in_channel_length in_file)
  ;;

  let { print_progress            = report_decode
      ; print_newline_if_not_done = report_decode_print_newline_if_not_done
      }
    : (unit, stats, stats * in_channel) Progress_report.progress_print_functions =
    Progress_report.gen_print_generic
      ~header:"Decode progress"
      ~silence_settings:Param.Common.silence_settings
      ~display_while_active:Param.Decode.Decode_progress.display_while_active
      ~display_on_finish:Param.Decode.Decode_progress.display_on_finish
      ~display_on_finish_early:Param.Decode.Decode_progress.display_on_finish_early
      ~unit:"blocks"
      ~print_interval:Param.Decode.progress_report_interval
      ~eval_start_time:Sys.time
      ~eval_units_so_far:(fun (stats:stats) -> stats.blocks_processed)
      ~eval_total_units:
        (fun (stats, in_file) ->
           let block_size =
             Int64.of_int stats.Stats.block_size
           and total_file_size =
             LargeFile.in_channel_length in_file in
           Int64.div total_file_size block_size
        )
  ;;
end

module Processor = struct
  type find_both_result = { meta : Block.t option
                          ; data : Block.t option
                          }

  let find_first_both_proc ~(prefer:Block.block_type) (in_file:in_channel) : find_both_result =
    let open Read_chunk in
    let rec find_first_both_proc_internal (result_so_far:find_both_result) (stats:scan_stats) : scan_stats * find_both_result =
      (* report progress *)
      Progress.report_scan ~start_time_src:() ~units_so_far_src:stats ~total_units_src:in_file;
      match result_so_far with
      | { meta = Some _; data = Some _ }                     -> (stats, result_so_far)
      | { meta = Some _; data = _ }      when prefer = `Meta -> (stats, result_so_far)
      | { meta = _;      data = Some _ } when prefer = `Data -> (stats, result_so_far)
      | _                                                    ->
        let (read_len, block) = Processor_components.try_get_block_from_in_channel in_file in
        if read_len = 0L then
          (stats, result_so_far)
        else
          let new_stats = Stats.add_bytes_scanned stats ~num:read_len in
          match block with
          | None       -> find_first_both_proc_internal result_so_far new_stats
          | Some block -> 
            let new_result_so_far =
              { meta =
                  begin
                    match result_so_far.meta with
                    | Some _ as x -> x
                    | None        -> if Block.is_meta block then Some block else None
                  end
              ; data =
                  begin
                    match result_so_far.data with
                    | Some _ as x -> x
                    | None        -> if Block.is_data block then Some block else None
                  end
              } in
            find_first_both_proc_internal new_result_so_far new_stats in
    let (stats, res) = find_first_both_proc_internal { meta = None; data = None } (Stats.make_blank_scan_stats ()) in
    LargeFile.seek_in in_file 0L;  (* reset seek position *)
    Progress.report_scan_print_newline_if_not_done ~start_time_src:() ~units_so_far_src:stats ~total_units_src:in_file;
    res
  ;;

  (* ref_block will be used as reference for version and uid
   *  block must match those two parameters to be accepted
   *)
  let find_valid_data_block_proc ~(ref_block:Block.t) (in_file:in_channel) ~(stats:stats) : stats * (Block.t option) =
    let open Read_chunk in
    let ref_ver        = Block.block_to_ver ref_block in
    let ref_file_uid   = Block.block_to_file_uid ref_block in
    let ref_block_size = ver_to_block_size ref_ver in
    let rec find_valid_data_block_proc_internal (stats:stats) (result_so_far:Block.t option) : stats * Block.t option =
      (* report progress *)
      Progress.report_decode ~start_time_src:() ~units_so_far_src:stats ~total_units_src:(stats, in_file);
      match result_so_far with
      | Some _ as x -> (stats, x)
      | None        ->
        let (read_len, block) = Processor_components.try_get_block_from_in_channel ~fixed_len:ref_block_size in_file in
        if read_len = 0L then
          (stats, result_so_far)
        else
          let (new_stats, new_result) =
            match block with
            | Some block ->
              let file_uid = Block.block_to_file_uid block in
              let ver      = Block.block_to_ver      block in
              if file_uid = ref_file_uid && ver = ref_ver then
                if Block.is_meta block then
                  (Stats.add_okay_meta_block stats, None)       (* do not return metadata block *)
                else
                  (Stats.add_okay_data_block stats, Some block)
              else
                (Stats.add_failed_block stats, None)
            | None       ->
              if read_len = Int64.of_int ref_block_size then
                (Stats.add_failed_block stats, None)
              else
                (* reading in a chunk smaller than a block is not considered to be a failure *)
                (stats,                        None) in
          find_valid_data_block_proc_internal new_stats new_result in
    find_valid_data_block_proc_internal stats None
  ;;

  let output_decoded_data_proc ~(ref_block:Block.t) ~(block:Block.t) (out_file:out_channel) : unit =
    let open Write_chunk in
    if Block.is_meta block then
      ()  (* ignore attempts to write metadata block *)
    else
      (* determine position to write to using reference block and block's sequence number *)
      let ref_ver           = Block.block_to_ver ref_block in
      let data_len  : int64 = Int64.of_int    (ver_to_data_size ref_ver) in
      match Block.block_to_seq_num block with
      | None         -> assert false
      | Some seq_num ->
        let seq_num   : int64 = Uint32.to_int64 seq_num in
        let write_pos : int64 = (seq_num <-> 1L) <*> data_len in  (* seq_num is guaranteed to be > 0 due to above check of is_meta *)
        (* seek to the proper position then write *)
        LargeFile.seek_out out_file write_pos;
        write out_file ~chunk:(Block.block_to_data block)
  ;;

  let decode_and_output_proc ~(ref_block:Block.t) (in_file:in_channel) (out_file:out_channel) : stats =
    let rec decode_and_output_proc_internal (stats:stats) : stats =
      match find_valid_data_block_proc ~ref_block in_file ~stats with
      | (stats, None)       -> stats
      | (stats, Some block) ->
        output_decoded_data_proc ~ref_block ~block out_file;
        decode_and_output_proc_internal stats in
    decode_and_output_proc_internal (Stats.make_blank_stats ~ver:(Block.block_to_ver ref_block))
  ;;

  let ref_block_fetcher (in_file:in_channel) : Block.t option =
    Printf.printf "Scanning for reference block\n";
    match find_first_both_proc ~prefer:`Meta in_file with
    | { meta = Some block; data = _          } ->
      begin
        Printf.printf "Metadata block found\n";
        Some block
      end
    | { meta = None      ; data = Some block } ->
      begin
        Printf.printf "No metadata blocks were found, resorting to data block\n";
        Some block
      end
    | { meta = None      ; data = None       } -> None
  ;;

  let make_decoder ~(ref_block:Block.t option) : (stats * (int64 option)) Stream.in_out_processor =
    (fun in_file out_file ->
       let ref_block : Block.t option =
         match ref_block with
         | Some _ as x -> x
         | None        -> ref_block_fetcher in_file in
       match ref_block with
       | None           -> raise (Packaged_exn "No usable blocks in file")
       | Some ref_block ->
         (* got a reference block, decode all data blocks *)
         let stats = decode_and_output_proc ~ref_block in_file out_file in
         (* if reference block is a metadata block, then use the recorded file size to indicate truncatation *)
         let truncate : int64 option =
           if Block.is_meta ref_block then
             let metadata_list   = Block.block_to_meta ref_block in
             try
               let open Metadata in
               match List.find (function | FSZ _ -> true | _ -> false) metadata_list with
               | FSZ file_size -> Some (Uint64.to_int64 file_size)
               | _             -> None
             with
             | Not_found -> None
           else
             None in
         let recorded_hash : Multihash.hash_bytes option =
           let open Metadata in
           let metadata_list      = dedup (Block.block_to_meta ref_block) in
           try
             match List.find (function | HSH _ -> true | _ -> false) metadata_list with
             | HSH hash_bytes -> Some hash_bytes
             | _              -> None
           with
           | Not_found -> None in
         let stats = Stats.add_hashes ~recorded_hash ~output_file_hash:None stats in
         (stats, truncate)
    )
  ;;

  let make_hasher ~(hash_type:Multihash.hash_type) : bytes Stream.in_processor =
    let read_len = 1024 * 1024 (* 1 MiB *) in
    (fun in_file ->
       let rec hash_proc (stats:hash_stats) (hash_state:Multihash.Hash.ctx) (in_file:in_channel) : bytes =
         let open Read_chunk in
         Progress.report_hash ~start_time_src:() ~units_so_far_src:stats ~total_units_src:in_file;
         match read in_file ~len:read_len with
         | None           -> Multihash.Hash.get_raw_hash hash_state
         | Some { chunk } ->
           Multihash.Hash.feed hash_state chunk;
           let new_stats =
             Stats.add_bytes_hashed stats ~num:(Int64.of_int (Bytes.length chunk)) in
           hash_proc new_stats hash_state in_file in
       let stats      = Stats.make_blank_hash_stats () in
       let hash_state = Multihash.Hash.init hash_type in
       hash_proc stats hash_state in_file
    )
  ;;
end

module Process = struct
  let fetch_ref_block ~(in_filename:string) : (Block.t option, string) result =
    Stream.process_in ~in_filename Processor.ref_block_fetcher
  ;;

  let hash_file ~(hash_type:Multihash.hash_type) ~(in_filename:string) : (bytes, string) result =
    let processor = Processor.make_hasher ~hash_type in
    Stream.process_in ~in_filename processor
  ;;

  let hash_file_w_warning ~(hash_type:Multihash.hash_type) ~(in_filename:string) : bytes option =
    match hash_file ~hash_type ~in_filename with
    | Ok hash   -> Some hash
    | Error msg -> Printf.printf "Warning : %s\n" msg; None
  ;;

  let decode_file ~(ref_block:Block.t option) ~(in_filename:string) ~(out_filename:string option) : (stats, string) result =
    let final_out_filename : string option =
      match out_filename with
      | Some str -> Some str
      | None     -> Sbx_block_helpers.try_block_to_filename ref_block in
    match final_out_filename with
    | None              ->
      Error (Printf.sprintf "Failed to obtain a filename for output(none is provided and no valid metadata block with filename field is found in %s)" in_filename)
    | Some out_filename ->
      let decoder = Processor.make_decoder ~ref_block in
      match Stream.process_in_out ~append:false ~in_filename ~out_filename decoder with
      | Ok (stats, Some trunc_size) ->
        begin
          try
            Unix.LargeFile.truncate out_filename trunc_size;
            match stats.recorded_hash with
            | Some hash_bytes ->
              begin
                let hash_type = Multihash.hash_bytes_to_hash_type hash_bytes in
                if Multihash.Hash.hash_type_is_supported hash_type then
                  let output_file_hash =
                    match hash_file_w_warning ~hash_type ~in_filename:out_filename with
                    | Some raw ->
                      Some (Multihash.raw_hash_to_hash_bytes hash_type raw)
                    | None     -> None in
                  Ok (Stats.add_hashes ~recorded_hash:None ~output_file_hash stats)
                else
                  Ok stats
              end
            | None            ->
              Ok stats
          with
          | Multihash.Length_mismatch -> assert false
          | _                         -> Error "Failed to truncate output file"
        end
      | Ok (stats, None)            -> Ok stats
      | Error msg as em             -> em
  ;;
end
