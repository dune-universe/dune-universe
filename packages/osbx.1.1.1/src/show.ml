open Sbx_block
open Stream_file

let (<+>) = Int64.add;;

let (<->) = Int64.sub;;

let (<*>) = Int64.mul;;

module Stats = struct
  type t = { bytes_processed   : int64
           ; meta_blocks_found : int64
           ; start_time        : float
           }

  let add_bytes_scanned (stats:t) ~(num:int64) : t =
    { bytes_processed   = stats.bytes_processed <+> num
    ; meta_blocks_found = stats.meta_blocks_found
    ; start_time        = stats.start_time
    }
  ;;

  let add_meta_block (stats:t) : t =
    { bytes_processed   = stats.bytes_processed
    ; meta_blocks_found = stats.meta_blocks_found <+> 1L
    ; start_time        = stats.start_time
    }
  ;;

  let make_blank_scan_stats () : t =
    { bytes_processed   = 0L
    ; meta_blocks_found = 0L
    ; start_time        = Sys.time ()
    }
  ;;
end

type stats = Stats.t

module Progress : sig
  val report_scan                 : stats -> in_channel -> unit

  val print_newline_possibly_scan : stats -> in_channel -> unit

end = struct

  let print_scan_progress_helper =
    let header         = "Scan progress" in
    let unit           = "bytes" in
    let print_interval = Param.Show.progress_report_interval in
    Progress_report.gen_print_generic ~header ~unit ~print_interval
  ;;

  let print_scan_progress ~(stats:stats) ~(total_bytes:int64) =
    print_scan_progress_helper
      ~start_time:stats.start_time
      ~units_so_far:stats.bytes_processed
      ~total_units:total_bytes
  ;;

  let report_scan : stats -> in_channel -> unit =
    (fun stats in_file ->
       let total_bytes =
         LargeFile.in_channel_length in_file in
       print_scan_progress ~stats ~total_bytes
    )
  ;;

  let print_newline_possibly_scan (stats:stats) (in_file:in_channel) : unit =
    Progress_report.print_newline_if_not_done
      ~units_so_far:stats.bytes_processed
      ~total_units:(LargeFile.in_channel_length in_file)
  ;;
end

module Processor = struct
  let find_meta_blocks_proc ~(get_at_most:int64) (in_file:in_channel) : Block.t list =
    let open Read_chunk in
    let len = Param.Decode.ref_block_scan_alignment in
    let rec find_meta_blocks_proc_internal (stats:stats) (acc:Block.t list) : stats * (Block.t list) =
      (* report progress *)
      Progress.report_scan stats in_file;
      if stats.meta_blocks_found >= get_at_most then
        (stats, acc)
      else
        match read in_file ~len with
        | None           -> (stats, acc)
        | Some { chunk } ->
          if Bytes.length chunk < 16 then
            (stats, acc)
          else
          let test_header_bytes = Misc_utils.get_bytes chunk ~pos:0 ~len:16 in
          let test_header : Header.raw_header option =
            try
              Some (Header.of_bytes test_header_bytes)
            with
            | Header.Invalid_bytes -> None in
          match test_header with
          | None            -> 
            let new_stats =
              Stats.add_bytes_scanned stats ~num:(Int64.of_int (Bytes.length chunk)) in
            find_meta_blocks_proc_internal new_stats acc (* go to next block *)
          | Some raw_header ->
            (* possibly grab more bytes depending on version *)
            let chunk =
              Processor_components.patch_block_bytes_if_needed in_file ~raw_header ~chunk in
            let test_block : Block.t option =
              Processor_components.bytes_to_block ~raw_header chunk in
            let new_stats =
              Stats.add_bytes_scanned stats ~num:(Int64.of_int (Bytes.length chunk)) in
            let (new_stats, new_acc) =
              match test_block with
              | None       -> (new_stats, acc)
              | Some block ->
                if Block.is_meta block then
                  (Stats.add_meta_block new_stats, block :: acc)
                else
                  (new_stats, acc) in
            find_meta_blocks_proc_internal new_stats new_acc in
    let (stats, res) = find_meta_blocks_proc_internal (Stats.make_blank_scan_stats ()) [] in
    Progress.print_newline_possibly_scan stats in_file;
    res
  ;;

  let single_meta_fetcher (in_file:in_channel) : Block.t option =
    match find_meta_blocks_proc ~get_at_most:1L in_file with
    | []       -> None
    | [x]      -> Some x
    | hd :: tl -> assert false
  ;;

  (* return up to 100 metadata blocks found *)
  let multi_meta_fetcher (in_file:in_channel) : Block.t list =
    List.rev (find_meta_blocks_proc ~get_at_most:Param.Show.meta_list_max_length in_file)
  ;;
end

module Process = struct
  let fetch_single_meta ~(in_filename:string) : (Block.t option, string) result =
    Stream.process_in ~in_filename Processor.single_meta_fetcher
  ;;

  let fetch_multi_meta ~(in_filename:string) : (Block.t list, string) result =
    Stream.process_in ~in_filename Processor.multi_meta_fetcher
  ;;
end
