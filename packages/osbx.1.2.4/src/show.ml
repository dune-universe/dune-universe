open Sbx_block
open Stream_file
open Int64_ops

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

module Progress = struct
  let { print_progress            = report_scan
      ; print_newline_if_not_done = report_scan_print_newline_if_not_done
      }
    : (unit, stats, int64) Progress_report.progress_print_functions =
    Progress_report.gen_print_generic
      ~header:"Scan progress"
      ~silence_settings:Dynamic_param.Common.silence_settings
      ~display_while_active:Progress_report_param.Show.Show_progress.display_while_active
      ~display_on_finish:Progress_report_param.Show.Show_progress.display_on_finish
      ~display_on_finish_early:Progress_report_param.Show.Show_progress.display_on_finish_early
      ~unit:"bytes"
      ~print_interval:Progress_report_param.Show.progress_report_interval
      ~eval_start_time:Sys.time
      ~eval_units_so_far:(fun (stats:stats) -> stats.bytes_processed)
      ~eval_total_units:(fun x -> x)
  ;;
end

module Processor = struct
  let find_meta_blocks_proc ~(from_byte:int64 option) ~(to_byte:int64 option) ~(force_misalign:bool) ~(get_at_most:int64) (in_file:in_channel) : (Block.t * int64) list =
    let open Misc_utils in
    let get_at_most          = ensure_at_least ~at_least:0L get_at_most in
    let last_possible_pos    = Int64.pred (LargeFile.in_channel_length in_file) in
    let { required_len; seek_to } =
      calc_required_len_and_seek_to_from_byte_range ~from_byte ~to_byte ~force_misalign ~bytes_so_far:0L ~last_possible_pos in
    let raw_header_pred      = Header.raw_header_is_meta in
    let rec find_meta_blocks_proc_internal (stats:stats) (acc:(Block.t * int64) list) : stats * ((Block.t * int64) list) =
      (* report progress *)
      Progress.report_scan ~start_time_src:() ~units_so_far_src:stats ~total_units_src:required_len;
      if (stats.meta_blocks_found >= get_at_most)
      || (stats.bytes_processed   >= required_len)
      then
        (stats, acc)
      else
        let (read_len, block) = Processor_components.try_get_block_from_in_channel ~raw_header_pred in_file in
        if read_len = 0L then
          (stats, acc)
        else
          let new_stats            = Stats.add_bytes_scanned stats ~num:read_len in
          let (new_stats, new_acc) =
            match block with
            | Some block ->
              if Block.is_meta block then
                (Stats.add_meta_block new_stats, (block, stats.bytes_processed <+> seek_to) :: acc)
              else
                (new_stats, acc)
            | None       -> (new_stats, acc) in
          find_meta_blocks_proc_internal new_stats new_acc in
    let start_stats  = Stats.make_blank_scan_stats () in
    LargeFile.seek_in in_file seek_to;
    let (stats, res) = find_meta_blocks_proc_internal start_stats [] in
    Progress.report_scan_print_newline_if_not_done ~start_time_src:() ~units_so_far_src:stats ~total_units_src:required_len;
    res
  ;;

  let make_single_meta_fetcher ~(from_byte:int64 option) ~(to_byte:int64 option) ~(force_misalign:bool) : ((Block.t * int64) option) Stream.in_processor =
    (fun in_file ->
       match find_meta_blocks_proc ~from_byte ~to_byte ~force_misalign ~get_at_most:1L in_file with
       | []     -> None
       | [x]    -> Some x
       | _ :: _ -> assert false
    )
  ;;

  let make_multi_meta_fetcher ~(from_byte:int64 option) ~(to_byte:int64 option) ~(force_misalign:bool) : ((Block.t * int64) list) Stream.in_processor =
    (fun in_file ->
       List.rev (find_meta_blocks_proc ~from_byte ~to_byte ~force_misalign ~get_at_most:!Dynamic_param.Show.meta_list_max_length in_file)
    )
  ;;
end

module Process = struct
  let fetch_single_meta ~(from_byte:int64 option) ~(to_byte:int64 option) ~(force_misalign:bool) ~(in_filename:string) : ((Block.t * int64) option, string) result =
    let processor = Processor.make_single_meta_fetcher ~from_byte ~to_byte ~force_misalign in
    Stream.process_in ~in_filename processor
  ;;

  let fetch_multi_meta ~(from_byte:int64 option) ~(to_byte:int64 option) ~(force_misalign:bool) ~(in_filename:string) : ((Block.t * int64) list, string) result =
    let processor = Processor.make_multi_meta_fetcher  ~from_byte ~to_byte ~force_misalign in
    Stream.process_in ~in_filename processor
  ;;
end
