open Stream_file
open Sbx_block

let (<+>) = Int64.add;;

let (<->) = Int64.sub;;

let (<*>) = Int64.mul;;

let (</>) = Int64.div;;

module Stats = struct
  type t = { bytes_processed       : int64
           ; blocks_processed      : int64
           ; meta_blocks_processed : int64
           ; data_blocks_processed : int64
           ; start_time            : float
           }

  let make_blank_stats () : t =
    { bytes_processed       = 0L
    ; blocks_processed      = 0L
    ; meta_blocks_processed = 0L
    ; data_blocks_processed = 0L
    ; start_time            = Sys.time ()
    }
  ;;

  let add_bytes (stats:t) ~(num:int64) : t =
    { bytes_processed       = stats.bytes_processed       <+> num
    ; blocks_processed      = stats.blocks_processed
    ; meta_blocks_processed = stats.meta_blocks_processed
    ; data_blocks_processed = stats.data_blocks_processed
    ; start_time            = stats.start_time
    }
  ;;

  let add_meta_block (stats:t) : t =
    { bytes_processed       = stats.bytes_processed
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_processed = stats.meta_blocks_processed <+> 1L
    ; data_blocks_processed = stats.data_blocks_processed
    ; start_time            = stats.start_time
    }
  ;;

  let add_data_block (stats:t) : t =
    { bytes_processed       = stats.bytes_processed
    ; blocks_processed      = stats.blocks_processed      <+> 1L
    ; meta_blocks_processed = stats.meta_blocks_processed
    ; data_blocks_processed = stats.data_blocks_processed <+> 1L
    ; start_time            = stats.start_time
    }
  ;;

  (* automatically correct bytes_processed alignment
   * by rounding down to closest 128 bytes
   *)
  let make_stats (bytes_processed:int64) (blocks_processed:int64) (meta_blocks_processed:int64) (data_blocks_processed:int64) : t =
    { bytes_processed =
        begin
          let alignment = Int64.of_int Param.Rescue.scan_alignment in
          (bytes_processed </> alignment ) <*> alignment
        end
    ; blocks_processed
    ; meta_blocks_processed
    ; data_blocks_processed
    ; start_time = Sys.time ()
    }
  ;;

  let print_stats (stats:t) : unit =
    Printf.printf "Number of          bytes  processed : %Ld\n" stats.bytes_processed;
    Printf.printf "Number of          blocks processed : %Ld\n" stats.blocks_processed;
    Printf.printf "Number of metadata blocks processed : %Ld\n" stats.meta_blocks_processed;
    Printf.printf "Number of data     blocks processed : %Ld\n" stats.data_blocks_processed;
    let (hour, minute, second) = Progress_report.seconds_to_hms (int_of_float (Sys.time() -. stats.start_time)) in
    Printf.printf "Time elapsed                        : %02d:%02d:%02d\n" hour minute second
  ;;

  (*let print_stats_single_line (stats:t) : unit =
    Printf.printf "\rBytes : %Ld, Blocks : %Ld, Meta : %Ld, Data : %Ld"
      stats.bytes_processed
      stats.blocks_processed
      stats.meta_blocks_processed
      stats.data_blocks_processed
  ;;*)
end

type stats = Stats.t

module Progress : sig
  val report_rescue : stats -> in_channel -> unit

end = struct

  let print_rescue_progress_helper =
    let header         = "Data rescue progress" in
    let unit           = "bytes" in
    let print_interval = Param.Rescue.progress_report_interval in
    Progress_report.gen_print_generic ~header ~unit ~print_interval
  ;;

  let print_rescue_progress ~(stats:stats) ~(total_bytes:int64) =
    print_rescue_progress_helper
      ~start_time:stats.start_time
      ~units_so_far:stats.bytes_processed
      ~total_units:total_bytes
  ;;

  let report_rescue : stats -> in_channel -> unit =
    let first_time = ref true in
    (fun stats in_file ->
       let total_bytes =
         (LargeFile.in_channel_length in_file) in
       if !first_time then
         begin
           (* print a notice *)
           Printf.printf "Press Ctrl-C to interrupt\n";
           first_time := false
         end;
       print_rescue_progress ~stats ~total_bytes
    )
  ;;
end

module Logger = struct
  let make_write_proc ~(stats:stats) : unit Stream.out_processor =
    (fun out_file ->
       let open Write_chunk in
       write out_file ~chunk:(Printf.sprintf "bytes_processed=%Ld\n"       stats.bytes_processed);
       write out_file ~chunk:(Printf.sprintf "blocks_processed=%Ld\n"      stats.blocks_processed);
       write out_file ~chunk:(Printf.sprintf "meta_blocks_processed=%Ld\n" stats.meta_blocks_processed);
       write out_file ~chunk:(Printf.sprintf "data_blocks_processed=%Ld\n" stats.data_blocks_processed);
    )
  ;;

  let write_helper ~(stats:stats) ~(log_filename:string) : (unit, string) result =
    let processor = make_write_proc ~stats in
    let write_helper_internal_w_exn () =
      match Stream.process_out ~pack_break_into_error:false ~append:false ~out_filename:log_filename processor with
      | Ok _      -> Ok ()
      | Error msg -> Error msg in
    let write_helper_internal_no_exn () =
      match Stream.process_out                              ~append:false ~out_filename:log_filename processor with
      | Ok _      -> Ok ()
      | Error msg -> Error msg in
    (* This is to make sure log writing is still done even when Ctrl-C is entered
     *
     * This probably will not stop extremely frequent Ctrl-C presses where Break
     * exception is raised during the exception handling bit (maybe? Not sure about this really)
     *
     * But should be good enough for normal actual human uses
     *)
    try
      write_helper_internal_w_exn ()
    with
    | Sys.Break ->
      begin
        write_helper_internal_no_exn () |> ignore; 
        raise Sys.Break (* raise Sys.Break again so the interrupt still stops the process *)
      end
  ;;

  let write =
    let write_interval  : float     = Param.Rescue.log_write_interval in
    let last_write_time : float ref = ref 0. in
    (fun ~(stats:stats) ~(log_filename:string) ~(in_file:in_channel) : bool ->
       let cur_time : float = Sys.time () in
       let time_since_last_write : float = cur_time -. !last_write_time in
       let total_bytes = LargeFile.in_channel_length in_file in
       if time_since_last_write > write_interval || stats.bytes_processed = total_bytes (* always write when 100% done *) then
         begin
           last_write_time := cur_time;
           match write_helper ~stats ~log_filename with
           | Error msg -> Printf.printf "%s\n" msg; false
           | Ok _      -> true
         end
       else
         true (* things are okay and do nothing *)
    )
  ;;

  module Parser = struct
    open Angstrom
    open Parser_components

    let bytes_processed_p =
      string "bytes_processed=" *> integer64 <* string "\n"
    ;;

    let blocks_processed_p =
      string "blocks_processed=" *> integer64 <* string "\n"
    ;;

    let meta_blocks_processed_p =
      string "meta_blocks_processed=" *> integer64 <* string "\n"
    ;;

    let data_blocks_processed_p =
      string "data_blocks_processed=" *> integer64 <* string "\n"
    ;;

    let log_file_p =
      lift4 Stats.make_stats bytes_processed_p blocks_processed_p meta_blocks_processed_p data_blocks_processed_p
    ;;
  end

  let make_read_proc () : (stats option) Stream.in_processor =
    let open Read_chunk in
    let try_parse_len = 1000  (* grab some bytes and try to parse, ignore rest of the log file *) in
    (fun in_file ->
       match read in_file ~len:try_parse_len with
       | None         -> None
       | Some { chunk } ->
         let open Angstrom in
         match parse_only Parser.log_file_p (`String chunk) with
         | Ok stats -> Some stats
         | Error _  -> None
    )
  ;;

  let read ~(log_filename:string) : (stats option, string) result =
    let processor = make_read_proc () in
    if Sys.file_exists log_filename then
      match Stream.process_in ~in_filename:log_filename processor with
      | Ok v      -> Ok v
      | Error msg -> Error msg
    else
      Ok (Some (Stats.make_blank_stats ()))
  ;;
end

module Processor = struct
  (* scan for valid block *)
  let scan_proc ~(stats:stats) ~(log_filename:string option) (in_file:in_channel) : stats * ((Block.t * bytes) option) =
    let open Read_chunk in
    let len = Param.Rescue.scan_alignment in
    let rec scan_proc_internal (stats:stats) : stats * ((Block.t * bytes) option) =
      (* report progress *)
      Progress.report_rescue stats in_file;
      let log_okay : bool =
        match log_filename with
        | None              -> true
        | Some log_filename -> Logger.write ~stats ~log_filename ~in_file in
      if not log_okay then
        begin
          (* just print and quit if cannot write log *)
          print_newline ();
          Printf.printf "Failed to write to log file";
          print_newline ();
          (stats, None)
        end
      else
        begin
          match read in_file ~len with
          | None           -> (stats, None)
          | Some { chunk } ->
            let new_stats =
              Stats.add_bytes stats ~num:(Int64.of_int (Bytes.length chunk)) in
            if Bytes.length chunk < 16 then
              (new_stats, None)  (* no more bytes left in file *)
            else
              let test_header_bytes = Misc_utils.get_bytes chunk ~pos:0 ~len:16 in
              let test_header : Header.raw_header option =
                try
                  Some (Header.of_bytes test_header_bytes)
                with
                | Header.Invalid_bytes -> None in
              match test_header with
              | None            -> scan_proc_internal new_stats
              | Some raw_header ->
                (* possibly grab more bytes depending on version *)
                let chunk =
                  Processor_components.patch_block_bytes_if_needed in_file ~raw_header ~chunk in
                let test_block : Block.t option =
                  Processor_components.bytes_to_block ~raw_header chunk in
                let new_stats =
                  Stats.add_bytes stats ~num:(Int64.of_int (Bytes.length chunk)) in
                match test_block with
                | None       -> scan_proc_internal new_stats
                | Some block -> (new_stats, Some (block, chunk))  (* found a valid block *)
        end in
    scan_proc_internal stats
  ;;

  (* append blocks to filename (use uid in hex string as filename)
   * return Error if failed to write for whatever reason
   *)
  let output_proc ~(stats:stats) ~(block_and_chunk:Block.t * bytes) ~(out_dirname:string) : stats * ((unit, string) result) =
    let (block, chunk) = block_and_chunk in
    let out_filename =
      let uid_hex =
        Conv_utils.bytes_to_hex_string (Block.block_to_file_uid block) in
      Misc_utils.make_path [out_dirname; uid_hex] in
    let output_proc_internal_processor (out_file:out_channel) : unit =
      let open Write_chunk in
      write out_file ~chunk in  (* use the actual bytes in original file rather than generating from scratch *)
    let new_stats =
      if Block.is_meta block then
        Stats.add_meta_block stats
      else
        Stats.add_data_block stats in
    match Stream.process_out ~append:true ~out_filename output_proc_internal_processor with
    | Ok _      -> (new_stats, Ok ())
    | Error msg -> (new_stats, Error msg)
  ;;

  (* if there is any error with outputting, just print directly and return stats
   * this should be very rare however, if happening at all
   *)
  let rec scan_and_output ~(stats:stats) ~(out_dirname:string) ~(log_filename:string option) (in_file:in_channel) : stats =
    match scan_proc ~stats ~log_filename in_file with
    | (stats, None)                 -> stats  (* ran out of valid blocks in input file *)
    | (stats, Some block_and_chunk) ->
      match output_proc ~stats ~block_and_chunk ~out_dirname with
      | (stats, Ok _ )     -> scan_and_output ~stats ~out_dirname ~log_filename in_file
      | (stats, Error msg) -> Printf.printf "%s\n" msg; stats
  ;;

  let make_rescuer ~(out_dirname:string) ~(log_filename:string option) : ((stats, string) result) Stream.in_processor =
    (fun in_file ->
       (* try to get last stats from log file and seek to the last position recorded
        * otherwise just make blank stats
        *)
       let possibly_stats : (stats, string) result  =
         match log_filename with
         | None      -> Ok (Stats.make_blank_stats ())
         | Some log_filename ->
           match Logger.read ~log_filename with
           | Error msg       -> Error msg
           | Ok None         -> Error "Failed to parse log file"
           | Ok (Some stats) -> Ok stats in
       match possibly_stats with
       | Error msg -> Error msg (* just exit due to error *)
       | Ok stats  ->
         (* seek to last position read *)
         LargeFile.seek_in in_file stats.bytes_processed;
         (* start scan and output process *)
         Ok (scan_and_output in_file ~stats ~out_dirname ~log_filename)
    )
  ;;

end

module Process = struct
  let rescue_from_file ~(in_filename:string) ~(out_dirname:string) ~(log_filename:string option) : (stats, string) result =
    let processor = Processor.make_rescuer ~out_dirname ~log_filename in
    match Stream.process_in ~in_filename processor with
    | Ok stats  -> stats
    | Error msg -> Error msg
  ;;
end
