open Stdint
open Sbx_specs
open Sbx_block
open Stream_file
open Multihash

exception File_metadata_get_failed

let (<+>) = Int64.add;;

(*let (<->) = Int64.sub;;

let (<*>) = Int64.mul;;*)

module Stats = struct
  type t = { block_size          : int
           ; data_size           : int
           ; blocks_written      : int64
           ; meta_blocks_written : int64
           ; data_blocks_written : int64
           ; total_data_encoded  : int64
           ; start_time          : float 
           }

  let make_blank_stats ~(ver:version) : t =
    { block_size          = ver_to_block_size ver
    ; data_size           = ver_to_data_size  ver
    ; blocks_written      = 0L
    ; meta_blocks_written = 0L
    ; data_blocks_written = 0L
    ; total_data_encoded  = 0L
    ; start_time          = Sys.time ()
    }

  let add_written_meta_block (stats:t) : t =
    { block_size          = stats.block_size
    ; data_size           = stats.data_size
    ; blocks_written      = stats.blocks_written      <+> 1L
    ; meta_blocks_written = stats.meta_blocks_written <+> 1L
    ; data_blocks_written = stats.data_blocks_written
    ; total_data_encoded  = stats.total_data_encoded
    ; start_time          = stats.start_time
    }

  let add_written_data_block (stats:t) ~(data_len:int) : t =
    { block_size          = stats.block_size
    ; data_size           = stats.data_size
    ; blocks_written      = stats.blocks_written      <+> 1L
    ; meta_blocks_written = stats.meta_blocks_written
    ; data_blocks_written = stats.data_blocks_written <+> 1L
    ; total_data_encoded  = stats.total_data_encoded  <+> (Int64.of_int data_len)
    ; start_time          = stats.start_time
    }

  let print_stats (stats:t) : unit =
    Printf.printf "Block size used in encoding       : %d\n"  stats.block_size;
    Printf.printf "Data  size used in encoding       : %d\n"  stats.data_size;
    Printf.printf "Number of          blocks written : %Ld\n" stats.blocks_written;
    Printf.printf "Number of metadata blocks written : %Ld\n" stats.meta_blocks_written;
    Printf.printf "Number of data     blocks written : %Ld\n" stats.data_blocks_written;
    Printf.printf "Amount of data encoded (in bytes) : %Ld\n" stats.total_data_encoded;
    let (hour, minute, second) = Progress_report.seconds_to_hms (int_of_float (Sys.time() -. stats.start_time)) in
    Printf.printf "Time elapsed                      : %02d:%02d:%02d\n" hour minute second
  ;;

end

type stats = Stats.t

module Progress : sig
  val report_encode : stats -> in_channel -> unit

end = struct

  let print_encode_progress_helper =
    let header         = "Data encoding progress" in
    let unit           = "chunks" in
    let print_interval = Param.Encode.progress_report_interval in
    Progress_report.gen_print_generic ~header ~unit ~print_interval
  ;;

  let print_encode_progress ~(stats:stats) ~(total_chunks:int64) =
    print_encode_progress_helper
      ~start_time:stats.start_time
      ~units_so_far:stats.blocks_written
      ~total_units:total_chunks
  ;;

  let report_encode : stats -> in_channel -> unit  =
    let first_time    = ref true in
    (fun stats in_file ->
       let data_size    : int64 =
         Int64.of_int stats.data_size in
       let total_chunks : int64 =
         Int64.div
           (Int64.add (LargeFile.in_channel_length in_file) (Int64.sub data_size 1L))
           data_size (* use of data_size is correct here *) in
       if !first_time then
         begin
           (* print a notice *)
           Printf.printf "Only data blocks are reported in the progress reporting below\n";
           first_time := false
         end;
       print_encode_progress ~stats ~total_chunks;
    )
  ;;
end

module Processor = struct
  (* Converts data to data blocks *)
  let rec data_to_block_proc (in_file:in_channel) (out_file:out_channel) ~(data_len:int) ~(stats:stats) ~(common:Header.common_fields) : stats =
    let open Read_chunk in
    let open Write_chunk in
    (* report progress *)
    Progress.report_encode stats in_file;
    match read in_file ~len:data_len with
    | None           -> stats
    | Some { chunk } ->
      let chunk_len   = Bytes.length chunk in
      let seq_num     = Uint32.of_int64 (stats.data_blocks_written <+> 1L) in (* always off by +1 *)
      let block       = Block.make_data_block ~seq_num common ~data:chunk in
      let block_bytes = Block.to_bytes block in
      (* write to file *)
      write out_file ~chunk:block_bytes;
      data_to_block_proc in_file out_file ~data_len ~stats:(Stats.add_written_data_block stats ~data_len:chunk_len) ~common
  ;;

  let data_to_block_proc_w_hash (hash_type:hash_type) (in_file:in_channel) (out_file:out_channel) ~(data_len:int) ~(stats: stats) ~(common:Header.common_fields) : stats * hash_bytes =
    let rec data_to_block_proc_w_hash_internal (hash_state:Hash.ctx) (in_file:in_channel) (out_file:out_channel) ~(data_len:int) ~(stats:stats) ~(common:Header.common_fields) : stats * hash_bytes =
      let open Read_chunk in
      let open Write_chunk in
      (* report progress *)
      Progress.report_encode stats in_file;
      match read in_file ~len:data_len with
      | None           -> (stats, Hash.get_hash_bytes hash_state)
      | Some { chunk } ->
        let chunk_len   = Bytes.length chunk in
        let seq_num     = Uint32.of_int64 (stats.data_blocks_written <+> 1L) in (* always off by +1 *)
        let block       = Block.make_data_block ~seq_num common ~data:chunk in
        let block_bytes = Block.to_bytes block in
        (* update hash *)
        Hash.feed hash_state chunk;
        (* write to file *)
        write out_file ~chunk:block_bytes;
        data_to_block_proc_w_hash_internal hash_state in_file out_file ~data_len ~stats:(Stats.add_written_data_block stats ~data_len:chunk_len) ~common in
    data_to_block_proc_w_hash_internal (Hash.init hash_type) in_file out_file ~data_len ~stats ~common
  ;;

  let make_in_out_encoder ~(hash_type:hash_type) ~(common:Header.common_fields) ~(metadata:(Metadata.t list) option) : stats Stream.in_out_processor =
    let ver      = Header.common_fields_to_ver common in
    let data_len = ver_to_data_size ver in
    let open Read_chunk in
    let open Write_chunk in
    match metadata with
    | None ->
      (fun in_file out_file ->
         data_to_block_proc in_file out_file ~data_len ~stats:(Stats.make_blank_stats ~ver) ~common
      )
    | Some metadata_list ->
      (fun in_file out_file ->
         try
           (* write a empty metadata block first to shift space and also to test length of metadata fields *)
           let open Metadata in
           let fields_except_hash =
             List.filter (function | HSH _ -> false | _ -> true) metadata_list in
           (* a dummy multihash is added to make sure there is actually enough space
            * in the metadata block before the encoding starts
            *)
           let dummy_hash_bytes           = Multihash.make_dummy_hash_bytes hash_type in
           let dummy_fields               = (HSH dummy_hash_bytes) :: fields_except_hash in
           let dummy_metadata_block       = Block.make_metadata_block common ~fields:dummy_fields in
           let dummy_metadata_block_bytes = Block.to_bytes dummy_metadata_block in
           write out_file ~chunk:dummy_metadata_block_bytes;
           (* write data blocks *)
           let (stats, hash_bytes)        =
             data_to_block_proc_w_hash hash_type in_file out_file ~data_len ~stats:(Stats.make_blank_stats ~ver) ~common in
           let fields                     =
             (HSH hash_bytes) :: fields_except_hash in
           let metadata_block             = Block.make_metadata_block common ~fields in
           let metadata_block_bytes       = Block.to_bytes metadata_block in
           (* go back and write metadata block *)
           LargeFile.seek_out out_file 0L;
           write out_file ~chunk:metadata_block_bytes;
           (* update stats *)
           Stats.add_written_meta_block stats
         with
         | Metadata.Too_much_data msg
         | Metadata.Invalid_entry msg -> raise (Packaged_exn msg)
      )
  ;;
end

module Process = struct
  let get_file_metadata ~(in_filename:string) ~(out_filename:string) : Metadata.t list =
    try
      let open Metadata in
      let open File_utils in
      let open Time_utils in
      [ FNM (Misc_utils.path_to_file in_filename)
      ; SNM (Misc_utils.path_to_file out_filename)
      ; FSZ (getsize_uint64  ~filename:in_filename)
      ; FDT (getmtime_uint64 ~filename:in_filename)
      ; SDT (gettime_uint64 ())
      ]
    with
    | _ -> raise File_metadata_get_failed
  ;;

  let encode_file ~(uid:bytes option) ~(want_meta:bool) ~(ver:version) ~(hash:string) ~(in_filename:string) ~(out_filename:string) : (stats, string) result =
    try
      (* check file size first *)
      let max_file_size = ver_to_max_file_size ver in
      let file_size     = File_utils.getsize ~filename:in_filename in
      if file_size > max_file_size then
        Error (Printf.sprintf "File size (%Ld bytes) exceeds upper limit (%Ld bytes)" file_size max_file_size)
      else
        (* check hash *)
        match string_to_hash_type hash with
        | Error msg    -> Error msg
        | Ok hash_type ->
          if not (Hash.hash_type_is_supported hash_type) then
            Error "Hash type is not supported"
          else
            begin
              let common   =
                match uid with
                | Some uid -> Header.make_common_fields ~uid ver
                | None     -> Header.make_common_fields      ver in
              let metadata =
                match want_meta with
                | true  -> Some (get_file_metadata ~in_filename ~out_filename)
                | false -> None in
              let encoder  = Processor.make_in_out_encoder ~hash_type ~common ~metadata in
              Stream.process_in_out ~append:false ~in_filename ~out_filename encoder
            end
    with
    | File_utils.File_access_error        -> Error "Failed to access file"
    | File_metadata_get_failed            -> Error "Failed to get file metadata"
    | Sbx_block.Header.Invalid_uid_length -> Error "Invalid uid length"
  ;;
end
