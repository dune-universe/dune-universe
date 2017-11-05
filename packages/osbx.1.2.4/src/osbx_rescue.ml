open Cmdliner
open Rescue

exception Packaged_exn of string

let rescue
    (silent:Progress_report.silence_level)
    (only_pick_block:Sbx_block.Block.block_type)
    (only_pick_uid:string option)
    (from_byte:int64 option)
    (to_byte:int64 option)
    (force_misalign:bool)
    (in_filename:string)
    (out_dirname:string)
    (log_filename:string option)
  : unit =
  Dynamic_param.Common.set_silence_settings silent;
  try
    let only_pick_uid : string option =
      match only_pick_uid with
      | None -> None
      | Some str ->
        match Conv_utils.hex_string_to_string str with
        | Ok uid -> Some uid
        | Error _ -> raise (Packaged_exn (Printf.sprintf "Uid %s is not a valid hex string" str)) in
    match Process.rescue_from_file ~only_pick_block ~only_pick_uid ~from_byte ~to_byte ~force_misalign ~in_filename ~out_dirname ~log_filename with
    | Ok stats  -> Stats.print_stats stats
    | Error msg -> Printf.printf "%s\n" msg
  with 
  | Packaged_exn str -> Printf.printf "%s\n" str
;;

let only_pick_block =
  let doc = "Only pick $(docv) of blocks. $(docv) is one of : any, meta, data." in
  let open Sbx_block.Block in
  Arg.(value
       & opt
         (enum [("any",  (`Any:block_type));
                ("meta", (`Meta:block_type));
                ("data", (`Data:block_type))])
         (`Any:block_type)
       & info
         ["only-pick-block"]
         ~docv:"TYPE"
         ~doc)
;;

let only_pick_uid =
  let doc = "Only pick blocks with $(docv) as uid. Uid must be exactly 6 bytes(12 hex digits) in length." in
  Arg.(value & opt (some string) None & info ["only-pick-uid"] ~docv:"UID-HEX" ~doc)
;;

let in_file =
  let doc = "File to rescue sbx data from" in
  Arg.(required & pos 0 (some non_dir_file) None & info [] ~docv:"INFILE" ~doc)
;;

let out_dir =
  let doc = "Directory to store rescued data" in
  Arg.(required & pos 1 (some dir) None & info [] ~docv:"OUTDIR" ~doc)
;;

let log_file =
  let doc = "Log file to keep track of the progress to survive interruptions.
  Note that you should use the same log file for the same file and range specified in the initial run." in
  Arg.(value & pos 2 (some string) None & info [] ~docv:"LOGFILE" ~doc)
;;
