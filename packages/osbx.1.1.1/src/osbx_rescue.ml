open Cmdliner
open Rescue

let rescue (in_filename:string) (out_dirname:string) (log_filename:string option) : unit =
  match Process.rescue_from_file ~in_filename ~out_dirname ~log_filename with
  | Ok stats  -> Stats.print_stats stats
  | Error msg -> Printf.printf "%s\n" msg
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
  let doc = "Logfile to keep track of progress to survive interruptions" in
  Arg.(value & pos 2 (some string) None & info [] ~docv:"LOGFILE" ~doc)
;;
