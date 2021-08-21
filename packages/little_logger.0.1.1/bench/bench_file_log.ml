open! Core
open! Core_bench

open! Little_logger

(* Return list of entries in [path] as [path/entry] *)
let readdir path =
  Array.fold ~init:[]
    ~f:(fun acc entry -> Filename.concat path entry :: acc)
    (Sys.readdir path)

(* May raise some unix errors? *)
let rec rm_rf name =
  match Unix.lstat name with
  | { st_kind = S_DIR; _ } ->
      List.iter (readdir name) ~f:rm_rf;
      Unix.rmdir name
  | _ -> Unix.unlink name
  | exception Unix.Unix_error (ENOENT, _, _) -> ()

(* to file *)
let el_file_logger =
  let open Easy_logging in
  Logging.make_logger "file_logger" Debug [ File ("el_file_log.txt", Debug) ]

let el_hi_ryan_file () = el_file_logger#error "hi %s %s" "ryan" "face"

let fname = "silly_file.txt"

let chan = Out_channel.create fname
let printer msg = Out_channel.output_string chan (msg ^ "\n")

let () = Logger.set_printer printer

let logger_hi_ryan_file () =
  Logger.error (fun () -> sprintf "hi %s %s" "ryan" "face")

let () =
  let bench name f = Bench.Test.create ~name (fun () -> f ()) in
  Command.run
    (Bench.make_command
       [
         bench "el_hi_ryan_file" el_hi_ryan_file;
         bench "logger_hi_ryan_file" logger_hi_ryan_file;
       ])

let () = Out_channel.close chan

let () = rm_rf "logs"
let () = rm_rf "silly_file.txt"
