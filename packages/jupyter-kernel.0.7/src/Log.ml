(* 
 * iocaml - an OCaml kernel for IPython
 *
 *   (c) 2014 MicroJamJar Ltd
 *
 * Author(s): andy.ray@ujamjar.com
 * Description: log file for debugging
 *
 *)

let src = Logs.Src.create ~doc:"jupyter kernel logs" "jupyter-kernel"

let combine_reporters r1 r2 =
  let report = fun src level ~over k msgf ->
    let v = r1.Logs.report src level ~over:(fun () -> ()) k msgf in
    r2.Logs.report src level ~over (fun () -> v) msgf
  in
  { Logs.report }

let file_reporter s : Logs.reporter = 
  let flog = open_out_gen [Open_text;Open_creat;Open_append] 0o666 s in
  let () = at_exit (fun () -> flush flog; close_out flog) in
  let fmt = Format.formatter_of_out_channel flog in
  let rep = Logs.format_reporter ~app:fmt ~dst:fmt () in
  rep

let open_log_file s = 
  Logs.set_reporter (combine_reporters (Logs.reporter ()) @@ file_reporter s); 
  Logs.info ~src (fun k->k "open file %S for logging" s);
  ()

let log s = Logs.debug ~src (fun k->k "%s" s)
let logf s = Printf.ksprintf log s

include (val Logs.src_log src)
