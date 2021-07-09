(* Time-stamp: <modified the 22/01/2016 (at 14:45) by Erwan Jahier> *)

open Mypervasives

let (sim2chro_dyn : unit -> out_channel) =
  fun () ->
    let sim2chro = try mygetenv "SIM2CHRO" with _ -> "sim2chrogtk" in
    let oc = Unix.open_process_out (sim2chro ^ " -ecran /dev/null") in
    print_string "sim2chro is launched and waits for data...\n";
    flush stdout;
    oc          
