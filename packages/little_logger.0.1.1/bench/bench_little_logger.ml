open! Core
open! Core_bench

open! Little_logger

let () = Logger.set_log_level Logger.Level.Info

let logger_hi () = Logger.info (fun () -> "hi")
let logger_hi_s () = Logger.sinfo "hi"
let logger_hi_ryan () = Logger.info (fun () -> sprintf "hi %s %s" "ryan" "face")
let logger_hi_ryan_2 () =
  Logger.info (fun () -> sprintf "hi %s %s %d %f" "ryan" "face" 1234 1234.1234)

(* no op... *)
let logger_hi_no_op () = Logger.trace (fun () -> "hi")
let logger_hi_s_no_op () = Logger.strace "hi"
let logger_hi_ryan_no_op () =
  Logger.trace (fun () -> sprintf "hi %s %s" "ryan" "face")
let logger_hi_ryan_2_no_op () =
  Logger.trace (fun () -> sprintf "hi %s %s %d %f" "ryan" "face" 1234 1234.1234)

let el_logger =
  let open Easy_logging in
  Logging.make_logger "my_logger" Info [ CliErr Debug ]

let el_hi () = el_logger#info "hi"
let el_hi_s () = el_logger#sinfo "hi"
let el_hi_ryan () = el_logger#info "hi %s %s" "ryan" "face"
let el_hi_ryan_2 () =
  el_logger#info "hi %s %s %d %f" "ryan" "face" 1234 1234.1234

(* no op *)
let el_hi_no_op () = el_logger#trace "hi"
let el_hi_s_no_op () = el_logger#strace "hi"
let el_hi_ryan_no_op () = el_logger#trace "hi %s %s" "ryan" "face"
let el_hi_ryan_2_no_op () =
  el_logger#trace "hi %s %s %d %f" "ryan" "face" 1234 1234.1234

module Log = Dolog.Log

let () = Log.set_log_level Log.INFO
let () = Log.set_output stderr

let dolog_hi () = Log.info "hi"
let dolog_hi_ryan () = Log.info "hi %s %s" "ryan" "face"
let dolog_hi_ryan_2 () = Log.info "hi %s %s %d %f" "ryan" "face" 1234 1234.1234

(* no op *)
let dolog_hi_no_op () = Log.debug "hi"
let dolog_hi_ryan_no_op () = Log.debug "hi %s %s" "ryan" "face"
let dolog_hi_ryan_2_no_op () =
  Log.debug "hi %s %s %d %f" "ryan" "face" 1234 1234.1234

let () =
  let bench name f = Bench.Test.create ~name (fun () -> f ()) in
  Command.run
    (Bench.make_command
       [
         bench "logger_hi_s" logger_hi_s;
         bench "logger_hi" logger_hi;
         bench "logger_hi_ryan" logger_hi_ryan;
         bench "logger_hi_ryan_2" logger_hi_ryan_2;
         bench "el_hi_s" el_hi_s;
         bench "el_hi" el_hi;
         bench "el_hi_ryan" el_hi_ryan;
         bench "el_hi_ryan_2" el_hi_ryan_2;
         bench "dolog_hi" dolog_hi;
         bench "dolog_hi_ryan" dolog_hi_ryan;
         bench "dolog_hi_ryan_2" dolog_hi_ryan_2;
         bench "logger_hi_s_no_op" logger_hi_s_no_op;
         bench "logger_hi_no_op" logger_hi_no_op;
         bench "logger_hi_ryan_no_op" logger_hi_ryan_no_op;
         bench "logger_hi_ryan_2_no_op" logger_hi_ryan_2_no_op;
         bench "el_hi_s_no_op" el_hi_s_no_op;
         bench "el_hi_no_op" el_hi_no_op;
         bench "el_hi_ryan_no_op" el_hi_ryan_no_op;
         bench "el_hi_ryan_2_no_op" el_hi_ryan_2_no_op;
         bench "dolog_hi_no_op" dolog_hi_no_op;
         bench "dolog_hi_ryan_no_op" dolog_hi_ryan_no_op;
         bench "dolog_hi_ryan_2_no_op" dolog_hi_ryan_2_no_op;
       ])
