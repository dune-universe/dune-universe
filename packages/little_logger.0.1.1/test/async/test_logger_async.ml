open! Core
open! Async
open! Little_logger

let x = 1

let date_re = Re2.create_exn "[0-9]{4}-[0-9]{2}-[0-9]{2}"
let time_re = Re2.create_exn "[0-9]{2}:[0-9]{2}:[0-9]{2}"
let pid_re = Re2.create_exn "#[0-9]+"

let redact out =
  Re2.replace_exn ~f:(fun _m -> "DATE") date_re
  @@ Re2.replace_exn ~f:(fun _m -> "TIME") time_re
  @@ Re2.replace_exn ~f:(fun _m -> "PID") pid_re out

let%expect_test _ =
  let msg = "hi ryan" in
  Logger.set_log_level Logger.Level.Trace;
  Logger.set_printer Async.print_endline;
  (* trace *)
  Logger.strace msg;
  let%bind out = [%expect.output] in
  print_endline @@ redact out;
  [%expect {| T, [DATE TIME PID] TRACE -- hi ryan |}]

let%expect_test _ =
  let msg = "hi ryan" in
  Logger.set_log_level Logger.Level.Silent;
  Logger.set_printer Async.print_endline;
  Logger.strace msg;
  let%bind out = [%expect.output] in
  print_endline @@ redact out;
  [%expect {| |}]
