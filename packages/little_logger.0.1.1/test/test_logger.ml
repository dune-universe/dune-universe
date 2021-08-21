open! Core
open! Little_logger

let date_re = Re2.create_exn "[0-9]{4}-[0-9]{2}-[0-9]{2}"
let time_re = Re2.create_exn "[0-9]{2}:[0-9]{2}:[0-9]{2}"
let pid_re = Re2.create_exn "#[0-9]+"

let redact out =
  Re2.replace_exn ~f:(fun _m -> "DATE") date_re
  @@ Re2.replace_exn ~f:(fun _m -> "TIME") time_re
  @@ Re2.replace_exn ~f:(fun _m -> "PID") pid_re out

let log_all () =
  let msg () = sprintf "hi %s" "ryan" in
  Logger.trace msg;
  Logger.debug msg;
  Logger.info msg;
  Logger.warning msg;
  Logger.error msg;
  Logger.fatal msg;
  Logger.unknown msg

let log_all_string () =
  let msg = "hi ryan" in
  Logger.strace msg;
  Logger.sdebug msg;
  Logger.sinfo msg;
  Logger.swarning msg;
  Logger.serror msg;
  Logger.sfatal msg;
  Logger.sunknown msg

(* Level of string *)

let%expect_test "level_of_string" =
  let f s =
    Logger.Level.of_string s |> Or_error.ok_exn |> Logger.Level.to_string
    |> print_endline
  in
  f "tRaCe";
  [%expect {| TRACE |}];
  f "dEbUg";
  [%expect {| DEBUG |}];
  f "iNfO";
  [%expect {| INFO |}];
  f "wArNiNg";
  [%expect {| WARN |}];
  f "eRrOr";
  [%expect {| ERROR |}];
  f "fAtAl";
  [%expect {| FATAL |}];
  f "uNkNoWn";
  [%expect {| UNKNOWN |}];
  f "sIlEnT";
  [%expect {| SILENT |}];
  let () =
    match Logger.Level.of_string "bad thing" with
    | Ok _ -> assert false
    | Error err -> print_endline @@ Error.to_string_hum err
  in
  [%expect
    {| Level must be one of trace, debug, info, warning, error, fatal, unknown, silent.  Got 'bad thing'. |}]

(* Check that the getter/setters work. *)

let%expect_test "getting/setting the log level" =
  let open Logger in
  let set_and_print_log_level level =
    Logger.set_log_level level;
    print_endline @@ Level.to_string @@ get_log_level ()
  in
  (* Default *)
  print_endline @@ Level.to_string @@ get_log_level ();
  [%expect {| WARN |}];
  set_and_print_log_level Logger.Level.Trace;
  [%expect {| TRACE |}];
  set_and_print_log_level Logger.Level.Debug;
  [%expect {| DEBUG |}];
  set_and_print_log_level Logger.Level.Info;
  [%expect {| INFO |}];
  set_and_print_log_level Logger.Level.Warning;
  [%expect {| WARN |}];
  set_and_print_log_level Logger.Level.Error;
  [%expect {| ERROR |}];
  set_and_print_log_level Logger.Level.Fatal;
  [%expect {| FATAL |}];
  set_and_print_log_level Logger.Level.Unknown;
  [%expect {| UNKNOWN |}];
  set_and_print_log_level Logger.Level.Silent;
  [%expect {| SILENT |}]

(* Check that the levels work as expected. *)

let%expect_test _ =
  Logger.set_log_level Logger.Level.Trace;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    T, [DATE TIME PID] TRACE -- hi ryan
    D, [DATE TIME PID] DEBUG -- hi ryan
    I, [DATE TIME PID] INFO -- hi ryan
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Debug;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    D, [DATE TIME PID] DEBUG -- hi ryan
    I, [DATE TIME PID] INFO -- hi ryan
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Info;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    I, [DATE TIME PID] INFO -- hi ryan
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Warning;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Error;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Fatal;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Unknown;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect {|
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Silent;
  Logger.set_printer print_endline;
  log_all ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect {| |}]

(* String versions. *)

let%expect_test _ =
  Logger.set_log_level Logger.Level.Trace;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    T, [DATE TIME PID] TRACE -- hi ryan
    D, [DATE TIME PID] DEBUG -- hi ryan
    I, [DATE TIME PID] INFO -- hi ryan
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Debug;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    D, [DATE TIME PID] DEBUG -- hi ryan
    I, [DATE TIME PID] INFO -- hi ryan
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Info;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    I, [DATE TIME PID] INFO -- hi ryan
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Warning;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Error;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Fatal;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect
    {|
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Unknown;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect {|
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]

let%expect_test _ =
  Logger.set_log_level Logger.Level.Silent;
  Logger.set_printer print_endline;
  log_all_string ();
  let out = [%expect.output] in
  print_endline @@ redact out;
  [%expect {| |}]

(* Custom printer to file *)
let%expect_test _ =
  let fname = "silly_file.txt" in
  let printer msg =
    Out_channel.with_file fname ~append:true ~f:(fun chan ->
        Out_channel.output_string chan msg;
        Out_channel.newline chan)
  in
  Logger.set_log_level Logger.Level.Trace;
  Logger.set_printer printer;
  log_all_string ();
  print_endline @@ redact @@ In_channel.read_all fname;
  [%expect
    {|
    T, [DATE TIME PID] TRACE -- hi ryan
    D, [DATE TIME PID] DEBUG -- hi ryan
    I, [DATE TIME PID] INFO -- hi ryan
    W, [DATE TIME PID] WARN -- hi ryan
    E, [DATE TIME PID] ERROR -- hi ryan
    F, [DATE TIME PID] FATAL -- hi ryan
    U, [DATE TIME PID] UNKNOWN -- hi ryan |}]
