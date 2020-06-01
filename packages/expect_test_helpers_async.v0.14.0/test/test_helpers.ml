open! Core
open! Async
open! Import

let raises_exe = "bin/raises.exe"

let%expect_test "[~hide_positions:true] with a [Time.t]" =
  print_s ~hide_positions:true [%message (Time.epoch : Time.t)];
  [%expect {|
    (Time.epoch (1969-12-31 19:00:00.000000-05:00)) |}]
;;

let%expect_test "run" =
  let%bind () = run "echo" [ "foo" ] in
  [%expect {| foo |}]
;;

let%expect_test "run, with print_cmdline:true" =
  let%bind () = run ~print_cmdline:true "echo" [ "foo" ] in
  [%expect {|
    (run (cmdline (echo foo)))
    foo |}]
;;

let with_cd_into_temp_dir f =
  with_temp_dir (fun dir ->
    let%bind cwd = Unix.getcwd () in
    let%bind () = Unix.chdir dir in
    Monitor.protect f ~finally:(fun () -> Unix.chdir cwd))
;;

let%expect_test "run, with working dir" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "mkdir" [ "foo" ] in
    let%bind () = run "touch" [ "foo/bar" ] in
    let%bind () = run "ls" [] in
    let%bind () = [%expect {| foo |}] in
    let%bind () = run "ls" [] ~working_dir:"./foo" in
    let%bind () = [%expect {| bar |}] in
    return ())
;;

let%expect_test "run, with no expansion" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "echo" [ "~" ] in
    [%expect {|
      ~ |}])
;;

let%expect_test "run, with stdin" =
  let%bind () = run "cat" [ "-" ] ~stdin:"foo $PATH" in
  [%expect {|
    foo $PATH |}]
;;

let%expect_test "run, with stdin and print_cmdline:true" =
  let%bind () = run ~print_cmdline:true "cat" [ "-" ] ~stdin:"foo" in
  [%expect {|
    (run (cmdline (cat -)) (stdin foo))
    foo |}]
;;

let%expect_test "run, with stderr and non-zero exit" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "cat" [ "./i-hope-this-does-not-exist" ] in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      cat: ./i-hope-this-does-not-exist: No such file or directory |}])
;;

let%expect_test "run, with print_stdout/print_stderr overrides" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "echo" [ "success" ] ~print_stdout:If_unclean_exit in
    let%bind () = [%expect {| |}] in
    let%bind () =
      run "cat" [ "./i-hope-this-does-not-exist" ] ~print_stderr:If_unclean_exit
    in
    let%bind () =
      [%expect
        {|
      ("Unclean exit" (Exit_non_zero 1))
      --- STDERR ---
      cat: ./i-hope-this-does-not-exist: No such file or directory |}]
    in
    let%bind () = run "cat" [ "./i-hope-this-does-not-exist" ] ~print_stderr:Never in
    let%bind () = [%expect {|
      ("Unclean exit" (Exit_non_zero 1)) |}] in
    return ())
;;

let%expect_test "run, with bad exec" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = run "./i-hope-this-does-not-exist" [] in
    [%expect
      {|
      ("Process creation failed"
        (prog ./i-hope-this-does-not-exist)
        (args ())
        (error (
          Unix.Unix_error
          "No such file or directory"
          Core.Unix.create_process
          "((prog ./i-hope-this-does-not-exist) (args ()) (env (Extend ((OCAMLRUNPARAM b=0)))))"))) |}])
;;

let%expect_test "[run ~hide_positions:true]" =
  let%bind () =
    run ~hide_positions:true "echo" [ [%message [%here]] |> Sexp.to_string ]
  in
  [%expect {|
    lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL |}]
;;

let%expect_test "run, with postprocess" =
  let%bind () =
    run
      ~hide_positions:true
      ~postprocess:(fun string ->
        String.substr_replace_all string ~pattern:"test" ~with_:"foo")
      "echo"
      [ [%message [%here]] |> Sexp.to_string ]
  in
  [%expect {|
    lib/expect_foo_helpers/async/foo/foo_helpers.ml:LINE:COL |}]
;;

let%expect_test "system" =
  let%bind () = system {| echo $((1 + 1)) | tee /dev/stderr |} in
  [%expect {|
    2
    --- STDERR ---
    2 |}]
;;

let%expect_test "system, with cmdline" =
  let%bind () = system ~print_cmdline:true {| echo $((1 + 1)) | tee /dev/stderr |} in
  [%expect
    {|
    (run (cmdline (/bin/bash -c " echo $((1 + 1)) | tee /dev/stderr ")))
    2
    --- STDERR ---
    2 |}]
;;

let%expect_test "system, with non-zero exit" =
  let%bind () = system {| kill $$ |} in
  [%expect {|
    ("Unclean exit" (Signal sigterm)) |}]
;;

let%expect_test "system, with multi-line command" =
  let%bind () =
    system {|
      for i in $(seq 1 10); do
        echo $i
      done; |}
  in
  [%expect {|
    1
    2
    3
    4
    5
    6
    7
    8
    9
    10 |}]
;;

let%expect_test "system, with stdin" =
  let%bind () = system {| cat - |} ~stdin:"foo $PATH" in
  [%expect {|
    foo $PATH |}]
;;

let%expect_test "system, with non-existent command" =
  with_cd_into_temp_dir (fun () ->
    let%bind () = system "./i-hope-this-does-not-exist" in
    [%expect
      {|
      ("Unclean exit" (Exit_non_zero 127))
      --- STDERR ---
      /bin/bash: ./i-hope-this-does-not-exist: No such file or directory |}])
;;

let%expect_test "system, with a bash-ism not in POSIX or POSIX-mode bash" =
  let%bind () = system "cat <(echo foo)" in
  [%expect {| foo |}]
;;

let%expect_test "[system ~hide_positions:true]" =
  let%bind () =
    system
      ~hide_positions:true
      (concat [ "echo >&2 '"; [%message [%here]] |> Sexp.to_string; "'" ])
  in
  [%expect
    {|
    --- STDERR ---
    lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL |}]
;;

let%expect_test "system, without backtraces" =
  let%bind () = system ~enable_ocaml_backtraces:false raises_exe in
  [%expect
    {|
    ("Unclean exit" (Exit_non_zero 2))
    --- STDERR ---
    Uncaught exception:

      "An exception appeared!" |}]
;;


let%expect_test "run, without backtraces" =
  let%bind () = run ~enable_ocaml_backtraces:false raises_exe [] in
  [%expect
    {|
    ("Unclean exit" (Exit_non_zero 2))
    --- STDERR ---
    Uncaught exception:

      "An exception appeared!" |}]
;;

(* Can't test [run ~enable_ocaml_backtraces:true] because the backtraces are fragile. *)

let%expect_test "[show_raise_async], no exception, ignores return value" =
  let%bind () = show_raise_async ~hide_positions:true (fun () -> Deferred.return 1) in
  [%expect {|
    "did not raise" |}]
;;

let%expect_test "[show_raise_async], raises hiding positions" =
  let%bind () =
    show_raise_async ~hide_positions:true (fun () -> raise_s [%message [%here]])
  in
  [%expect
    {|
    (raised lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL) |}]
;;

let%expect_test "[show_raise_async] with a deep stack" =
  let rec loop n =
    if n = 0
    then failwith "raising"
    else (
      let%map r = loop (n - 1) in
      r + 1)
  in
  let%bind () = show_raise_async (fun () -> loop 5) in
  [%expect {|
    (raised (Failure raising)) |}]
;;

let%expect_test "[show_raise_async], raise after return" =
  let returned = Ivar.create () in
  let%bind () =
    show_raise_async ~hide_positions:true (fun () ->
      upon (Ivar.read returned) (fun () ->
        Exn.raise_without_backtrace (Failure "raise after return"));
      Deferred.unit)
  in
  let%bind () = [%expect {|
    "did not raise" |}] in
  Ivar.fill returned ();
  let%bind () = Scheduler.yield () in
  let%bind () =
    [%expect {|
    ("Raised after return" (Failure "raise after return")) |}]
  in
  return ()
;;

let%expect_test "[require_does_not_raise_async], no raise" =
  let%bind () = require_does_not_raise_async [%here] (fun () -> return ()) in
  let%bind () = [%expect {| |}] in
  return ()
;;

let%expect_test "[require_does_not_raise_async], raises" =
  let%bind () =
    require_does_not_raise_async [%here] ~cr:Comment (fun () ->
      raise_s [%message "KABOOM"])
  in
  let%bind () =
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL. *)
    ("unexpectedly raised" KABOOM) |}]
  in
  return ()
;;

let%expect_test "[require_does_not_raise_async], raise after return" =
  let returned = Ivar.create () in
  let%bind () =
    require_does_not_raise_async [%here] ~cr:Comment (fun () ->
      upon (Ivar.read returned) (fun () -> raise_s [%message "KABOOM"]);
      return ())
  in
  let%bind () = [%expect {| |}] in
  Ivar.fill returned ();
  let%bind () = Scheduler.yield () in
  let%bind () =
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL. *)
    ("Raised after return" KABOOM) |}]
  in
  return ()
;;

let%expect_test "[require_does_raise_async], no raise" =
  let%bind () =
    require_does_raise_async [%here] ~cr:Comment (fun () -> return `ignore_return_value)
  in
  let%bind () =
    [%expect
      {|
    (* require-failed: lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL. *)
    "did not raise" |}]
  in
  return ()
;;

let%expect_test "[require_does_raise_async], raises" =
  let%bind () =
    require_does_raise_async [%here] (fun () -> raise_s [%message "KABOOM"])
  in
  let%bind () = [%expect {|
    KABOOM |}] in
  return ()
;;

let%expect_test "[require_does_raise_async], raise after return" =
  let returned = Ivar.create () in
  let%bind () =
    require_does_raise_async [%here] ~cr:Comment (fun () ->
      upon (Ivar.read returned) (fun () -> raise_s [%message "also KAPOW"]);
      raise_s [%message "KABOOM"])
  in
  let%bind () = [%expect {|
    KABOOM |}] in
  Ivar.fill returned ();
  let%bind () = Scheduler.yield () in
  let%bind () =
    [%expect
      {|
    ("Raised after return"
     lib/expect_test_helpers/async/test/test_helpers.ml:LINE:COL
     "also KAPOW") |}]
  in
  return ()
;;
