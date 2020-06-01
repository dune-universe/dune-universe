open! Core
open! Async
open Expect_test_helpers_core

module Print_rule = struct
  type t =
    | Always
    | If_unclean_exit
    | Never
  [@@deriving sexp_of]
end

let run
      ?(enable_ocaml_backtraces = false)
      ?(extend_env = [])
      ?(hide_positions = false)
      ?(postprocess = Fn.id)
      ?(print_cmdline = false)
      ?(print_stdout = Print_rule.Always)
      ?(print_stderr = Print_rule.Always)
      ?stdin
      ?working_dir
      prog
      args
  =
  let env =
    `Extend
      (if enable_ocaml_backtraces
       then extend_env
       else ("OCAMLRUNPARAM", "b=0") :: extend_env)
  in
  if print_cmdline
  then (
    let cmdline = prog :: args in
    match stdin with
    | None -> print_s [%message "run" (cmdline : string list)]
    | Some stdin -> print_s [%message "run" (cmdline : string list) (stdin : string)]);
  match%bind Process.create ?working_dir ~env ~prog ~args () with
  | Error error ->
    print_s
      [%message
        "Process creation failed"
          (prog : string)
          (args : string list)
          (working_dir : (string option[@sexp.option]))
          (error : Error.t)];
    return ()
  | Ok process ->
    (match stdin with
     | None -> ()
     | Some stdin -> Writer.write (Process.stdin process) stdin);
    let%bind { stdout; stderr; exit_status } = Process.collect_output_and_wait process in
    let maybe_hide_positions string =
      if not hide_positions then string else hide_positions_in_string string
    in
    let stdout = maybe_hide_positions stdout |> postprocess in
    let stderr = maybe_hide_positions stderr |> postprocess in
    let should_print : Print_rule.t -> bool = function
      | Always -> true
      | If_unclean_exit -> Result.is_error exit_status
      | Never -> false
    in
    if should_print print_stdout then print_string stdout;
    (match exit_status with
     | Ok () -> ()
     | Error err -> print_s [%message "Unclean exit" ~_:(err : Unix.Exit_or_signal.error)]);
    if should_print print_stderr && not (String.is_empty stderr)
    then (
      print_endline "--- STDERR ---";
      print_string stderr);
    return ()
;;

let system ?enable_ocaml_backtraces ?hide_positions ?print_cmdline ?stdin cmd =
  run
    ?enable_ocaml_backtraces
    ?hide_positions
    ?print_cmdline
    ?stdin
    "/bin/bash"
    [ "-c"; cmd ]
;;

let with_temp_dir f =
  let in_dir = Sys.getenv "TMPDIR" in
  let keep_tmp_dir = Option.is_some (Sys.getenv "KEEP_EXPECT_TEST_DIR") in
  let dir = Filename.temp_dir ?in_dir "expect-" "-test" in
  (* Note that this blocks *)
  assert (Filename.is_absolute dir);
  Monitor.protect
    (fun () -> f dir)
    ~finally:(fun () ->
      if keep_tmp_dir
      then (
        eprintf "OUTPUT LEFT IN %s\n" dir;
        return ())
      else run "rm" [ "-rf"; dir ])
;;

let hardlink_or_copy ~orig ~dst =
  match%bind
    Monitor.try_with ~extract_exn:true (fun () ->
      Unix.link ~target:orig ~link_name:dst ())
  with
  | Ok () -> return ()
  | Error (Unix.Unix_error (EXDEV, _, _)) -> run "cp" [ "-T"; "--"; orig; dst ]
  | Error e -> raise e
;;

let within_temp_dir ?(links = []) f =
  let%bind cwd = Unix.getcwd () in
  with_temp_dir (fun temp_dir ->
    let path_var = "PATH" in
    let old_path = Unix.getenv_exn path_var in
    let bin = temp_dir ^/ "bin" in
    Unix.putenv ~key:path_var ~data:(String.concat ~sep:":" [ bin; old_path ]);
    let%bind () = run "mkdir" [ bin ] in
    let%bind () =
      Deferred.List.iter links ~f:(fun (file, action, link_as) ->
        let link_as =
          match action with
          | `In_path_as -> "bin" ^/ link_as
          | `In_temp_as -> link_as
        in
        hardlink_or_copy ~orig:file ~dst:(temp_dir ^/ link_as))
    in
    let%bind () = Unix.chdir temp_dir in
    Monitor.protect f ~finally:(fun () ->
      Unix.putenv ~key:path_var ~data:old_path;
      Unix.chdir cwd))
;;

let sets_temporarily_async and_values ~f =
  let restore_to = List.map and_values ~f:Ref.And_value.snapshot in
  Ref.And_value.sets and_values;
  Monitor.protect f ~finally:(fun () ->
    Ref.And_value.sets restore_to;
    return ())
;;

let set_temporarily_async r x ~f = sets_temporarily_async [ T (r, x) ] ~f

let try_with f ~rest =
  let monitor = Monitor.create () in
  Monitor.detach_and_iter_errors monitor ~f:(fun exn -> rest (Monitor.extract_exn exn));
  Scheduler.within' ~monitor (fun () ->
    Monitor.try_with ~extract_exn:true ~rest:`Raise f)
;;

let show_raise_async (type a) ?hide_positions (f : unit -> a Deferred.t) =
  let%map result =
    try_with f ~rest:(fun exn ->
      print_s ?hide_positions [%message "Raised after return" ~_:(exn : exn)])
  in
  show_raise ?hide_positions (fun () -> Result.ok_exn result)
;;

let require_does_not_raise_async ?cr ?hide_positions ?show_backtrace here f =
  let%map result =
    try_with f ~rest:(fun exn ->
      print_cr here ?cr ?hide_positions [%message "Raised after return" ~_:(exn : exn)])
  in
  require_does_not_raise ?cr ?hide_positions ?show_backtrace here (fun () ->
    Result.ok_exn result)
;;

let require_does_raise_async
      ?(cr = CR.CR)
      ?(hide_positions = CR.hide_unstable_output cr)
      ?show_backtrace
      here
      f
  =
  let%map result =
    try_with f ~rest:(fun exn ->
      (* It's not clear what do if we get exceptions after the deferred is
         returned... Just printing out "Raised after return" for now. *)
      print_s
        ~hide_positions
        [%message
          "Raised after return" ~_:(here : Source_code_position.t) ~_:(exn : exn)])
  in
  require_does_raise ~cr ~hide_positions ?show_backtrace here (fun () ->
    Result.ok_exn result)
;;
