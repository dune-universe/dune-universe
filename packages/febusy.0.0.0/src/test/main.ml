open Nonstd

let () =
  let open Rresult.R in
  let open Cmdliner in
  let default_cmd =
    let doc = "Febusy tests." in
    let sdocs = Manpage.s_common_options in
    let exits = Term.default_exits in
    ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ pure ()))
    , Term.info "febusy-testing" ~version:"0.0.0" ~doc ~sdocs ~exits )
  in
  let test_build f =
    match f () with
    | Ok _ -> printf "OK\n%!"
    | Error e ->
        eprintf "ERROR: %s\n%!" (Febusy.Common.Error.to_string e) ;
        exit 2
  in
  let test_full_build ~state_file ~show_log f =
    test_build (fun () ->
        let res, log = Febusy.Edsl.Make_unix.run ~state_file f in
        if show_log then printf "LOG:\n%s\n%!" log ;
        res )
  in
  let show_log_flag () =
    Arg.(value & flag & info ["show-log"; "L"] ~doc:"Show the build log graph.")
  in
  let state_file_arg v =
    let open Term in
    pure (fun s -> `State_file s)
    $
    let open Arg in
    let inf =
      info ["state-file"] ~doc:"The file to load/save the state from/to."
    in
    match v with
    | None -> required & opt (some string) None & inf
    | Some s -> value & opt string s & inf
  in
  let random_one_cmd =
    let open Term in
    let term =
      pure (fun (`State_file state_file) show_log ->
          test_full_build
            (fun () -> Examples.random_one ())
            ~state_file ~show_log )
      $ state_file_arg (Some "/tmp/ro.state")
      $ show_log_flag ()
    in
    (term, info "random-one" ~doc:"Run the random-one test.")
  in
  let usage_report_cmd =
    let open Term in
    let term =
      pure (fun (`State_file state_file) show_log path_from path_to ->
          test_full_build ~state_file ~show_log (fun () ->
              Examples.usage_report path_from path_to ) )
      $ state_file_arg None $ show_log_flag ()
      $ Arg.(
          required
          & opt (some string) None
          & info ["from"] ~doc:"The path to write the report about.")
      $
      Arg.(
        required
        & opt (some string) None
        & info ["to"] ~doc:"The path to write the report in.")
    in
    (term, info "usage-report" ~doc:"Run the usage-report test.")
  in
  let tiny_example_cmd =
    let open Term in
    let term =
      pure (fun (`State_file state_file) show_log ->
          test_full_build ~state_file ~show_log (fun () ->
              Examples.tiny_example () ) )
      $ state_file_arg None $ show_log_flag ()
    in
    (term, info "tiny-example" ~doc:"Build the documentation website.")
  in
  let build_doc_cmd =
    let open Term in
    let term =
      pure (fun (`State_file state_file) show_log output ->
          test_full_build ~state_file ~show_log (fun () ->
              Examples.build_website ~output ) )
      $ state_file_arg None $ show_log_flag ()
      $
      Arg.(
        required
        & pos 0 (some string) None
        & info [] ~doc:"The path to write the website in.")
    in
    (term, info "build-documentation" ~doc:"Build the documentation website.")
  in
  let cmds =
    [ random_one_cmd
    ; usage_report_cmd
    ; default_cmd
    ; tiny_example_cmd
    ; build_doc_cmd ]
  in
  Term.(exit @@ eval_choice default_cmd cmds)
