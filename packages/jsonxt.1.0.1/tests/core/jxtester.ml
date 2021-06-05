module CmdlineOptions = struct
  let tester command file alco_opts =
    Printf.printf "command: %s\nFile: %s\nAlcoTest: %s\n" command file (String.concat ", " alco_opts)

  open Cmdliner
  let alco_opts = Arg.(value & pos_all string [] & info [] ~docv:"-- [AlcoTest options]")
  let tfile = Arg.(required & opt (some file) None & info ["t"; "tests"] ~docv:"FILE" ~doc:"file to get test data from")

  let compliance_cmd f =
    let doc = "perform compliance checks with the various supported levels" in
    let man = [
      `S Manpage.s_description;
      `P "run internal compliance testing as defined in -t FILE. This has the format:";
      `Pre "  <level> [pass|fail] <filename> [32|64|all]";
      `P "Where the fields are defined as follows";
      `I ("level",
          "one of strict, basic, extended, yjbasic and yjsafe. These correspond)
           to the various compliance levels (yjbasic maps to Yojson.Basic etc)");
      `I ("[pass|fail]", "indicates the expected outcome");
      `I ("<filename>",
          "the file containing the test, it is assumed to be in the current directory,
           .json is automatically appended to the filename.");
      `I ("[32|64|all]", "is optional and defines the int size to run the test on, defaults to all.")
    ]
    in
    Term.(const f $ tfile $ alco_opts),
    Term.info "compliance" ~exits:Term.default_exits ~doc ~man

  let validation_cmd gen validate =
    let man = [
      `S Manpage.s_description;
      `P "run the parser test suite, parsing and verifying each of the
          json strings defined in -t FILE. This is a tab seperated list
          of json and expected sexp in the format:";
      `Pre "  bits <tab> json <tab> sexp <tab> sexp_strem";
      `P "Where the fields are defined as follows";
      `I ("bits", "bit size of platform to run test on. Either 32, 64 or all");
      `I ("json", "json to parse");
      `I ("sexp", "is the expected sexp");
      `I ("sexp_stream", "is the expected sexp from the stream parser");
      `P "When in gen mode the sexps are ignored and a file suitable for
          using with run is output to stdout\n"
    ]
    in
    let gen_run subcmd tfile alco_opts =
      match subcmd with
      | "gen" -> gen tfile alco_opts; `Ok ()
      | "run" -> validate tfile alco_opts; `Ok ()
      | _ -> `Error (true, "expected run or gen")
    in
    let alco_opts = Arg.(value & pos_right 0 string [] & info [] ~docv:"-- [AlcoTest options]") in
    let doc = "run to run encode/decode, gen to generated validation data" in
    let subcmd = Arg.(required & pos 0 (some string) None & info [] ~docv:"[gen|run]" ~doc) in
    let doc = "perform decode and encode validation" in
    Term.(ret (const gen_run $ subcmd $ tfile $ alco_opts)),
    Term.info "validation" ~exits:Term.default_exits ~doc ~man

  let suite_cmd std_f stream_f monad_f all_f =
    let suite_run subcmd files =
      let files = List.filter (fun n -> not (Filename.check_suffix n ".exe")) files in
      match subcmd with
      | "std" -> std_f files; `Ok ()
      | "stream" -> stream_f files; `Ok ()
      | "monad" -> monad_f files; `Ok ()
      | "all" -> all_f files; `Ok ()
      | _ -> `Error (true, "expected std, stream or monad")
    in
    let doc = "Process the JsonSuite files available from https://github.com/nst/JSONTestSuite" in
    let subcmd = Arg.(required & pos 0 (some string) None & info [] ~docv:"[std|stream|monad]" ~doc) in
    let files = Arg.(value & pos_right 0 string [] & info [] ~docv:"FILES") in
    Term.(ret (const suite_run $ subcmd $ files)),
    Term.info "suite" ~exits:Term.default_exits ~doc

  let default_cmd =
    let exits = Term.default_exits in
    Term.(ret (const (fun _ -> `Help (`Pager, None)) $ alco_opts)),
    Term.info "jxtester" ~version:"v1.0.1" ~exits

  let command = Arg.(required & pos 0 (some string) None & info [] ~docv:"[compliance|validation]")
  
  let cmd =
    Term.(const tester $ command $ tfile $ alco_opts),
    Term.info "jxtester" ~version:"v1.0.1" ~exits:Term.default_exits

end

let cmds = [
    CmdlineOptions.compliance_cmd ComplianceTests.run_tests
  ; CmdlineOptions.validation_cmd ValidationTests.gen_config ValidationTests.run_tests
  ; CmdlineOptions.suite_cmd
      JsonTestSuite.test_suite_std
      JsonTestSuite.test_suite_stream
      JsonTestSuite.test_suite_monad
      JsonTestSuite.test_suite_all
  ]
let () = Cmdliner.Term.(exit @@ eval_choice CmdlineOptions.default_cmd cmds)
