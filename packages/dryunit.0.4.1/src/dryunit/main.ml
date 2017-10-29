open Cmdliner
open Core_normalization

let version = "0.4.1"
let sdocs = Manpage.s_common_options
let exits = Term.default_exits


let help_secs = [
 `S Manpage.s_common_options;
 `P "These options are common to all commands.";
 `S "MORE HELP";
 `P "Use `$(mname) $(i,COMMAND) --help' for help on a single command.";`Noblank;
 `P "Use `$(mname) help patterns' for help on patch matching."; `Noblank;
 `P "Use `$(mname) help environment' for help on environment variables.";
 `S Manpage.s_bugs; `P "Check bug reports at http://bugs.example.org.";
]


let gen_opts nocache framework cache_dir ignore only ignore_path targets =
  Action.({ nocache; framework; cache_dir; ignore; only; ignore_path; targets; })


let gen_opts_t =
  let docs = "Generate source code for test executable with appropriate code to bootstrap a test framework" in
  let open Arg in
  let nocache = value & flag & info ["nocache"] ~docs ~doc:"Do not use cache." in
  let framework = value & opt (some string) None & info ["framework"] ~docs ~doc:"Select a test framework." in
  let cache_dir = value & opt (some string) None & info ["cache-dir"] ~docs ~doc:"Select a custom cache dir." in
  let only = value & opt (some string) None & info ["only"] ~docs ~doc:"Space separated list of words used to filter tests." in
  let ignore = value & opt (some string) None & info ["ignore"] ~docs ~doc:"Space separated list of words used to ignore tests." in
  let ignore_path = value & opt (some string) None & info ["ignore-path"] ~docs ~doc:"Space separated list of words used to ignore files." in
  let targets = value & pos_all string [] & info [] ~docv:"TARGET" in
  Term.(const gen_opts $ nocache $ framework $ cache_dir $ ignore $ only $ ignore_path $ targets)


let init_opts framework =
  Action.( {framework } )
let init_opts_t =
  let framework =
    Arg.(value & pos 0 string "alcotest" & info [] ~docv:"FRAMEWORK" ~doc:"Select a framework") in
  Term.(const init_opts $ framework)


let init_cmd =
  let doc = "shows a template configuration" in
  Term.(ret (const Action.init_executable $ init_opts_t)),
  Term.info "init" ~doc ~sdocs ~exits


let gen_extension_cmd =
  let doc = "Generate dryunit initialization code" in
  Term.(ret (const Action.gen_extension $ gen_opts_t)),
  Term.info "extension" ~doc ~sdocs ~exits


let gen_cmd =
  let doc = "Generate dryunit initialization code" in
  Term.(ret (const Action.gen_executable $ gen_opts_t)),
  Term.info "gen" ~doc ~sdocs ~exits


let help_cmd =
  let topic =
    let doc = "The topic to get help on. `topics' lists the topics." in
    Arg.(value & pos 0 (some string) None & info [] ~docv:"TOPIC" ~doc)
  in
  let doc = "show help" in
  Term.(ret
    (const Action.help $ Arg.man_format $ Term.choice_names $ topic)),
  Term.info "help" ~doc ~exits:Term.default_exits


let default_cmd ~version =
  let doc = "a detection tool for traditional testing in OCaml" in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Term.info "dryunit" ~version ~doc ~sdocs ~exits ~man:help_secs


let cmds =
  [ init_cmd
  ; gen_cmd
  ; gen_extension_cmd
  ; help_cmd
  ]

let () =
  Random.self_init ();
  Cmdliner.Term.(exit @@ eval_choice (default_cmd ~version) cmds)
