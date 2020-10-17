open Cmdliner

let build () =
  let config =
    try
      Filesystem.read_bin "config.yml"
      |> Yaml.of_string_exn
      |> Config.of_json
    with
    | Sys_error _ -> Config.default
  in
  Build.build config

let build_cmd =
  let doc = "build the site" in
  Term.(const build $ const ()),
  Term.info "build" ~doc

let default_cmd =
  let doc = "static site generator" in
  Term.(ret (const (`Help(`Auto, None)))),
  Term.info "camyll" ~doc

let cmds = [build_cmd]

let main () = Term.(exit @@ eval_choice default_cmd cmds)
