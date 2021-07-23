open Camyll
open Cmdliner

let guard f x =
  try `Ok (f x) with
  | Failure e -> `Error(false, e)
  | Sys_error e -> `Error(false, e)
  | Unix.Unix_error(e, cmd, "") ->
    `Error(false, cmd ^ ": " ^ Unix.error_message e)
  | Unix.Unix_error(e, cmd, p) ->
    `Error(false, cmd ^ " " ^ p ^ ": " ^ Unix.error_message e)

let build_cmd =
  let doc = "build the site" in
  Term.(ret (const (guard Build.build) $ const ())),
  Term.info "build" ~doc

let serve_cmd =
  let doc = "serve the site" in
  let port =
    let inf = Arg.info ~docv:"PORT" ["port"] in
    Arg.(value & opt int 8080 inf)
  in
  Term.(ret (const (guard Serve.serve) $ port)),
  Term.info "serve" ~doc

let default_cmd =
  let doc = "static site generator" in
  Term.(ret (const (`Help(`Auto, None)))),
  Term.info "camyll" ~doc

let cmds = [build_cmd; serve_cmd]

let () = Term.(exit @@ eval_choice default_cmd cmds)
