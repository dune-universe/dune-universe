open Base
open Common
open Cmdliner

let no_documentation =
  Printf.sprintf "`%s` documentation is not available. \n Consider contributing Pull Request to https://github.com/tldr-pages/tldr"

let display_page command platform update_cache =
  ignore update_cache;
  match get_page command platform with
  | Missing -> Stdio.printf "%s\n" (no_documentation command)
  | Error e -> Stdio.printf "%s\n" e
  | Success page -> Display.display page

module Args = struct
  open Arg

  let command =
    let doc = "Display the tldr page for $(docv)" in
    non_empty
    & pos_all string []
    & info [] ~docv:"COMMAND" ~doc

  let platform =
    let doc = "Display the command for a given $(docv). Options can be one of [linux, sunos, osx]." in
    value
    & opt string Environment.system
    & info ["p"; "platform"] ~docv:"PLATFORM" ~doc

  let update_cache =
    let doc = "Update the local cache." in
    value
    & flag
    & info ["u"; "update"] ~docv:"UPDATE" ~doc
end

let () =
  let open Term in

  let info =
    let doc = "Simplified man pages" in
    let man = [
      `S Manpage.s_bugs;
      `P "Email bug reports to <rosaleschase.j@gmail.com>." ]
    in
    info "tldr" ~version:"0.3.0" ~doc ~man
  in

  let run_t = const display_page
              $ (const (String.concat ~sep:"-") $ Args.command)
              $ Args.platform
              $ Args.update_cache
  in

  exit @@ eval (run_t, info)
