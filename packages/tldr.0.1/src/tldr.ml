open Core
open Common


let no_documentation =
  Printf.sprintf "`%s` documentation is not available. \n Consider contributing Pull Request to https://github.com/tldr-pages/tldr\n"

let display = Display.display

let display_page command platform =
  match get_page command platform with
  | Missing -> printf "%s" (no_documentation command)
  | Error e -> printf "%s" e
  | Success page -> display page


let () =
  Command.basic ~summary:"tldr"
    Command.Let_syntax.(
      [%map_open
        let update_cache = flag "--update-cache" no_arg
            ~doc:"Update the cached commands"
        and os = flag "--os" (optional_with_default Environment.system string)
            ~doc:"Override the operating System [linux, osx, sunos, windows]"
        and command = anon (sequence ("command" %: string))
        in

        fun () ->
          if command = [] then
            printf "tldr\n\nSimplified man pages\n\nUsage: tldr <command> \nFor more info run: tldr -help\n"
          else
            let command = String.concat ~sep:"-" command in
            ignore update_cache;
            display_page command os
      ])
  |> Command.run

