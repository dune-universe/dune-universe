open Lilac
open Base

let debug fin =
  let yaml = yaml_from_fpath fin  in
    yaml_value_str ~path:"lilac-params.source.user" yaml |> Option.value ~default:"n/a" |>  Stdio.print_endline;
    yaml_value_str ~path:"lilac-params.source.cred" yaml |> Option.value ~default:"n/a" |>  Stdio.print_endline;
    yaml_value_str ~path:"lilac-params.source.url" yaml |> Option.value ~default:"n/a" |>  Stdio.print_endline;
    yaml_value_str ~path:"lilac-params" yaml |> Option.value ~default:"n/a" |>  Stdio.print_endline;

open Cmdliner

let config =
  let doc = "Path to YAML config file. See some examples here https://github.com/shnewto/lilac/tree/main/test/res" in
  Arg.(required & opt (some string) None & info ["i"; "input"] ~doc)

let lilac_t = Term.(const debug $ config)

let info =
  let doc = constellation in
  let man = [
    `S Manpage.s_synopsis;
    `P "lilac --input <path-to-a-config.yaml>";
    `S Manpage.s_name;
    `P "lilac";
    `S Manpage.s_bugs;
    `P "Report bugs by raising an issue at https://github.com/shnewto/lilac/issues";]
  in
  Term.info "lilac" ~version:"v0.1.0~dev" ~doc ~exits:Term.default_exits ~man

let () = Term.exit @@ Term.eval (lilac_t, info)