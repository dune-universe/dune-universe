let of_file path =
  Bos.OS.File.with_ic path
    (fun ic (parser, lexer) ->
       let lexbuf = Lexing.from_channel ic in
       parser lexer lexbuf)
    (Format_a_parser.file, Format_a_lexer.token)

open Rresult

let pp_array ppf lst =
  let sep = Fmt.const Fmt.string ";" in
  Fmt.(suffix (const string "|]") @@ prefix (const string "[|") @@ list ~sep int) ppf lst

let produce oc database =
  let ppf = Format.formatter_of_out_channel oc in
  let res = Array.init 256 (fun idx -> match Ptmap.find idx database with
      | (cp, _) -> cp
      | exception Not_found -> (-1)) |> Array.to_list in
  Fmt.pf ppf "let map = %a\n%!" pp_array res; Ok ()

let parse source destination =
  let source = Fpath.v source in

  of_file source >>= fun src ->
  Koi8.extract src >>= fun maps ->

  Bos.OS.File.with_oc destination produce maps

open Cmdliner

let source =
  let doc = "Maps file to unicode" in
  Arg.(required & pos 0 (some file) None & info [] ~docv:"FILE" ~doc)

let fpath = Arg.conv ~docv:"PATH" Fpath.(of_string, pp)

let destination =
  let doc = "Output file" in
  Arg.(required & pos 1 (some fpath) None & info [] ~docv:"FILE" ~doc)

let cmd =
  let doc = "Uuuu import tool" in
  let exits = Term.default_exits in
  let man =
    [ `S Manpage.s_description
    ; `P "Tool to import a map between characters into Unicode." ] in
  Term.(const parse $ source $ destination),
  Term.info "import" ~version:"0.1" ~doc ~exits ~man

let () = Term.(exit @@ eval cmd)
