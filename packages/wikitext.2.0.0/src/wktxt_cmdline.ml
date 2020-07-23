open Wikitext
let () =
  let lexbuf = Lexing.from_channel stdin in
  let doc = doc_from_lexbuf lexbuf |> Mapper.set_toc |> Mapper.set_links in
  output_document (Printf.printf "%s") doc
