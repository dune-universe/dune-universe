open Wikitext

let () =
  assert (Array.length Sys.argv = 2) ;
  let file = Array.get Sys.argv 1 in
  let ch = open_in file in
  let lexbuf = Lexing.from_channel ch in
  let ast =
    doc_from_lexbuf lexbuf
    |> Mapper.set_toc
    |> Mapper.set_links
    |> Mapper.normalize
  in
  close_in ch ;
  print_endline @@ Test_show.printer ast
