let from_lexbuf lexbuf =
  let next_line () = Lexer.file lexbuf in
  let msgs = ref [] in
  let rec loop = function
    | Some msg ->
        msgs := msg :: !msgs ;
        loop (next_line ())
    | None ->
        ()
  in
  loop (next_line ()) ;
  List.rev !msgs

let from_channel chan = from_lexbuf (Sedlexing.Utf8.from_channel chan)

let from_string string = from_lexbuf (Sedlexing.Utf8.from_string string)
