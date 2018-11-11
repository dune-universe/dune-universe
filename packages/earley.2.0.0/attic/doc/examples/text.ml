open Earley

let inter_paragraph = blank_regexp ''[ \t\r\n]*''

let inter_word str pos =
  let gram = parser ''[ \t\r]*'' '\n'? ''[ \t\r]*'' in
  let _, str, pos = partial_parse_buffer gram no_blank str pos in
  str, pos

let word = parser w:''[^ \n\t\r]+'' -> w

let paragraph = change_layout (parser ws:word+ -> ws) inter_word

let text = parser ps:paragraph* -> ps

let _ =
  let ps = handle_exception (parse_channel text inter_paragraph) stdin in
  let nb = List.length ps in
  Printf.printf "%i paragraphs read.\n" nb
