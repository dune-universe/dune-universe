open Decap

let g = declare_grammar "g"

let _ = set_grammar g (
  parser
    CHR'a' g
  | EMPTY )

let blank str pos = str, pos

let _ = parse_string g blank "aaab"
