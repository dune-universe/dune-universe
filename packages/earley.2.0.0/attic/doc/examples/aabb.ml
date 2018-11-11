open Earley

let parser aabb =
  | 'a' aabb 'b'
  | EMPTY

let parser aabb_eof =
  | aabb EOF

let _ =
  let no_blank buf pos = (buf, pos) in
  handle_exception (parse_channel aabb_eof no_blank) stdin
