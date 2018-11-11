open Earley

let parser nb_cc nb =
  | 'c' (nb_cc (nb - 1))  when nb > 0
  | EMPTY                 when nb <= 0

let parser aabb =
  | 'a' n:aabb 'b' -> n + 1
  | EMPTY          -> 0

let parser aabbcc =
  | n:aabb ->> (nb_cc n) EOF

let _ =
  let no_blank buf pos = (buf, pos) in
  handle_exception (parse_channel aabbcc no_blank) stdin
