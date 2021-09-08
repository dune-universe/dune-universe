let is_digit c =
  (function | '0'|'1'|'2'|'3'|'4'|'5'|'6'|'7'|'8'|'9' -> true | _ -> false) c
let x = function | 2|4 -> true | _ -> false
let f v = (function | Some x when x > 4 -> true | _ -> false) v
