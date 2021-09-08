let is_digit c = [%matches? '0' | '1' | '2' | '3' | '4' | '5' | '6' | '7' | '8' | '9'] c

let x = [%matches? 2 | 4]

let f v = [%matches? Some(x) when x > 4] v