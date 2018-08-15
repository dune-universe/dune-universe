{
open Format_a_parser

let explode str =
  let lst = ref [] in
  String.iter (fun chr -> lst := chr :: !lst) str;
  List.rev !lst

let of_hex str =
  let lst = List.map
      (function
        | '0' .. '9' as chr -> Char.code chr - 48
        | 'a' .. 'f' as chr -> Char.code chr - 97 + 10
        | 'A' .. 'F' as chr -> Char.code chr - 65 + 10
        | _ -> invalid_arg "Invalid character")
      (explode str) in
  List.fold_left (fun n k -> n * 16 + k) 0 lst
}

let ZERO = '0'
let HEXA = [ '0' - '9' 'a' - 'f' 'A' - 'F' ]
let EMPTY = [ ' ' '\t' ]
let NEWLINE = '\n'

rule token = parse
  | eof                     { EOF }
  | '#'                     { let buffer = Buffer.create 16 in COMMENT (string buffer lexbuf) }
  | ZERO 'x' (HEXA+ as lst) { NUMBER (of_hex lst) }
  | EMPTY+                  { token lexbuf }
  | NEWLINE                 { token lexbuf }
and string buffer = parse
  | NEWLINE                 { Buffer.contents buffer }
  | _ as chr                { Buffer.add_char buffer chr; string buffer lexbuf }

{

}
