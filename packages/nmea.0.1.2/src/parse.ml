open Sentence;;

let parse s = 
  try 
    Parser.sentence Lexer.token (Lexing.from_string s)
  with | _ -> raise Invalid_sentence
;;

let parse_opt s = try Some(parse s) with | _ -> None;;

let next ch = parse_opt @@ input_line ch;;

let next_coord ch = 
  match next ch with
  | Some(GPGGA (s)) -> Some(s.coord)
  | Some(GPRMC (s)) -> Some(s.coord)
  | _ -> None
;;