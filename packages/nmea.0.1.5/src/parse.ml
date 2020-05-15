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
  | Some(GGA (s)) -> Some(s.coord)
  | Some(RMC (s)) -> Some(s.coord)
  | _ -> None
;;