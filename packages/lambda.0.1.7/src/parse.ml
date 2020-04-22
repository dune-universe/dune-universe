exception InvalidLambdaString;;

let parse s = 
  try 
    Parser.prog Lexer.lexer (Lexing.from_string s)
  with | _ -> raise InvalidLambdaString
;;