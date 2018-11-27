let digit = ['0'-'9']
let alpha = ['a'-'z'] | ['A'-'Z'] | '_'
let alphanum = alpha | digit

rule token = parse
  | [' ' '\t'] { token lexbuf }
  | '/' '/' [^'\n']*           { token lexbuf }
  | '\n'       { Lexing.new_line lexbuf; token lexbuf }
  | '1'                     { Parser.ONE }
  | digit+ '.' digit+ as num    { Parser.FLOAT_LIT (float_of_string num) }
  | digit+ as num           { Parser.INT_LIT (int_of_string num) }
  | alpha alphanum* as name { try List.assoc name ["range", Parser.RANGE;
                                                   "cname", Parser.CNAME;
                                                   "extern", Parser.EXTERN;
                                                   "public", Parser.PUBLIC;
                                                   "private", Parser.PRIVATE;
						   "phantom", Parser.PHANTOM;
                                                   "sum", Parser.SUM;
                                                   "proc", Parser.PROC;
                                                   "gets", Parser.GETS;
                                                   "recomputes", Parser.RECOMPUTES;
                                                   "propagates", Parser.PROPAGATES;
                                                   "delta", Parser.DELTA;
                                                   "sets", Parser.SETS;
                                                   "increments", Parser.INCREMENTS;	
						   "scales", Parser.SCALES;
                                                   "min", Parser.MIN;
                                                   "max", Parser.MAX;
                                                   "abs", Parser.ABS;
                                                   "unit", Parser.UNIT;
                                                   "float32", Parser.FLOAT32;
                                                   "float64", Parser.FLOAT64]
                              with Not_found -> Parser.ID name
                            }
  | '>' '=' { Parser.GE }
  | '<' '=' { Parser.LE }
  | '>' { Parser.GT }
  | '<' { Parser.LT }
  | '(' { Parser.LP }
  | ')' { Parser.RP }
  | '[' { Parser.LB }
  | ']' { Parser.RB }
  | '*' { Parser.STAR }
  | '/' { Parser.SLASH }
  | '+' { Parser.PLUS }
  | '-' { Parser.MINUS }
  | '^' { Parser.HAT }
  | '?' { Parser.QUESTION_MARK }
  | ':' { Parser.COLON }
  | ';' { Parser.SEMICOLON }
  | ',' { Parser.COMMA }
  | '=' { Parser.EQUALS }
  | _ as c { Printf.fprintf stderr "Unrecognized character: %c\n" c; flush stdout; exit 1 }
  | eof { Parser.EOF }
