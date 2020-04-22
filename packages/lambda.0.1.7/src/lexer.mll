{
  open Parser
  exception LexingError of string
}

let digit = ['0' - '9']
let alpha = ['a' - 'z' 'A' - 'Z']
let empty = ['\n' '\t' ' ']

rule lexer = parse
  | "#" [^'\n']* '\n'?                        { lexer lexbuf                                       }
  | eof                                       { TEOF                                               }
  | empty+                                    { lexer lexbuf                                       }
  | "/"                                       { TLAMBDA                                            }
  | "λ"                                       { TLAMBDA                                            }
  | "."                                       { TDOT                                               }
  | "("                                       { TPAREN_LEFT                                        }
  | ")"                                       { TPAREN_RIGHT                                       }
  | (alpha (alpha | digit)*) as v             { TVAR v                                             }
  | _ as error                                { raise (LexingError ("Unknown " ^ Char.escaped error ^ "token." ))}
