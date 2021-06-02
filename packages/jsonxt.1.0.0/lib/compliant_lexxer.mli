module type Lex = sig
  val read : Lexing.lexbuf -> Tokens.token
end

module Make (Compliance : Compliance.S) : Lex
