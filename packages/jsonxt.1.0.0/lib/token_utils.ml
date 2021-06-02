let token_to_string (tok:Tokens.token) =
  match tok with
  | STRING s -> s
  | OS -> "{"
  | OE -> "}"
  | NULL -> "Null"
  | NEGINFINITY -> "-Infinity"
  | NAN -> "NAN"
  | LEX_ERROR err -> "input error '" ^ err ^ "'"
  | LARGEINT s -> s
  | INT i -> string_of_int i
  | INFINITY -> "Infinity"
  | FLOAT f -> string_of_float f
  | EOF -> "end-of-file"
  | COMPLIANCE_ERROR err -> err
  | COMMA -> ","
  | COLON -> ":"
  | BOOL b -> if b then "true" else "false"
  | AS -> "["
  | AE -> "]"
  | VS -> "<"
  | VE -> ">"
  | TS -> "("
  | TE -> ")"
