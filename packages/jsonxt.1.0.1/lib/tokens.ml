type token = 
  | STRING of string
  | OS
  | OE
  | NULL
  | NEGINFINITY
  | NAN
  | LEX_ERROR of string
  | LARGEINT of string
  | INT of int
  | INFINITY
  | FLOAT of float
  | EOF
  | COMPLIANCE_ERROR of string
  | COMMA
  | COLON
  | BOOL of bool
  | AS
  | AE
  | VS
  | VE
  | TS
  | TE
