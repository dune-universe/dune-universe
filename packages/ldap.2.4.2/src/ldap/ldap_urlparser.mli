type token =
  | SCHEME
  | COLONSLASHSLASH
  | SLASH
  | QUESTION
  | EQUAL
  | COLON
  | COMMA
  | WHSP
  | CRITICAL
  | HOST of (string)
  | PORT of (string)
  | DN of (string)
  | IDENT of (string)
  | SCOPE of (string)
  | FILTER of (string)

val ldapurl :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> Ldap_types.ldap_url
