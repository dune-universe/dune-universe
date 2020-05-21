{
  type lexeme =
    | EOF
    | Reference of { ident : string; number : string }
}

rule main = parse
| "\\newlabel{" ([^ '}']* as ident) '}'
  "{{\\M@TitleReference {" ([^ '}']* as number) '}' {
  Reference { ident; number }
}
| eof {
  EOF
}
| _ {
  main lexbuf
}
