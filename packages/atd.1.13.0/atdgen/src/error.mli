val error : Atd.Ast.loc -> string -> 'a

val error2 : Atd.Ast.loc -> string -> Atd.Ast.loc -> string -> 'a

val error3
  : Atd.Ast.loc
  -> string
  -> Atd.Ast.loc
  -> string
  -> Atd.Ast.loc
  -> string -> 'a
