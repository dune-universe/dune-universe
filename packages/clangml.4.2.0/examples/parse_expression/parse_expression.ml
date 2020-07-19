let () =
  Format.printf "%a@." Clang.Expr.pp
    (Option.get (fst (Clang.Expr.parse_string ~context:[
       Clang.Ast.node (Clang.Ast.Var (
         Clang.Ast.var "i" (Clang.Type.make (BuiltinType Int))))]
     {|i + "a"|})))
