[%%metadir "metapp/.metapp.objs/byte/"]
[%%metapackage "stdcompat"]
[%%metaflag "-open", "Stdcompat"]

let () =
  Test_framework.assert_eq Int.equal Format.pp_print_int
    [%meta (new Metapp.filter)#expression
       [%e (1, 2 [@if false])]] 1

[%%meta (new Metapp.filter)#structure_item
  [%stri let a = 1 and[@if false] b = c]]

let () =
  Test_framework.assert_eq Int.equal Format.pp_print_int a 1

let () =
  assert
    [%meta
       Metapp.Exp.of_bool (Option.equal Metapp.Longident.equal
         (Metapp.Longident.of_expression_opt [%e M.(+)])
         (Some (Longident.Ldot (Lident "M", "+"))))]
