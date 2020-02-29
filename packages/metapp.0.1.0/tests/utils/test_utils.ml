[%%metadir "metapp/.metapp.objs/byte/"]

let () =
  Test_framework.assert_eq Int.equal Format.pp_print_int
    [%meta Metapp.filter.expr Metapp.filter
       [%e (1, 2 [@if false])]] 1

[%%meta Metapp.filter.structure_item Metapp.filter
  [%stri let a = 1 and[@if false] b = c]]

let () =
  Test_framework.assert_eq Int.equal Format.pp_print_int a 1
