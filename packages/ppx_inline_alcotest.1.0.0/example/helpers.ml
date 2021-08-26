let f v = List.map ((+) 1) v


let%test "doubles argument" = Alcotest.(check (list int)) "same list" (f [1;2;3]) [2;3;4]
