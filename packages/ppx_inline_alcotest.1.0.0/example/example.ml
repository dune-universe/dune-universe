module E = Helpers


let x a = a + a

let%test "doubles argument" = Alcotest.(check int) "same int" (x 3) 6 
let%test "doubles argument" = Alcotest.(check int) "same int" (x 4) 8
let%test "doubles argument" = Alcotest.(check int) "same int" (x 2) 4
let%test "doubles argument" = Alcotest.(check int) "same int" (x 5) 10

