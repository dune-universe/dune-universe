(* Test of compilation errors *)
let x = 1 + 'a'
[%%expect{|
Line _, characters 12-15:
Error: This expression has type char but an expression was expected of type
         int
|}];;

#verbose true;; (* To enable printing of values *)

Array.init 10 string_of_int;;
[%%expect{|
- : bytes array = [|"0"; "1"; "2"; "3"; "4"; "5"; "6"; "7"; "8"; "9"|]
|}]
