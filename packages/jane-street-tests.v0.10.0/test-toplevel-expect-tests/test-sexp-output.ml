#verbose true;;

let x = 42
[%%expect {|
val x : int = 42
|}];;

[@@@part "blah"];;

6 * 7
[%%expect {|
- : int = 10
|}]
