let _ = Test.foo (* ? foo *)
let _ = Test.O.bar2 (* ? F.bar2 *)

include Test

let _ = foo (* ? foo *)

let (* in_test4 => *) in_test4 (* <= in_test4 *) = 1
