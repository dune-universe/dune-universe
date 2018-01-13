(*
  Tests which must fail are declared by [%%TEST_FAIL] 
  or [%%TEST] with _fail suffix.
*)

module PTest = Ppx_test.Test

(* [%%TEST_FAIL ..] *)
[%%TEST_FAIL let x = raise Exit]

(* [%%TEST let ..._fail = ...] *)
[%%TEST
 let x_fail = raise Exit
]

(* Tests are executed by calling [PTest.collect ()] *)      
let () = PTest.collect ()
