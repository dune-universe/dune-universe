open OUnit2
open Shared_secret

let identity value = value

module First = struct
  include Exception (String)

  let procedure ( )     = Raiser.raise "Oops!"
  let handler procedure = Handler.handle procedure identity
end

module Second = struct
  include Exception (String)

  let procedure ( )     = Raiser.raise "Hello, World!"
  let handler procedure = Handler.handle procedure identity
end

let __caught_first  _ = assert_equal (First.handler First.procedure)   "Oops!"
let __caught_second _ = assert_equal (Second.handler Second.procedure) "Hello, World!"

let __uncaught_first _ =
  let uncaught = "Uncaught." in
  assert_equal (try (First.handler Second.procedure) with _ -> uncaught) uncaught

let __uncaught_second _ =
  let uncaught = "Uncaught." in
  assert_equal (try (Second.handler First.procedure) with _ -> uncaught) uncaught

let suite = "exceptional-suite" >::: [
  "caught-first"    >:: __caught_first;
  "caught-second"   >:: __caught_second;
  "uncaught-first"  >:: __uncaught_first;
  "uncaught-second" >:: __uncaught_second
]

let _ =
  run_test_tt_main suite

(* END *)
