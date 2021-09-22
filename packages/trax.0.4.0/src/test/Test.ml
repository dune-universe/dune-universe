(*
   Entrypoint to run the unit tests from the command line.
*)

let test_suites : unit Alcotest.test list = [
  "Trax", Test_trax.tests;
]

let main () = Alcotest.run "trax" test_suites

let () = main ()
