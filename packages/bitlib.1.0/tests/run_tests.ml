let test_suites: unit Alcotest.test list = [
  "Bitlib", Test_bitlib.tests
]

let () = Alcotest.run "bitlib" test_suites
