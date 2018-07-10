let test_suites: unit Alcotest.test list = [
  "Color format conversions", Color_conversion.tests
]

let () = Alcotest.run "Color tests" test_suites
