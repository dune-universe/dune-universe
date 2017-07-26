let test_something_case () = ()

let test_something = [
  "something", `Quick, test_something_case;
]

let () =
  Logs.set_reporter (Logs_fmt.reporter ());
  Alcotest.run "hvsock" [
    "something", test_something;
  ]
