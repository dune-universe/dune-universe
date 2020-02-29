let assert_eq eq print a b =
  if not (eq a b) then
    failwith (Format.asprintf "Assertion failure: %a != %a.@." print a print b)
