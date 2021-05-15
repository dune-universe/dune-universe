open OUnit2

let suite =
  "Key-parsers" >:::
  [ "LTPA" >::: Test_ltpa.suite
  ; "ASN1" >::: Test_asn1.suite
  ; "CVC" >::: Test_cvc.suite
  ]

let _ = run_test_tt_main suite
