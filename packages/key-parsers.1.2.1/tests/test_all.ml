open OUnit2

let suite =
  "Key-parsers"
  >::: [ "LTPA" >::: Test_ltpa.suite
       ; "ASN1" >::: Test_asn1.suite
       ; "CVC" >::: Test_cvc.suite
       ; "PGP" >::: Test_pgp.suite ]

let _ = run_test_tt_main suite
