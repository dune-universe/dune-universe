let test_enabled () =
  Unix.putenv "FAKE_ROWS" "1234";
  Unix.putenv "FAKE_COLS" "5678";
  Alcotest.check Alcotest.(option int) "rows" (Terminal_size.get_rows ()) (Some 1234);
  Alcotest.check Alcotest.(option int) "columns" (Terminal_size.get_columns ()) (Some 5678)

let test_disabled () =
  Unix.putenv "FAKE_ROWS" "";
  Unix.putenv "FAKE_COLS" "";
  Alcotest.check Alcotest.(option int) "rows" (Terminal_size.get_rows ()) None;
  Alcotest.check Alcotest.(option int) "columns" (Terminal_size.get_columns ()) None

let suite =
  [ ( "Terminal_size"
    , [ ("enabled", `Quick, test_enabled)
      ; ("disabled", `Quick, test_disabled)
      ]
    )
  ]

let () =
  Alcotest.run "terminal_size" suite
