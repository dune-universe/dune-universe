let computation = Dkim.remove_signature_of_raw_dkim

let v str =
  let _, v = Unstrctrd.safely_decode str in
  v

let rem str =
  let res = Dkim.remove_signature_of_raw_dkim (v str) in
  Unstrctrd.to_utf_8_string res

let test_0 =
  Alcotest.test_case "b=sig;" `Quick @@ fun () ->
  Alcotest.(check string) "" (rem "b=sig;") "b=;"

let test_3 =
  Alcotest.test_case "b=|<wsp>sig<wsp>|;" `Quick @@ fun () ->
  Alcotest.(check string) "" (rem "b= sig ;") "b=;"

let test_4 =
  Alcotest.test_case "s=1234;|b=sig;" `Quick @@ fun () ->
  Alcotest.(check string) "" (rem "s=1234; b=sig;") "s=1234; b=;"

let test_6 =
  Alcotest.test_case "s|=1234|;|b=|sig|;" `Quick @@ fun () ->
  Alcotest.(check string) "" (rem "s=1234;b=sig;") "s=1234;b=;"

let test_7 =
  Alcotest.test_case "b=|<wsp>|s|<wsp>|ig|<wsp>|;" `Quick @@ fun () ->
  Alcotest.(check string) "" (rem "b= s ig ;") "b=;"

let () =
  Alcotest.run "dkim_field"
    [ ("signature", [ test_0; test_3; test_4; test_6; test_7 ]) ]
