open OUnit2

module Derivable_Cstruct = struct
  let to_yojson_suite =
    let test ~expected ~cstruct ctxt =
      assert_equal ~ctxt expected (Key_parsers.Derivable.Cstruct.to_yojson cstruct)
    in
    [ "Empty" >:: test ~expected:(`String "0x") ~cstruct:(Cstruct.of_string "")
    ; "Non empty" >:: test ~expected:(`String "0x616263") ~cstruct:(Cstruct.of_string "abc")
    ]

  let of_yojson_suite =
    let test ~expected ~yojson ctxt =
      let cmp = [%eq: (Cstruct.t, string) Result.result] in
      let printer = [%show: (Key_parsers.Derivable.Cstruct.t, string) Result.result] in
      assert_equal ~ctxt ~cmp ~printer expected (Key_parsers.Derivable.Cstruct.of_yojson yojson)
    in
    [ "Not a string" >::
      test
        ~expected:(Result.Error "Key_parsers.Cstruct.of_yojson: expected json string")
        ~yojson:(`List [])
    ; "Not an hex string" >::
      test
        ~expected:(Result.Error "Key_parsers.Cstruct.of_yojson: expected hex encoded json string")
        ~yojson:(`String "hello!")
    ; "Empty string" >::
      test ~expected:(Result.Ok (Cstruct.of_string "")) ~yojson:(`String "")
    ; "0x" >::
      test ~expected:(Result.Ok (Cstruct.of_string "")) ~yojson:(`String "0x")
    ; "Hex encoded string" >::
      test ~expected:(Result.Ok (Cstruct.of_string "abc")) ~yojson:(`String "0x616263")
    ]

  let suite =
    [ "to_yojson" >::: to_yojson_suite
    ; "of_yojson" >::: of_yojson_suite
    ]
end

let suite =
  [ "Cstruct" >::: Derivable_Cstruct.suite
  ]
