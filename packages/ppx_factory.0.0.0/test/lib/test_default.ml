open OUnit2

let test__name_from_type_name =
  let test ~input ~expected ctxt =
    let actual = Ppx_factory_lib.Default._name_from_type_name input in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "_name_from_type_name" >:::
  [ "Is default" >:: test ~input:"t" ~expected:"default"
  ; "Uses right suffix" >:: test ~input:"a" ~expected:"default_a"
  ]

let suite =
  "Default" >:::
  [ test__name_from_type_name
  ]
