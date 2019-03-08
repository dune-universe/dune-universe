open OUnit2

let test__name_from_type_name =
  let test ~input ~expected ctxt =
    let actual = Ppx_factory_lib.Factory._name_from_type_name input in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "_name_from_type_name" >:::
  [ "Is factory" >:: test ~input:"t" ~expected:"factory"
  ; "Uses right prefix" >:: test ~input:"a" ~expected:"a_factory"
  ]

let test__name_from_type_and_constructor_name =
  let test ~type_name ~constructor_name ~expected ctxt =
    let actual =
      Ppx_factory_lib.Factory._name_from_type_and_constructor_name ~type_name ~constructor_name
    in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "_name_from_type_and_constructor_name" >:::
  [ "Handle type t" >:: test ~type_name:"t" ~constructor_name:"A" ~expected:"a_factory"
  ; "Handle other type names" >:: test ~type_name:"u" ~constructor_name:"A" ~expected:"u_a_factory"
  ; "Lowercase ctr name" >:: test ~type_name:"u" ~constructor_name:"RSA" ~expected:"u_rsa_factory"
  ]

let suite =
  "Factory" >:::
  [ test__name_from_type_name
  ; test__name_from_type_and_constructor_name
  ]
