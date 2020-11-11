open OUnit2

let test_affix_from_type_name =
  let test ~kind ~type_name ~expected ctxt =
    let actual = Ppx_factory_lib.Util.affix_from_type_name ~kind type_name in
    assert_equal ~ctxt ~cmp:[%eq: string] ~printer:[%show: string] expected actual
  in
  "affix_from_type_name" >:::
  [ "Type t" >:: test ~kind:`Suffix ~type_name:"t" ~expected:""
  ; "Suffix" >:: test ~kind:`Suffix ~type_name:"a" ~expected:"_a"
  ; "Prefix" >:: test ~kind:`Prefix ~type_name:"a" ~expected:"a_"
  ; "Preserves leading underscores" >:: test ~kind:`Suffix ~type_name:"_a" ~expected:"__a"
  ]

module List_ = struct
  let test_all_ok =
    let test ~input ~expected ctxt =
      let actual = Ppx_factory_lib.Util.List_.all_ok input in
      assert_equal ~ctxt
        ~cmp:[%eq: (int list, int) result]
        ~printer:[%show: (int list, int) result]
        expected
        actual
    in
    "all_ok" >:::
    [ "Empty" >:: test ~input:[] ~expected:(Ok [])
    ; "Ok" >:: test ~input:[Ok 0] ~expected:(Ok [0])
    ; "Error" >:: test ~input:[Error 0] ~expected:(Error 0)
    ; "Longer with error" >:: test ~input:[Ok 0; Ok 1; Error 2; Ok 3] ~expected:(Error 2)
    ; "Longer ok" >:: test ~input:[Ok 0; Ok 1; Ok 2] ~expected:(Ok [0; 1; 2])
    ]

  let test_find_ok =
    let test ~f ~input ~expected ctxt =
      let actual = Ppx_factory_lib.Util.List_.find_ok ~f input in
      assert_equal ~ctxt
        ~cmp:[%eq: (int, [`Empty | `Last of int]) result]
        ~printer:[%show: (int, [`Empty | `Last of int]) result]
        expected
        actual
    in
    "find_ok" >:::
    [ "Empty" >:: test ~f:(fun _ -> Ok 0) ~input:[] ~expected:(Error `Empty)
    ; "Return first ok" >:: test
        ~f:(fun i -> if i mod 2 = 0 then Ok i else Error i)
        ~input:[0; 1; 2; 3]
        ~expected:(Ok 0)
    ; "Return last error" >:: test
        ~f:(fun i -> Error i)
        ~input:[0; 1; 2]
        ~expected:(Error (`Last 2))
    ]

  let suite =
    "List_" >:::
    [ test_all_ok
    ; test_find_ok
    ]
end

let suite =
  "Util" >:::
  [ test_affix_from_type_name
  ; List_.suite
  ]
