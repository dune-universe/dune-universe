open Option_types

let basic _ =
  OUnit2.assert_equal None @@ make_a ();
  OUnit2.assert_equal (Some 1) @@ make_a ~value:1 ()

let default _ =
  OUnit2.assert_equal (Some 7) @@ make_d ();
  OUnit2.assert_equal (Some 1) @@ make_d ~value:1 ()

let suite =
  let open OUnit2 in
  "option" >::: [ "basic" >:: basic; "default" >:: default ]
