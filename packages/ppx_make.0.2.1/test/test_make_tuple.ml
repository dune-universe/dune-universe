open Tuple_types

let basic _ = OUnit2.assert_equal (32, 44) (make_b ~v0:32 ~v1:44 ())

let option_ _ =
  OUnit2.assert_equal (None, None) (make_o ());
  OUnit2.assert_equal (Some 2718, Some 2818) (make_o ~v0:2718 ~v1:2818 ())

let list_ _ =
  OUnit2.assert_equal ([], []) (make_l ());
  OUnit2.assert_equal
    ([ 1; 1; 2; 3 ], [ 5; 8; 11; 19 ])
    (make_l ~v0:[ 1; 1; 2; 3 ] ~v1:[ 5; 8; 11; 19 ] ())

let string_ _ =
  OUnit2.assert_equal ("", "") (make_s ());
  OUnit2.assert_equal ("foo", "bar") (make_s ~v0:"foo" ~v1:"bar" ())

let default _ =
  OUnit2.assert_equal (42, 420) (make_d ());
  OUnit2.assert_equal (420, 42) (make_d ~v0:420 ~v1:42 ())

let required _ =
  OUnit2.assert_equal (None, "cov", Some 1729)
    (make_r ~v1:"cov" ~v2:(Some 1729) ())

let complex_1 _ =
  OUnit2.assert_equal
    (32, None, [], "", 1024, "Z06")
    (make_complex ~v0:32 ~v5:"Z06" ());
  OUnit2.assert_equal
    (32, Some 64, [ 128; 256; 512 ], "boom", 0, "Z06")
    (make_complex ~v0:32 ~v1:64 ~v2:[ 128; 256; 512 ] ~v3:"boom" ~v4:0 ~v5:"Z06"
       ())

let suite =
  let open OUnit2 in
  "tuple"
  >::: [
         "basic" >:: basic;
         "option" >:: option_;
         "list" >:: list_;
         "string" >:: string_;
         "default" >:: default;
         "required" >:: required;
         "complex_1" >:: complex_1;
       ]
