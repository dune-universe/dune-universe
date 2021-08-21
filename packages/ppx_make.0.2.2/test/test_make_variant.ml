open Variant_types

let tuple_basic _ =
  OUnit2.assert_equal (T_baisc 32) (make_t_baisc_of_tuple_v ~v0:32 ())

let tuple_option _ =
  OUnit2.assert_equal (T_option None) (make_t_option_of_tuple_v ());
  OUnit2.assert_equal (T_option (Some 2718))
    (make_t_option_of_tuple_v ~v0:2718 ())

let tuple_complex_1 _ =
  OUnit2.assert_equal
    (T_complex (32, None, [], "", 1024, "Z06"))
    (make_t_complex_of_tuple_v ~v0:32 ~v5:"Z06" ());
  OUnit2.assert_equal
    (T_complex (32, Some 64, [ 128; 256; 512 ], "boom", 0, "Z06"))
    (make_t_complex_of_tuple_v ~v0:32 ~v1:64 ~v2:[ 128; 256; 512 ] ~v3:"boom"
       ~v4:0 ~v5:"Z06" ())

let record_basic _ =
  OUnit2.assert_equal (R_basic { b1 = 32 }) (make_r_basic_of_record_v ~b1:32 ())

let record_complex_1 _ =
  OUnit2.assert_equal
    (R_complex { c1 = 32; c2 = None; c3 = []; c4 = ""; c5 = 1024; c6 = "Z06" })
    (make_r_complex_of_record_v ~c1:32 ~c6:"Z06" ());
  OUnit2.assert_equal
    (R_complex
       {
         c1 = 32;
         c2 = Some 64;
         c3 = [ 128; 256; 512 ];
         c4 = "boom";
         c5 = 0;
         c6 = "Z06";
       })
    (make_r_complex_of_record_v ~c1:32 ~c2:64 ~c3:[ 128; 256; 512 ] ~c4:"boom"
       ~c5:0 ~c6:"Z06" ())

let record_complex_2 _ =
  OUnit2.assert_equal
    (R_complex_with_main { cm1 = 8; cm2 = None; cm3 = None })
    (make_r_complex_with_main_of_record_v 8 None);
  OUnit2.assert_equal
    (R_complex_with_main { cm1 = 8; cm2 = Some 27; cm3 = Some 64 })
    (make_r_complex_with_main_of_record_v ~cm2:27 8 (Some 64))

let none _ = OUnit2.assert_equal N_none (make_n_none_of_none_v ())

let suite =
  let open OUnit2 in
  "variant"
  >::: [
         "tuple_basic" >:: tuple_basic;
         "tuple_option" >:: tuple_option;
         "tuple_complex_1" >:: tuple_complex_1;
         "record_basic" >:: record_basic;
         "record_complex_1" >:: record_complex_1;
         "record_complex_2" >:: record_complex_2;
         "none" >:: none;
       ]
