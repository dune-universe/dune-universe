open Poly_types

let option_ _ =
  OUnit2.assert_equal (Some 123) (make_poly_opt ~value:123 ());
  OUnit2.assert_equal (Some false) (make_poly_opt ~value:false ());
  OUnit2.assert_equal None (make_poly_opt ())

let tuple _ =
  OUnit2.assert_equal (123, 456) (make_poly_tuple ~v0:123 ~v1:456 ());
  OUnit2.assert_equal (false, 456) (make_poly_tuple ~v0:false ~v1:456 ())

let record _ =
  OUnit2.assert_equal
    { r_a = 123; r_b = false; r_int = 456 }
    (make_poly_rec ~r_a:123 ~r_b:false ~r_int:456 ());
  OUnit2.assert_equal
    { r_a = 12.3; r_b = "false"; r_int = 456 }
    (make_poly_rec ~r_a:12.3 ~r_b:"false" ~r_int:456 ())

let variant _ =
  OUnit2.assert_equal (Ok 123) (make_ok_of_poly_var ~v0:123 ());
  OUnit2.assert_equal (Ok false) (make_ok_of_poly_var ~v0:false ());
  OUnit2.assert_equal (Error 12.3) (make_error_of_poly_var ~v0:12.3 ());
  OUnit2.assert_equal (Error "false") (make_error_of_poly_var ~v0:"false" ());
  OUnit2.assert_equal Other (make_other_of_poly_var ())

let suite =
  let open OUnit2 in
  "polymorphic"
  >::: [
         "option" >:: option_;
         "tuple" >:: tuple;
         "record" >:: record;
         "variant" >:: variant;
       ]
