open Record_types

let basic _ = OUnit2.assert_equal { b1 = 32 } (make_b ~b1:32 ())

let option_ _ =
  OUnit2.assert_equal { o1 = None } (make_o ());
  OUnit2.assert_equal { o1 = Some 2718 } (make_o ~o1:2718 ())

let list_ _ =
  OUnit2.assert_equal { l1 = [] } (make_l ());
  OUnit2.assert_equal { l1 = [ 1; 1; 2; 3 ] } (make_l ~l1:[ 1; 1; 2; 3 ] ())

let string_ _ =
  OUnit2.assert_equal { s1 = "" } (make_s ());
  OUnit2.assert_equal { s1 = "foo" } (make_s ~s1:"foo" ())

let default _ =
  OUnit2.assert_equal { answer = 42 } (make_d ());
  OUnit2.assert_equal { answer = 420 } (make_d ~answer:420 ())

let main _ = OUnit2.assert_equal { m1 = 127; m2 = 8128 } (make_m 127 8128)

let required _ =
  OUnit2.assert_equal
    { r1 = None; r2 = "cov"; r3 = Some 1729 }
    (make_r ~r2:"cov" ~r3:(Some 1729) ())

let complex_1 _ =
  OUnit2.assert_equal
    { c1 = 32; c2 = None; c3 = []; c4 = ""; c5 = 1024; c6 = "Z06" }
    (make_complex ~c1:32 ~c6:"Z06" ());
  OUnit2.assert_equal
    {
      c1 = 32;
      c2 = Some 64;
      c3 = [ 128; 256; 512 ];
      c4 = "boom";
      c5 = 0;
      c6 = "Z06";
    }
    (make_complex ~c1:32 ~c2:64 ~c3:[ 128; 256; 512 ] ~c4:"boom" ~c5:0 ~c6:"Z06"
       ())

let complex_2 _ =
  OUnit2.assert_equal
    { cm1 = 8; cm2 = None; cm3 = None }
    (make_complex_with_main 8 None);
  OUnit2.assert_equal
    { cm1 = 8; cm2 = Some 27; cm3 = Some 64 }
    (make_complex_with_main ~cm2:27 8 (Some 64))

let suite =
  let open OUnit2 in
  "record"
  >::: [
         "basic" >:: basic;
         "option" >:: option_;
         "list" >:: list_;
         "string" >:: string_;
         "default" >:: default;
         "required" >:: required;
         "main" >:: main;
         "complex_1" >:: complex_1;
         "complex_2" >:: complex_2;
       ]
