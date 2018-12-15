open OUnit2;;
open Camelsnakekebab;;
(* test test case is from https://metacpan.org/pod/String::CamelSnakeKebab *) 
(* todo rewrite the test and make the test clean *)

(* test words_split *)
let test_split1 _ = assert_equal ["foo"; "bar"] (split_words "foo bar");;
let test_split2 _ = assert_equal ["foo"; "bar"] (split_words "foo\n\tbar");;
let test_split3 _ = assert_equal ["foo"; "bar"] (split_words "foo-bar");;
let test_split4 _ = assert_equal ["foo"; "Bar"] (split_words "fooBar");;
let test_split5 _ = assert_equal ["Foo"; "Bar"] (split_words "FooBar");;
let test_split6 _ = assert_equal ["foo"; "bar"] (split_words "foo_bar");; 
let test_split7 _ = assert_equal ["FOO"; "BAR"] (split_words "FOO_BAR");; 
let test_split8 _ = assert_equal ["foo1"] (split_words "foo1");; 
let test_split9 _ = assert_equal ["foo1bar"] (split_words "foo1bar");; 
let test_split10 _ = assert_equal ["foo1";"bar"] (split_words "foo1_bar");;
let test_split11 _ = assert_equal ["foo1";"Bar"] (split_words "foo1Bar");;

(* Name the test cases and group them together *)
let suite_split_words=
  "suite_case">:::
  ["test1">:: test_split1;
   "test2">:: test_split2;
   "test3">:: test_split3;
   "test4">:: test_split4;
   "test5">:: test_split5;
   "test6">:: test_split6;
   "test7">:: test_split7;
   "test8">:: test_split8;
   "test9">:: test_split9;
   "test10">:: test_split10;
   "test11">:: test_split11;
  ];;

let test_upper_camel_case _ = assert_equal "FluxCapacitor" (upper_camel_case "flux_capacitor");;
let test_lower_camel_case  _ = assert_equal "fluxCapacitor" (lower_camel_case "flux_capacitor");;
let test_lower_snake_case1 _ = assert_equal "a_snake_slithers_slyly" ( lower_snake_case "ASnakeSlithersSlyly");; 
let test_lower_snake_case2 _ = assert_equal "address1" ( lower_snake_case "address1");; 
let test_upper_snake_case _ = assert_equal "A_Snake_Slithers_Slyly" ( upper_snake_case "ASnakeSlithersSlyly");; 
let test_constant_case _ = assert_equal   "I_AM_CONSTANT" (constant_case "I am constant");;
let test_kebab_case _ =  assert_equal  "peppers-meat-pineapple" (kebab_case "Peppers_Meat_Pineapple");;
let test_http_header_case _ = assert_equal     "X-SSL-Cipher"   ( http_header_case "x-ssl-cipher");; 

let suite_convert_case =
  "suite_case_convert">:::
  ["test1">:: test_upper_camel_case;
   "test2">:: test_lower_camel_case;
   "test3">:: test_lower_snake_case1;
   "test4">:: test_lower_snake_case2;
   "test5">:: test_upper_snake_case;
   "test6">:: test_constant_case;
   "test7">:: test_kebab_case;
   "test8">:: test_http_header_case;
  ];;


let () =
  run_test_tt_main suite_split_words;
  run_test_tt_main suite_convert_case;
;;

