open OUnit2
open Dual

let d1 = dual 2.1
let d2 = dual 1.1
let d3 = dual 0.8

let test_add _ =
  let dt = dual 3.2 ~y:2.0 in
  let dd = add d1 d2 in
  assert_bool "add: [FAIL]" (equal dt dd)  

let test_sub _ =
  let dt = dual 1.0 ~y:0.0 in
  let dd = sub d1 d2 in
  assert_bool "sub: [FAIL]" (equal dt dd)    

let test_neg _ =
  let dt = dual (-. 2.1) ~y:(-. 1.0) in
  let dd = neg d1 in
  assert_bool "neg: [FAIL]" (equal dt dd)    

let test_mul _ =
  let dt = dual (2.1 *. 1.1) ~y:3.2 in
  let dd = mul d1 d2 in
  assert_bool "mul: [FAIL]" (equal dt dd)      

let test_div _ =
  let dt = dual (2.1 /. 1.1) ~y:(-. 1.0 /. (1.1 *. 1.1)) in
  let dd = div d1 d2 in
  assert_bool "div: [FAIL]" (equal dt dd)

let test_pow _ =
  let dt = dual (Float.pow 2.1 1.1) ~y:(1.1 *. (Float.pow 2.1 0.1)) in
  let dd = pow d1 1.1 in
  assert_bool "pow: [FAIL]" (equal dt dd)        

let test_exp _ =
  let e1 = Float.exp(2.1) in
  let dt = dual e1 ~y:e1 in
  let dd = exp d1 in
  assert_bool "exp: [FAIL]" (equal dt dd)          

let test_sin _ =
  let dt = dual (Float.sin 2.1) ~y:(Float.cos 2.1) in
  let dd = sin d1 in
  assert_bool "sin: [FAIL]" (equal dt dd)          

let test_cos _ =
  let dt = dual (Float.cos 2.1) ~y:(-. (Float.sin 2.1)) in
  let dd = cos d1 in
  assert_bool "cos: [FAIL]" (equal dt dd)            

let test_tan _ =
  let dt = dual (Float.tan 2.1) ~y:(1.0 /. (Float.pow (Float.cos 2.1) 2.0)) in
  let dd = tan d1 in
  assert_bool "tan: [FAIL]" (equal dt dd)            

let test_asin _ =
  let dt = dual (Float.asin 0.8) ~y:(1.0 /. (Float.sqrt (1.0 -. 0.8 *. 0.8))) in
  let dd = asin d3 in
  assert_bool "asin: [FAIL]" (equal dt dd)            

let test_acos _ =
  let dt = dual (Float.acos 0.8) ~y:(-. 1.0 /. (Float.sqrt (1.0 -. 0.8 *. 0.8))) in
  let dd = acos d3 in
  assert_bool "acos: [FAIL]" (equal dt dd)            

let test_atan _ =
  let dt = dual (Float.atan 0.8) ~y:(1.0 /. (1.0 +. 0.8 *. 0.8)) in
  let dd = atan d3 in
  assert_bool "atan: [FAIL]" (equal dt dd)            

let test_sinh _ =
  let dt = dual (Float.sinh 0.8) ~y:(Float.cosh 0.8) in
  let dd = sinh d3 in
  assert_bool "sinh: [FAIL]" (equal dt dd)            

let test_cosh _ =
  let dt = dual (Float.cosh 0.8) ~y:(Float.sinh 0.8) in
  let dd = cosh d3 in
  assert_bool "cosh: [FAIL]" (equal dt dd)

let test_tanh _ =
  let tanhx = Float.tanh 0.8 in
  let dt = dual tanhx ~y:(1.0 -. (Float.pow tanhx 2.0)) in
  let dd = tanh d3 in
  assert_bool "tanh: [FAIL]" (equal dt dd)            

let test_root _ =
  let f x =
    Float.exp (-. x) *. (Float.pow x 2.2) -. 0.1 *. x
  in
  let fd x =
    sub (mul (exp (neg x)) (pow x 2.2)) (mul (dual 0.1) x)
  in
  let x0 = dual 2.0 in
  let out = root fd x0 in
  assert_bool "root: [FAIL]" (Float.abs (f out.re) < (Float.sqrt Float.epsilon))

let suite =
  "Dual Test" >::: [
    "test_add" >:: test_add;
    "test_sub" >:: test_sub;
    "test_neg" >:: test_neg;    
    "test_mul" >:: test_mul;
    "test_div" >:: test_div;
    "test_pow" >:: test_pow;
    "test_exp" >:: test_exp;
    "test_sin" >:: test_sin;
    "test_cos" >:: test_cos;
    "test_tan" >:: test_tan;
    "test_asin" >:: test_asin;
    "test_acos" >:: test_acos;
    "test_atan" >:: test_atan;
    "test_sinh" >:: test_sinh;
    "test_cosh" >:: test_cosh;
    "test_tanh" >:: test_tanh;
    "test_root" >:: test_root;        
  ]

let () =
  run_test_tt_main suite
