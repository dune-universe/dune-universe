open Opti
open Expr
open Simplify
open Update
open OUnit

let expr_of_string (s: string): expr
    =
  Parser.expr_eof Lexer.token (Lexing.from_string s)

let suite = "Opti Tests" >:::
  ["assume_not_equal_1" >:: (fun _ -> assert_equal (assume_subscripts_not_equal_in_expr "i" "i2" (Expr_index_eq_ne("i","i2",Expr_const 1.0,Expr_const 0.0))) (Expr_const 0.0));
   "simplify_expr add(0,x)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "0 + x")) (expr_of_string "x"));
   "simplify_expr add(x,0)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "x + 0")) (expr_of_string "x"));
   "simplify_expr sub(0,x)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "0 - x")) (expr_of_string "-x"));
   "simplify_expr sub(x,0)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "x - 0")) (expr_of_string "x"));
   "simplify_expr mul(0,x)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "0 * x")) (expr_of_string "0"));
   "simplify_expr mul(x,0)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "x * 0")) (expr_of_string "0"));
   "simplify_expr div(0,x)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "0 / x")) (expr_of_string "0"));
   "simplify_expr div(x,1)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "x / 1")) (expr_of_string "x"));
   "simplify_expr mul(mul(a,b),0)" >:: (fun _ -> assert_equal (simplify_expr (expr_of_string "(a * b) * 0")) (expr_of_string"0"));
   "simplify_expr i1 == i2 ? 0.0 : 0.0" >:: (fun _ ->
      assert_equal (simplify_expr (Expr_index_eq_ne("i1", "i2", Expr_const 0.0, Expr_const 0.0))) (Expr_const 0.0));
   "subscript_equivalence nested" >:: (fun _ ->
     assert_equal true (expr_simplifies_to_zero_when_subscripts_not_equal
       (Expr_index_eq_ne("j0","i", Expr_index_eq_ne("i1","j", Expr_ref("delta",[]), Expr_const 0.0), Expr_const 0.0))
       ("i1","j")));
]

let _ =
    run_test_tt_main suite
