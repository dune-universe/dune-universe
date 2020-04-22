open OUnit2;;
open Lambda;;
open Parse;;
open L;;

let parse s t octx = 
  (* Printf.printf "%s = %s\n" (to_string t) (to_string @@ parse s); *)
  assert_equal (parse s) (parse @@ to_string @@ parse s);
  assert_equal (parse s) t;;
;;

let tlist = [
	"lambda.parse 'x'" >:: parse "x" (Var "x");
	"lambda.parse '(x y)'" >:: parse "(x y)" (App (Var "x", Var "y"));
	"lambda.parse '(λx.y)'" >:: parse "(λx.y)" (Abs ("x", Var "y"));
	"lambda.parse '(λx.(a b))'" >:: parse "(λx.(a b))" (Abs ("x", App(Var "a", Var "b")));
  "lambda.parse '(λx.(λx.(y b)))'" >:: parse "(λx.(λx.(y b)))" (Abs ("x", Abs("x", App(Var "y", Var "b"))));
  "lambda.parse '((λx.y b) (λz.w a))'" >:: parse "((λx.y b) (λz.w a))" (App(Abs ("x", App(Var "y", Var "b")),Abs ("z", App(Var "w", Var "a"))));
  "lambda.parse '(λx.λy.x)'" >:: parse "(λx.λy.x)" (Abs ("x", Abs("y", Var "x")));
  "lambda.parse '(/x./y.x)'" >:: parse "(/x./y.x)" (Abs ("x", Abs("y", Var "x")));
];;