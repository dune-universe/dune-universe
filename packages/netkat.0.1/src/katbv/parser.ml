include Nice_parser.Make(struct
  type result = Ast.exp
  type token = Menhir_parser.token
  exception ParseError = Menhir_parser.Error
  let parse = Menhir_parser.exp_eof
  include Lexer
end)

(* Expectation tests, see
   https://dune.readthedocs.io/en/stable/tests.html#inline-expectation-tests *)

let%expect_test "test then action" =
  parse_string "x=0?; y:=01"
  |> Printf.printf !"%{sexp:Ast.exp}";
  [%expect{| 
    (Seq (Assert (Test (Test x ((1 ())) ()))) (Action (y ((1 ())) ((0 ())))))
  |}]

let%expect_test "asserts maximally expanded" =
  parse_string "x:=1; y=00; z=10 + x=0; y=0 + y=0; x=1 + x=1; y=0; z:=10"
  |> Printf.printf !"%{sexp:Ast.exp}";
  [%expect{|
    (Union
     (Union
      (Seq (Action (x () ((0 ()))))
       (Assert
        (Conj (Test (Test y ((0 ()) (1 ())) ()))
         (Test (Test z ((0 ())) ((1 ())))))))
      (Assert
       (Disj (Conj (Test (Test x ((0 ())) ())) (Test (Test y ((0 ())) ())))
        (Conj (Test (Test y ((0 ())) ())) (Test (Test x () ((0 ()))))))))
     (Seq (Assert (Conj (Test (Test x () ((0 ())))) (Test (Test y ((0 ())) ()))))
      (Action (z ((0 ())) ((1 ()))))))
  |}]

let%expect_test "operator precedence" =
  parse_string "if a=?0 + b=1?1 then x:=101 + y:=100 else x:=10?1; y:=1 + z:=00*"
  |> Printf.printf !"%{sexp:Ast.exp}";
  [%expect{| 
  (Union
   (Seq
    (Assert
     (Disj (Test (Test a ((0 ())) ())) (Test (Test b () ((0 ()) (2 ()))))))
    (Union (Action (x ((1 ())) ((0 ()) (2 ()))))
     (Action (y ((0 ()) (1 ())) ((2 ()))))))
   (Seq
    (Assert
     (Neg
      (Disj (Test (Test a ((0 ())) ())) (Test (Test b () ((0 ()) (2 ())))))))
    (Union (Seq (Action (x ((2 ())) ((0 ()) (3 ())))) (Action (y () ((0 ())))))
     (Star (Action (z ((0 ()) (1 ())) ()))))))
  |}]