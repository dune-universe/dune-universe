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
  parse_string "x?; y`!"
  |> Printf.printf !"%{sexp:Ast.exp}";
  [%expect{| 
    (Seq (Assert (Test ((var x) (value true)))) (Action ((var y) (value false))))
  |}]

let%expect_test "asserts maximally expanded" =
  parse_string "x!; y`?; z? + x?; y? + y?; x? + x?; y?; z!"
  |> Printf.printf !"%{sexp:Ast.exp}";
  [%expect{| 
    (Union
     (Union
      (Seq (Action ((var x) (value true)))
       (Assert
        (Conj (Test ((var y) (value false))) (Test ((var z) (value true))))))
      (Assert
       (Disj (Conj (Test ((var x) (value true))) (Test ((var y) (value true))))
        (Conj (Test ((var y) (value true))) (Test ((var x) (value true)))))))
     (Seq
      (Assert (Conj (Test ((var x) (value true))) (Test ((var y) (value true)))))
      (Action ((var z) (value true)))))
  |}]

let%expect_test "operator precedence" =
  parse_string "if a? + b? then x! + y! else x!; y! + z!*"
  |> Printf.printf !"%{sexp:Ast.exp}";
  [%expect{| 
    (Union
     (Seq
      (Assert (Disj (Test ((var a) (value true))) (Test ((var b) (value true)))))
      (Union (Action ((var x) (value true))) (Action ((var y) (value true)))))
     (Seq
      (Assert
       (Neg (Disj (Test ((var a) (value true))) (Test ((var b) (value true))))))
      (Union
       (Seq (Action ((var x) (value true))) (Action ((var y) (value true))))
       (Star (Action ((var z) (value true)))))))
  |}]

let%expect_test "illegal" =
  Printexc.record_backtrace false;
  parse_string "this!; is illegal!; isntit?"
  |> Printf.printf !"%{sexp:Ast.exp}";
  [%expect.unreachable]
[@@expect.uncaught_exn {| ("Nice_parser.Make(P).ParseError(_, _)") |}]
