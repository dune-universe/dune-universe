(* The position of these symbols is tested below. Moving them requires fixing the tests *)
let callstack = CallStack.current ()
(* End of symbols to not move *)

module Tests = struct
  open Foundations
  open Testing

  exception TestException0
  exception TestException0'
  exception TestException1 of string

  let () = Exception.register_printer (function
    | TestException0 -> Some "TestingTests.Tests.TestException0"
    | TestException0' -> Some "TestingTests.Tests.TestException0'"
    | TestException1 s -> Some (Format.apply "TestingTests.Tests.TestException1(%S)" s)
    | _ -> None
  )

  module ResultExamples = struct
    open Result
    open Status

    let repr = [
      (Single {label="foo"; status=Success}, "Single {label=\"foo\"; status=Success}");
      (Single {label="foo"; status=Failure (NotEqual ("a", "b"))}, "Single {label=\"foo\"; status=Failure (NotEqual (\"a\", \"b\"))}");
      (Single {label="foo"; status=Failure (NoException TestException0)}, "Single {label=\"foo\"; status=Failure (NoException TestingTests.Tests.TestException0)}");
      (Single {label="foo"; status=Failure (NoExceptionNamed "Foo")}, "Single {label=\"foo\"; status=Failure (NoExceptionNamed \"Foo\")}");
      (Single {label="foo"; status=Failure (WrongException (TestException0, TestException0', None))}, "Single {label=\"foo\"; status=Failure (WrongException (TestingTests.Tests.TestException0, TestingTests.Tests.TestException0', None))}");
      (Single {label="foo"; status=Failure (WrongExceptionNamed ("Foo", TestException0', None))}, "Single {label=\"foo\"; status=Failure (WrongExceptionNamed (\"Foo\", TestingTests.Tests.TestException0', None))}");
      (Single {label="foo"; status=Failure (Custom "bad")}, "Single {label=\"foo\"; status=Failure (Custom \"bad\")}");
      (Single {label="foo"; status=Error (TestException0, None)}, "Single {label=\"foo\"; status=Error (TestingTests.Tests.TestException0, None)}");
      (Group {name="bar"; children=[Single {label="foo"; status=Success}]}, "Group {name=\"bar\"; children=[Single {label=\"foo\"; status=Success}]}");
    ]
  end

  let test = "Testing" >:: [
    "Result" >:: [
      (let module T = Traits.Representable.Tests.Make0(Result)(ResultExamples) in T.test);
      "to_indented_strings" >:: (
        let make ?(verbose=false) expected result =
          (expected |> Foundations.List.join_string_list ~sep:"\n") >: (lazy (
            let actual =
              result
              |> Result.decorate_with_counts
              |> Result.to_indented_strings ~verbose
            in
            check_string_list ~expected actual
          ))
        in
        Result.(Status.[
          make ~verbose:true
            ["\"foo\": OK"]
            (Single {label="foo"; status=Success});
          make
            ["\"bar 1\": FAILED: expected a, but got b"]
            (Single {label="bar 1"; status=Failure (NotEqual ("a", "b"))});
          make
            ["\"bar 2\": FAILED: expected exception TestingTests.Tests.TestException0 not raised"]
            (Single {label="bar 2"; status=Failure (NoException TestException0)});
          make
            ["\"bar 2'\": FAILED: expected exception Foo not raised"]
            (Single {label="bar 2'"; status=Failure (NoExceptionNamed "Foo")});
          make
            ["\"bar 3\": FAILED: expected exception TestingTests.Tests.TestException0 not raised, but exception TestingTests.Tests.TestException0' raised (no backtrace)"]
            (Single {label="bar 3"; status=Failure (WrongException (TestException0, TestException0', None))});
          make
            ["\"bar 3'\": FAILED: expected exception Foo not raised, but exception TestingTests.Tests.TestException0' raised (no backtrace)"]
            (Single {label="bar 3'"; status=Failure (WrongExceptionNamed ("Foo", TestException0', None))});
          make
            [
              if javascript then
                "\"bar 4\": FAILED: expected exception TestingTests.Tests.TestException1(\"bad\") not raised, but exception TestingTests.Tests.TestException1(\"too bad\") raised\n"
              else
                "\"bar 4\": FAILED: expected exception TestingTests.Tests.TestException1(\"bad\") not raised, but exception TestingTests.Tests.TestException1(\"too bad\") raised\n\
                 Raised by primitive operation at file \"Implementation/TestingTests.ml\", line 2, characters 16-36\n"
            ]
            (Single {label="bar 4"; status=Failure (WrongException (TestException1 "bad", TestException1 "too bad", Some callstack))});
          make
            [
              if javascript then
                "\"bar 4'\": FAILED: expected exception Foo not raised, but exception TestingTests.Tests.TestException1(\"too bad\") raised\n"
              else
                "\"bar 4'\": FAILED: expected exception Foo not raised, but exception TestingTests.Tests.TestException1(\"too bad\") raised\n\
                 Raised by primitive operation at file \"Implementation/TestingTests.ml\", line 2, characters 16-36\n"
            ]
            (Single {label="bar 4'"; status=Failure (WrongExceptionNamed ("Foo", TestException1 "too bad", Some callstack))});
          make
            ["\"bar 5\": FAILED: too bad"]
            (Single {label="bar 5"; status=Failure (Custom "too bad")});
          make
            ["\"bar 6\": ERROR: exception TestingTests.Tests.TestException0 raised (no backtrace)"]
            (Single {label="bar 6"; status=Error (TestException0, None)});
          make
            [
              if javascript then
                "\"bar 7\": ERROR: exception TestingTests.Tests.TestException1(\"bad\") raised\n"
              else
                "\"bar 7\": ERROR: exception TestingTests.Tests.TestException1(\"bad\") raised\n\
                 Raised by primitive operation at file \"Implementation/TestingTests.ml\", line 2, characters 16-36\n"
            ]
            (Single {label="bar 7"; status=Error (TestException1 "bad", Some callstack)});
          make ~verbose:true
            [
              "\"foo\" (Successes: 2)";
              "  \"bar\": OK";
              "  \"baz\": OK";
            ]
            (Group {name="foo"; children=[Single {label="bar"; status=Success}; Single {label="baz"; status=Success}]});
          make ~verbose:false
            [
              "\"foo\" (Successes: 2)";
            ]
            (Group {name="foo"; children=[Single {label="bar"; status=Success}; Single {label="baz"; status=Success}]});
          make ~verbose:true
            [
              "\"foo\" (Successes: 1, failures: 1, errors: 0)";
              "  \"bar\": FAILED: nope";
              "  \"baz\": OK";
            ]
            (Group {name="foo"; children=[Single {label="bar"; status=Failure (Custom "nope")}; Single {label="baz"; status=Success}]});
          make ~verbose:false
            [
              "\"foo\" (Successes: 1, failures: 1, errors: 0)";
              "  \"bar\": FAILED: nope";
            ]
            (Group {name="foo"; children=[Single {label="bar"; status=Failure (Custom "nope")}; Single {label="baz"; status=Success}]});
          make
            [
              "\"foo\" (Successes: 0, failures: 0, errors: 1)";
              "  \"bar\": ERROR: exception TestingTests.Tests.TestException0 raised (no backtrace)";
            ]
            (Group {name="foo"; children=[Single {label="bar"; status=Error (TestException0, None)}]});
        ])
      );
    ];
    "Test" >:: [
      ~:: "ru%s" "n" (
        let make expected test =
          let name = match test with
            | Test.Single {Test.label; _} -> label
            | Test.Group {Test.name; _} -> name
          in
          name >: (lazy (check ~repr:Result.repr ~equal:Result.equal ~expected (Test.run ~record_backtrace:false test)))
        in
        Result.(Status.[
          make (Single {label="single success"; status=Success}) ("single success" >: (lazy ()));
          make
            (Group {name="group success"; children=[Single {label="child"; status=Success}]})
            ("group success" >:: ["child" >: (lazy ())]);
          make
            (Single {label="not equal failure"; status=Failure (NotEqual ("42", "43"))})
            ("not equal failure" >: (lazy (check_42 43)));
          make
            (Single {label="no exception"; status=Failure (NoException TestException0)})
            ("no exception" >: (lazy (expect_exception ~expected:TestException0 (lazy ()))));
          make
            (Single {label="wrong exception"; status=Failure (WrongException (TestException0, TestException0', None))})
            ("wrong exception" >: (lazy (expect_exception ~expected:TestException0 (lazy (Exception.raise TestException0')))));
          make
            (Single {label="custom failure"; status=Failure (Custom "bad")})
            ("custom failure" >: (lazy (fail "bad")));
          make
            (Single {label="error"; status=Error (TestException0, None)})
            ("error" >: (lazy (Exception.raise TestException0)));
        ])
      );
    ];
  ]
end
