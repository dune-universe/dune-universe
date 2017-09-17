(* The position of these symbols is tested below. Moving them requires fixing the tests *)
[@@@ocaml.warning "-8"]
let match_failure = lazy (let 0 = 1 in 0) (*BISECT-IGNORE*)
[@@@ocaml.warning "+8"]
let assert_failure = lazy (assert false)
(* End of symbols to not move *)

module OCSP = OCamlStandard.Pervasives
module OCSL = OCamlStandard.List

include Foundations.Exception

module Tests = struct
  open Testing

  exception TestException

  module Examples = struct
    let equal = [
      [Failure "foo"];
    ]

    let different = [
      (Failure "foo", Failure "bar");
      (Failure "foo", InvalidArgument "foo");
    ]

    let repr = [
      (DivisionByZero, "Division_by_zero");
    ]

    let to_string = repr
  end

  let test = "Exception" >:: [
    (let module T = Concepts.Identifiable.Tests.Make0(Foundations.Exception)(Examples) in T.test);
    (let module T = Traits.Displayable.Tests.Make0(Foundations.Exception)(Examples) in T.test);
    "raise" >: (lazy (expect_exception ~expected:TestException (lazy (raise TestException))));
    "raise_without_backtrace" >: (lazy (expect_exception ~expected:TestException (lazy (raise_without_backtrace TestException))));
    "failure" >: (lazy (expect_exception ~expected:(Failure "Foo bar 42") (lazy (failure "Foo %s %n" "bar" 42))));
    "invalid_argument" >: (lazy (expect_exception ~expected:(Invalid_argument "Grmbl baz 43") (lazy (invalid_argument "Grmbl %s %n" "baz" 43))));
    "Aliases" >:: [
      "MatchFailure = Match_failure" >: (lazy (expect_exception ~expected:(MatchFailure ("Implementation/Exception.ml", 3, 30)) match_failure));
      "AssertFailure = Assert_failure" >: (lazy (expect_exception ~expected:(AssertFailure ("Implementation/Exception.ml", 5, 26)) assert_failure));
      "InvalidArgument = Invalid_argument" >: (lazy (expect_exception ~expected:(InvalidArgument "List.nth") (lazy (OCSL.nth [] (-1)))));
      "Failure = Failure" >: (lazy (expect_exception ~expected:(Failure "foo") (lazy (OCSP.failwith "foo"))));
      "NotFound = Not_found" >: (lazy (expect_exception ~expected:NotFound (lazy (OCSL.find (fun _ -> true) [])))); (*BISECT-IGNORE*)
      (* "OutOfMemory = Out_of_memory" >: (lazy (expect_exception ~expected:OutOfMemory (lazy ()))); *)
      "StackOverflow = Stack_overflow" >: (lazy (expect_exception ~expected:StackOverflow (lazy (let rec f x = 1 + (f x) in f 0))));
      (* "SysError = Sys_error" >: (lazy (expect_exception ~expected:SysError (lazy ()))); *)
      (* "EndOfFile = End_of_file" >: (lazy (expect_exception ~expected:EndOfFile (lazy ()))); *)
      "DivisionByZero = Division_by_zero" >: (lazy (expect_exception ~expected:DivisionByZero (lazy (1 / 0))));
      (* "SysBlockedIO = Sys_blocked_io" >: (lazy (expect_exception ~expected:SysBlockedIO (lazy ()))); *)
      (* "UndefinedRecursiveModule = Undefined_recursive_module" >: (lazy (expect_exception ~expected:UndefinedRecursiveModule (lazy ()))); *)

      "Exit = Pervasives.Exit" >: (lazy (expect_exception ~expected:Exit (lazy (raise Exit))));
    ]
  ]
end
