open Test_util
open Obeam.Abstract_format

(* We cannot use `let%expect_test` because it marks unreachable [%expect {| |}] as [%expect {| DID NOT REACH THIS PROGRAM POINT |}],
 * So we use `let%test` which is more primitive than `let%expect_test`.
 *)
let%test "test_try_stacktrace.beam" =
  if otp_version () >= 21 then
    let dummy_clause =
      ClsCatch {line=20; line_cls=20; line_stacktrace=20;
                exception_class=AtomVarAtom {line=20; atom="throw"};
                pattern=PatLit {lit=LitAtom {line=20; atom="dummy"}};
                stacktrace="_";
                guard_sequence=None;
                body=ExprBody {exprs=[ExprLit {lit=LitAtom {line=20; atom="dummy"}}]}}
    in
    let stacktrace_with_guard_sequence_clause =
      ClsCatch {line=21; line_cls=21; line_stacktrace=21;
                exception_class=AtomVarVar {line=21; id="Class"};
                pattern=PatVar {line=21; id="Err"};
                stacktrace="Stacktrace";
                guard_sequence=Some (GuardSeq {guards=[Guard {guard_tests=[GuardTestBinOp {line=21; op="=:=";
                                                                                           lhs=GuardTestVar {line=21; id="Err"};
                                                                                           rhs=GuardTestLit {lit=LitAtom {line=21; atom="error"}}}]}]});
                body=ExprBody {exprs=[ExprTuple {line=21; elements=[ExprVar {line=21; id="Class"};
                                                                    ExprVar {line=21; id="Err"};
                                                                    ExprVar {line=21; id="Stacktrace"}]}]}}
    in
    let stacktrace_clause =
      ClsCatch {line=22; line_cls=22; line_stacktrace=22;
                exception_class=AtomVarVar {line=22; id="Class"};
                pattern=PatVar {line=22; id="Err"};
                stacktrace="Stacktrace";
                guard_sequence=None;
                body=ExprBody {exprs=[ExprTuple {line=22; elements=[ExprVar {line=22; id="Class"};
                                                                    ExprVar {line=22; id="Err"};
                                                                    ExprVar {line=22; id="Stacktrace"}]}]}}
    in
    let expect =
      Ok
        (AbstractCode
           (ModDecl
              [AttrFile {line=1; file="test_try_stacktrace.erl"; file_line=1};
               AttrMod {line=1; module_name="test_try_stacktrace"};
               AttrExport {line=3; function_arity_list=[("catch_stacktrace", 0)]};
               DeclFun {line=16; function_name="catch_stacktrace"; arity=0;
                        clauses=[ClsFun {line=16; patterns=[]; guard_sequence=None;
                                         body=ExprBody {exprs=[ExprTry {line=17;
                                                                        exprs=[ExprLit {lit=LitAtom{line=18; atom="error"}}];
                                                                        case_clauses=[];
                                                                        catch_clauses=[dummy_clause;
                                                                                       stacktrace_with_guard_sequence_clause;
                                                                                       stacktrace_clause];
                                                                        after=[]}]}}]};
               FormEof]))
    in
    load_ast_from_beam "test_try_stacktrace.beam" = expect;
  else
     true
