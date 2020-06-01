open! Core
open! Async
open! Reader

let%expect_test "macros" =
  Test_macro_blocking.make
    ~reference:(module Sexp_macro.Blocking)
    (module struct
      let load_sexp_conv_exn ?allow_includes file f =
        Thread_safe.block_on_async (fun () ->
          Sexp_macro.load_sexp_exn ?allow_includes file f)
        |> function
        | Ok x -> x
        | Error e -> raise (Monitor.extract_exn e)
      ;;

      let load_sexps_conv ?allow_includes file f =
        Thread_safe.block_on_async_exn (fun () ->
          Sexp_macro.Macro_loader.load_sexps_conv ?allow_includes file f)
      ;;

      let included_files file =
        Thread_safe.block_on_async_exn (fun () ->
          Sexp_macro.Macro_loader.included_files file)
      ;;
    end);
  [%expect
    {|
    (test simple)
    Actual output agrees with reference output.

    (test "include chain with subdirectories")
    Actual output agrees with reference output.

    (test "not all files included")
    Actual output agrees with reference output.

    (test "hello world")
    Actual output agrees with reference output.

    (test "chained let")
    Actual output agrees with reference output.

    (test "nested let1")
    Actual output agrees with reference output.

    (test "nested let2")
    Actual output agrees with reference output.

    (test "argument list scoping")
    Actual output agrees with reference output.

    (test "empty argument")
    Actual output agrees with reference output.

    (test scoping1)
    Actual output agrees with reference output.

    (test scoping2)
    Actual output agrees with reference output.

    (test scoping3)
    Actual output agrees with reference output.

    (test scoping4)
    Actual output agrees with reference output.

    (test "argument shadowing")
    Actual output agrees with reference output.

    (test "high order function attempt")
    Actual output agrees with reference output.

    (test "high order function attempt 2")
    Actual output agrees with reference output.

    (test "unused variable (inside function macro)")
    Actual output agrees with reference output.

    (test "malformed concat")
    Actual output agrees with reference output.

    (test "error evaluating macros")
    Actual output agrees with reference output.

    (test "error evaluating macros")
    Actual output agrees with reference output.

    (test "unexpected :use")
    Actual output agrees with reference output.

    (test "malformed argument")
    Actual output agrees with reference output.

    (test "argument mismatch")
    Actual output agrees with reference output.

    (test "unused variable")
    Actual output agrees with reference output.

    (test "duplicated let argument")
    Actual output agrees with reference output.

    (test "undefined variable in let")
    Actual output agrees with reference output.

    (test "undefined variable")
    Actual output agrees with reference output.

    (test ":include can cause variable capture")
    Actual output agrees with reference output.

    (test "malformed concat")
    Actual output agrees with reference output.

    (test "malformed concat")
    Actual output agrees with reference output.

    (test "correct error location in a nested let")
    Actual output agrees with reference output.

    (test "correct location with chains of includes")
    Actual output agrees with reference output.

    (test "empty let body")
    Actual output agrees with reference output.

    (test "error location for conversion errors")
    Actual output agrees with reference output.

    (test "multiple conversion errors")
    Actual output agrees with reference output.

    (test "include loop")
    Actual output agrees with reference output.

    (test "sneaky include loop" (
      files (
        (input.sexp   ((:include include.sexp)))
        (include.sexp ((:include ././include.sexp))))))
    Actual output does not agree with reference output.
    Actual:
    (raised (
      "Error in file DIR/include.sexp" (
        Unix.Unix_error
        "File name too long"
        open
        "((filename DIR/include.sexp) (mode (O_RDONLY O_CLOEXEC)) (perm 0o0))")))
    Reference:
    (raised (
      "Error in file DIR/include.sexp" (
        Sys_error "DIR/include.sexp: File name too long")))

    (test "parsing error 1")
    Actual output agrees with reference output.

    (test "parsing error 2" (
      files ((input.sexp ((:include include.sexp) ())) (include.sexp "("))))
    Actual output does not agree with reference output.
    Actual:
    (raised (
      "Error in file DIR/include.sexp" (
        "Reader.read_sexp got unexpected eof" (
          reader (
            (file_descr _)
            (info       DIR/include.sexp)
            (kind       File))))))
    Reference:
    (raised (
      Failure
      "DIR/include.sexp: Sexplib.Sexp.input_rev_sexps: reached EOF while in state Parsing_list"))

    (test "value is not interpreted as code")
    Actual output agrees with reference output.

    (test "can load without includes when they're forbidden")
    Actual output agrees with reference output.

    (test "forbidden includes raise")
    Actual output agrees with reference output. |}]
;;
