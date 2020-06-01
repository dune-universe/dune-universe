open! Core
open! Async
open Expect_test_helpers_core
open Expect_test_helpers_async

let%expect_test "sexp command" =
  let sexp = "../../../../app/sexp/bin/main.exe" in
  (* We create a temp directory here, so any files we create will get blown away at the
     end of the test.  If you want to keep the files around, set the KEEP_EXPECT_TEST_DIR
     environment variable when running the inline-test runner from the command line. *)
  with_temp_dir (fun dir ->
    let sexp_file = dir ^/ "foo.sexp" in
    let%bind () =
      Writer.save_sexp
        sexp_file
        (Sexp.of_string
           {|((This (is an s-expression)
                           ((s a) (b c) (d e)))
                           (a b c d e f g h i j k l m n o p q r s
                            t u v w x y z))|})
    in
    (* [run] is a good default for shell invocations, since it doesn't expose you to any
       escaping problems. *)
    let%bind () = run "cat" [ dir ^/ "foo.sexp" ] in
    let%bind () =
      [%expect
        {|
      ((This (is an s-expression) ((s a) (b c) (d e)))
       (a b c d e f g h i j k l m n o p q r s t u v w x y z))
    |}]
    in
    (* We use [system] here because we want to do some piping, which is awkward
       otherwise *)
    let%bind () = system (sprintf "cat %s | %s pp" sexp_file sexp) in
    let%bind () =
      [%expect
        {|
      ((This
         (is an s-expression)
         ((s a)
          (b c)
          (d e)))
       (a b c d e f g h i j k l m n o p q r s t u v w x y z))
    |}]
    in
    let%bind () = system (sprintf "cat %s | %s print -machine" sexp_file sexp) in
    let%bind () =
      [%expect
        {|
      ((This(is an s-expression)((s a)(b c)(d e)))(a b c d e f g h i j k l m n o p q r s t u v w x y z))
    |}]
    in
    show_raise (fun () -> raise_s [%message "foo" 13]);
    let%bind () = [%expect {|
      (raised (foo 13))
    |}] in
    show_raise ignore;
    [%expect {|
      "did not raise"
    |}])
;;
