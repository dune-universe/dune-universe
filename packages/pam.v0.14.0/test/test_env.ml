open! Core
open Import
open Pam
open Expect_test_helpers_core

let key = "OCAML_PAM_TEST"

let%expect_test "pam_getenv non-existent key" =
  let result = with_pam ~f:(pam_getenv ~key) () in
  print_s [%message "" ~_:(result : string Or_error.t)];
  [%expect {| (Error "[pam_getenv] pam_getenv failed") |}]
;;

let%expect_test "pam_putenv" =
  let result =
    with_pam
      ~f:(fun t ->
        let open Or_error.Let_syntax in
        let%bind () = pam_putenv t ~key ~data:"expect_test" in
        pam_getenv t ~key)
      ()
  in
  print_s [%message "" ~_:(result : string Or_error.t)];
  [%expect {| (Ok expect_test) |}]
;;

let%expect_test "pam_putenv multiple times" =
  let result =
    with_pam
      ~f:(fun t ->
        let open Or_error.Let_syntax in
        let%bind () = pam_putenv t ~key ~data:"expect_test" in
        let%bind () = pam_putenv t ~key ~data:"expect_test_again" in
        pam_getenv t ~key)
      ()
  in
  print_s [%message "" ~_:(result : string Or_error.t)];
  [%expect {| (Ok expect_test_again) |}]
;;

let%expect_test "pam_unsetenv" =
  let environment =
    with_pam_exn
      ~f:(fun t ->
        let open Or_error.Let_syntax in
        let%bind () = pam_putenv t ~key ~data:"expect_test" in
        let%bind () = pam_unsetenv t ~key in
        pam_getenvlist t)
      ()
  in
  print_s [%message (environment : string list)];
  [%expect {| (environment ()) |}]
;;

let%expect_test "empty env list" =
  let environment = with_pam_exn ~f:pam_getenvlist () in
  print_s [%message (environment : string list)];
  [%expect {| (environment ()) |}]
;;

let%expect_test "pam_getenvlist" =
  let environment =
    with_pam_exn
      ~f:(fun t ->
        let open Or_error.Let_syntax in
        let%bind () =
          List.map (List.range 1 10) ~f:(fun i ->
            let key = sprintf "PAM_TEST_%d" i in
            let data = sprintf "expect_test_%d" i in
            pam_putenv t ~key ~data)
          |> Or_error.combine_errors_unit
        in
        pam_getenvlist t)
      ()
  in
  print_s [%message (environment : string list)];
  [%expect
    {|
   (environment (
     PAM_TEST_1=expect_test_1
     PAM_TEST_2=expect_test_2
     PAM_TEST_3=expect_test_3
     PAM_TEST_4=expect_test_4
     PAM_TEST_5=expect_test_5
     PAM_TEST_6=expect_test_6
     PAM_TEST_7=expect_test_7
     PAM_TEST_8=expect_test_8
     PAM_TEST_9=expect_test_9))
  |}]
;;
