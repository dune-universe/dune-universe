open! Core
open Poly
open! Async
open! Import

let%expect_test "[within_temp_dir `In_temp_as]" =
  let jbuild = "jbuild" in
  let foobar = "foobar" in
  let%bind jbuild_contents = Reader.file_contents jbuild in
  let%bind () =
    within_temp_dir
      ~links:[ jbuild, `In_temp_as, foobar ]
      (fun () ->
         let%bind foobar_contents = Reader.file_contents "foobar" in
         require
           [%here]
           (jbuild_contents = foobar_contents)
           ~if_false_then_print_s:
             (lazy [%message "" (jbuild_contents : string) (foobar_contents : string)]);
         return ())
  in
  [%expect {|
    |}]
;;

let%expect_test "[within_temp_dir `In_path_as]" =
  let%bind () =
    within_temp_dir
      ~links:[ "inline_tests_runner.exe", `In_path_as, "foobar" ]
      (fun () ->
         let%bind () = system "which foobar >/dev/null && echo ok" in
         return ())
  in
  [%expect {|
     ok |}]
;;
