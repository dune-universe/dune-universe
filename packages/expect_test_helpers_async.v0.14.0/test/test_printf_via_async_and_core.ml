open! Core
open! Async
open! Import

let%expect_test "{Core,Async}.printf" =
  Core.printf "Core.printf\n";
  Async.printf "Async.printf\n";
  [%expect {|
    Core.printf
    Async.printf |}]
;;

let%expect_test "{Async,Core}.printf" =
  Async.printf "Async.printf\n";
  Core.printf "Core.printf\n";
  [%expect {|
    Async.printf
    Core.printf |}]
;;
