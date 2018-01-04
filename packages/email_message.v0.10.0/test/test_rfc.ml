open! Core
open Async
open Email_message.Private
open Expect_test_helpers

open Rfc.RFC2045.Token

let%expect_test "RFC2045.Token" =
  let is_valid_or_quote strs =
    let results = List.map strs ~f:(fun str -> str, is_valid_or_quote str) in
    print_s [%message "" ~_:(results : (string * string) list)]
  in
  is_valid_or_quote
    ["abcdefghijkl"
    ; "abc=dka"
    ; ""
    ; "\""
    ];
  let%bind () =
  [%expect {|
    ((abcdefghijkl abcdefghijkl)
     (abc=dka      "\"abc=dka\"")
     (""           "\"\"")
     ("\""         "\"\\\"\"")) |}]
  in
  let is_valid strs =
    let results = List.map strs ~f:(fun str -> str, `Is_valid (is_valid str)) in
    print_s [%message "" ~_:(results : (string * [`Is_valid of bool]) list)]
  in
  is_valid
    [ "abcdefghijkl"
    ; "LoremIpsum"
    ; "3.141"
    ; "---"
    ; "a"
    ; "abc=dka"
    ; ""
    ; "\""
    ; "@"
    ; ","
    ; ":"
    ; ";"
    ; "\x00"
    ; "\x7F"
    ; "\x80"
    ];
  [%expect {|
    ((abcdefghijkl (Is_valid true))
     (LoremIpsum   (Is_valid true))
     (3.141        (Is_valid true))
     (---          (Is_valid true))
     (a            (Is_valid true))
     (abc=dka      (Is_valid false))
     (""           (Is_valid false))
     ("\""         (Is_valid false))
     (@            (Is_valid false))
     (,            (Is_valid false))
     (:            (Is_valid false))
     (";"          (Is_valid false))
     ("\000"       (Is_valid false))
     ("\127"       (Is_valid false))
     ("\128"       (Is_valid false))) |}]
;;
