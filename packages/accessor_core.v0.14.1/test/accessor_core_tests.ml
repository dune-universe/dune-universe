open! Core_kernel
open! Import

let example =
  List.init 26 ~f:(fun i -> Char.of_int_exn (i + Char.to_int 'a'), i)
  |> Char.Map.of_alist_exn
;;

let print m = print_s [%sexp (m : int Char.Map.t)]

let%expect_test "just showing the example" =
  print example;
  [%expect
    {|
    ((a 0) (b 1) (c 2) (d 3) (e 4) (f 5) (g 6) (h 7) (i 8) (j 9) (k 10) (l 11)
     (m 12) (n 13) (o 14) (p 15) (q 16) (r 17) (s 18) (t 19) (u 20) (v 21)
     (w 22) (x 23) (y 24) (z 25)) |}]
;;

let set_of_string string = Char.Set.of_list (String.to_list string)

let%expect_test "at_key_seti" =
  let accessor = Accessor.Map.at_key_seti (set_of_string "abc0ijk1xyz") in
  Accessor.to_listi accessor example
  |> [%sexp_of: ((char * unit) Accessor.Index.t * int option) list]
  |> print_s;
  [%expect
    {|
    (((0) ()) ((1) ()) ((a) (0)) ((b) (1)) ((c) (2)) ((i) (8)) ((j) (9))
     ((k) (10)) ((x) (23)) ((y) (24)) ((z) (25))) |}];
  print (Accessor.set accessor example ~to_:None);
  [%expect
    {|
    ((d 3) (e 4) (f 5) (g 6) (h 7) (l 11) (m 12) (n 13) (o 14) (p 15) (q 16)
     (r 17) (s 18) (t 19) (u 20) (v 21) (w 22)) |}]
;;

let%expect_test "found_key_seti" =
  let accessor = [%accessor Accessor.Map.found_key_seti (set_of_string "abc0ijk1xyz")] in
  Accessor.to_listi accessor example
  |> [%sexp_of: ((char * unit) Accessor.Index.t * int) list]
  |> print_s;
  [%expect
    {|
    (((a) 0) ((b) 1) ((c) 2) ((i) 8) ((j) 9) ((k) 10) ((x) 23) ((y) 24) ((z) 25)) |}];
  print
    (Accessor.mapi accessor example ~f:(fun [ c ] n ->
       n - (Char.to_int c - Char.to_int 'a')));
  [%expect
    {|
    ((a 0) (b 0) (c 0) (d 3) (e 4) (f 5) (g 6) (h 7) (i 0) (j 0) (k 0) (l 11)
     (m 12) (n 13) (o 14) (p 15) (q 16) (r 17) (s 18) (t 19) (u 20) (v 21)
     (w 22) (x 0) (y 0) (z 0)) |}]
;;
