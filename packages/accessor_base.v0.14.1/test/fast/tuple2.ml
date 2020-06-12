open! Core_kernel
open! Import
open Common

let%expect_test "fst" =
  field
    Accessor.Tuple2.fst
    ("a", "b")
    [%sexp_of: string]
    [%sexp_of: string * string]
    ~f:(fun str -> str ^ "z");
  [%expect {|
    (map (az b))
    (to_list (a))
    (iter (a))
    (get a) |}]
;;

let%expect_test "fsti" =
  fieldi
    Accessor.Tuple2.fsti
    ("a", "b")
    [%sexp_of: string * unit]
    [%sexp_of: string]
    [%sexp_of: (string * string) * string]
    ~f:(fun [ i ] str -> i, str);
  [%expect
    {|
    (mapi ((b a) b))
    (to_listi (((b) a)))
    (iteri (((b) a)))
    (geti ((b) a)) |}]
;;

let%expect_test "snd" =
  field
    Accessor.Tuple2.snd
    ("a", "b")
    [%sexp_of: string]
    [%sexp_of: string * string]
    ~f:(fun str -> str ^ "z");
  [%expect {|
    (map (a bz))
    (to_list (b))
    (iter (b))
    (get b) |}]
;;

let%expect_test "sndi" =
  fieldi
    Accessor.Tuple2.sndi
    ("a", "b")
    [%sexp_of: string * unit]
    [%sexp_of: string]
    [%sexp_of: string * (string * string)]
    ~f:(fun [ i ] str -> i, str);
  [%expect
    {|
    (mapi (a (a b)))
    (to_listi (((a) b)))
    (iteri (((a) b)))
    (geti ((a) b)) |}]
;;

let%expect_test "swap" =
  isomorphism
    Accessor.Tuple2.swap
    ("d", "c")
    ("a", "b")
    [%sexp_of: string * string]
    [%sexp_of: string * string]
    ~f:(fun (a, b) -> a ^ "1", b ^ "2");
  [%expect
    {|
    (map (a2 b1))
    (to_list ((b a)))
    (iter ((b a)))
    (get (b a))
    (get_option ((b a)))
    (to_list ((b a)))
    (iter ((b a)))
    (map (a2 b1))
    (to_list ((b a)))
    (iter ((b a)))
    (construct (c d)) |}]
;;

let%expect_test "assocl" =
  isomorphism
    Accessor.Tuple2.assocl
    (("d", "e"), "f")
    ("a", ("b", "c"))
    [%sexp_of: (string * string) * string]
    [%sexp_of: string * (string * string)]
    ~f:(fun ((a, b), c) -> (a ^ "1", b ^ "2"), c ^ "3");
  [%expect
    {|
    (map (a1 (b2 c3)))
    (to_list (((a b) c)))
    (iter (((a b) c)))
    (get ((a b) c))
    (get_option (((a b) c)))
    (to_list (((a b) c)))
    (iter (((a b) c)))
    (map (a1 (b2 c3)))
    (to_list (((a b) c)))
    (iter (((a b) c)))
    (construct (d (e f))) |}]
;;

let%expect_test "assocr" =
  isomorphism
    Accessor.Tuple2.assocr
    ("d", ("e", "f"))
    (("a", "b"), "c")
    [%sexp_of: string * (string * string)]
    [%sexp_of: (string * string) * string]
    ~f:(fun (a, (b, c)) -> a ^ "1", (b ^ "2", c ^ "3"));
  [%expect
    {|
    (map ((a1 b2) c3))
    (to_list ((a (b c))))
    (iter ((a (b c))))
    (get (a (b c)))
    (get_option ((a (b c))))
    (to_list ((a (b c))))
    (iter ((a (b c))))
    (map ((a1 b2) c3))
    (to_list ((a (b c))))
    (iter ((a (b c))))
    (construct ((d e) f)) |}]
;;

let%expect_test "each" =
  many
    Accessor.Tuple2.each
    ("a", "b")
    [%sexp_of: string]
    [%sexp_of: string * string]
    ~f:(fun str -> str ^ "z");
  [%expect {|
    (map (az bz))
    (to_list (a b))
    (iter (a b)) |}]
;;

type example = { foo : int } [@@deriving accessors, sexp_of]

let%expect_test "Fst.map and Snd.map" =
  let example = { foo = 1 } in
  let result = Accessor.Tuple2.Fst.map foo example ~f:(fun a -> succ a, a) in
  print_s [%sexp (result : example * int)];
  [%expect {| (((foo 2)) 1) |}];
  let result = Accessor.Tuple2.Snd.map foo example ~f:(fun a -> a, succ a) in
  print_s [%sexp (result : int * example)];
  [%expect {| (1 ((foo 2))) |}]
;;

let%expect_test "exists" =
  let f tuple = Accessor.exists Accessor.Tuple2.each tuple ~f:Fn.id in
  print_s [%sexp (f (false, false) : bool)];
  [%expect {| false |}];
  print_s [%sexp (f (false, true) : bool)];
  [%expect {| true |}];
  print_s [%sexp (f (true, false) : bool)];
  [%expect {| true |}];
  print_s [%sexp (f (true, true) : bool)];
  [%expect {| true |}]
;;

let%expect_test "for_all" =
  let f tuple = Accessor.for_all Accessor.Tuple2.each tuple ~f:Fn.id in
  print_s [%sexp (f (false, false) : bool)];
  [%expect {| false |}];
  print_s [%sexp (f (false, true) : bool)];
  [%expect {| false |}];
  print_s [%sexp (f (true, false) : bool)];
  [%expect {| false |}];
  print_s [%sexp (f (true, true) : bool)];
  [%expect {| true |}]
;;
