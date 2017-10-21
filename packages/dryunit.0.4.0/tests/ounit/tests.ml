open OUnit2
open Foo

let test1 test_ctxt =
  assert_equal "x" (Char.escaped 'x')

let test2 test_ctxt =
  assert_equal 100 (Foo.unity 100)
