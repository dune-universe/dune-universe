open OUnit

let test_in_secondary test_ctxt =
  assert_equal 100 (Foo.unity 100)
