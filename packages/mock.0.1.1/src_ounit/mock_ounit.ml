let rec cmp_list cmp a b =
  match (a, b) with
  | ([], []) -> true
  | ([], _::_) -> false
  | (_::_, []) -> false
  | ha::ta, hb::tb -> cmp ha hb && cmp_list cmp ta tb

let list_printer show l =
  "[" ^ String.concat ", " (List.map show l) ^ "]"

let assert_calls_are ~ctxt ~cmp ~printer expected mock =
  let got = Mock.recorded_calls mock in
  OUnit2.assert_equal
    ~ctxt
    ~cmp:(cmp_list cmp)
    ~printer:(list_printer printer)
    expected
    got

let assert_called_once_with ~ctxt ~cmp ~printer expected mock =
  let calls = Mock.recorded_calls mock in
  match calls with
  | []
    ->
    Printf.ksprintf
      OUnit2.assert_failure
      "expected %s to be called, but it was never called"
      (Mock.name mock)
  | _::_::_
    ->
    Printf.ksprintf
      OUnit2.assert_failure
      "expected %s to be called once, but it was called %d times"
      (Mock.name mock)
      (List.length calls)
  | [got]
    ->
    let msg =
      Printf.sprintf
        "when comparing %s calls"
        (Mock.name mock)
    in
    OUnit2.assert_equal
      ~ctxt
      ~msg
      ~cmp
      ~printer
      expected
      got
