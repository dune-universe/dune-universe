type ('a, 'b) expectation = 'a * 'b

type ('a, 'b) t = {
  testable : 'a Alcotest.testable;
  loc : string;
  mutable expectations : ('a, 'b) expectation list;
}

let expect expected ~and_return:return_value = (expected, return_value)

let call t got =
  match t.expectations with
  | [] ->
      Alcotest.failf "Got call at %s but no more expectations: %a" t.loc
        (Alcotest.pp t.testable) got
  | (expected, rv) :: other_expectations ->
      t.expectations <- other_expectations;
      Alcotest.check t.testable t.loc expected got;
      rv

let check_empty t =
  let remaining = List.map fst t.expectations in
  Alcotest.check (Alcotest.list t.testable) t.loc [] remaining

let create testable loc expectations =
  let t = { testable; loc; expectations } in
  (call t, fun () -> check_empty t)
