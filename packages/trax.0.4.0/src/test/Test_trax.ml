(*
   Unit tests for Trax
*)

let rec grow_stack n =
  if n > 0 then
    let _, res = grow_stack (n - 1) in
    n, res
  else
    n, Printexc.get_callstack max_int

let test_deduplicate_trace () =
  let _n, raw = grow_stack 10 in
  let trace = Trax.raw_backtrace_to_string raw in
  print_string trace;
  let re = Re.str "... (skipping 9 duplicates)\n" |> Re.compile in
  match Re.matches re trace with
  | [_] -> ()
  | [] -> Alcotest.fail "no matches, should have found one"
  | _ -> Alcotest.fail "multiple matches, should have found one"

let tests = [
  "deduplicate_trace", `Quick, test_deduplicate_trace;
]
