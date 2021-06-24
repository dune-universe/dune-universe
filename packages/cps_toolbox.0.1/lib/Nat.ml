open Functional

let zero = 0
let succ n = n + 1

let fold zero_case succ_case n =
  if n <= 0 then zero_case else
  let rec _visit n return =
    match n with
    | 0 -> return zero_case
    | _ -> _visit (n - 1) (return <== succ_case)
  in
  _visit n identity
