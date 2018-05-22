let () =
  let res =
    Some 1
    |> [%madcast: int option -> int]
  in
  assert (res = 1)
