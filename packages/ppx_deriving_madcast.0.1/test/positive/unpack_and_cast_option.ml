let () =
  let res =
    Some "2"
    |> [%madcast: string option -> int]
  in
  assert (2 = res)
