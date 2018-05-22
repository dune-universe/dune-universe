let () =
  let res =
    [1; 2]
    |> [%madcast: int list -> (int * int)]
  in
  assert (res = (1, 2))
