let () =
  let res =
    [|1; 2|]
    |> [%madcast: int array -> string array]
  in
  assert (res = [|"1"; "2"|])
