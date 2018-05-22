let () =
  let res =
    [| "1"; "2"; "3"; "4"; "5"; "6" |]
    |> [%madcast: string array -> (int * int) list]
  in
  assert (res = [ (1,2); (3,4); (5,6) ])
