let () =
  let res =
    [| "1"; "2" |]
    |> [%madcast: string array -> (int * string)]
  in
  assert (res = (1, "2"))

let () =
  let res =
    [| "1"; "Pierre"; "7" |]
    |> [%madcast: string array -> (int * string * int)]
  in
  assert (res = (1, "Pierre", 7))
