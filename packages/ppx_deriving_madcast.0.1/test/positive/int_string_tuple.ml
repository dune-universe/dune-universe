let () =
  let (a, b) = [%madcast: (string * int) -> (int * string)] ("1", 2) in
  assert ((1, "2") = (a, b))

let () =
  let value = (1, "2") in
  let value' =
    value
    |> [%madcast: (int * string) -> (string * int)]
    |> [%madcast: (string * int) -> (int * string)]
  in
  assert (value = value')
