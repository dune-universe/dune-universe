let () =
  let f = [%madcast: string -> int] in
  let g = [%madcast: (string -> int) -> (int -> string)] in
  assert ((g f) 2 = "2")
