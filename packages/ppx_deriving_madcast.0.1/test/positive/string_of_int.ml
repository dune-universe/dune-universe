let () =
  let two = [%madcast: int -> string] 2 in
  assert ("2" = two)
