let () =
  let one = [%madcast: string -> int] "1" in
  assert (1 = one)
