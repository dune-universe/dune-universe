let () =
  let id = [%madcast: 'a -> 'a] in
  assert (true = id true) ;
  assert (42 = id 42)
