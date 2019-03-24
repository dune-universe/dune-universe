let () =
  assert (Array.length Sys.argv = 2);
  let file = Sys.argv.(1) in
  Mybuild.Version.save ~identify:false file
