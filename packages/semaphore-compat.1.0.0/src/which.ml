let () =
  let version =
    Scanf.sscanf Sys.argv.(1) "%d.%d" (fun major minor -> (major, minor))
  in
  let file =
    if version < (4, 12) then "semaphore-as-newtype.ml"
    else "semaphore-as-alias.ml"
  in
  print_string file
