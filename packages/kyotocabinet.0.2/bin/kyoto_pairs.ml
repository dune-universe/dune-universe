let select_prefix path prefix =
  let db = Kyoto.opendb path [Kyoto.OREADER; Kyoto.ONOLOCK] in
  let print_pair () (k,v) = Printf.printf "%s: %s\n%!" k v in
  let () = match prefix with
    | None -> Kyoto.fold db print_pair ()
    | Some prefix -> Kyoto.fold_prefix db prefix print_pair ()
  in
  Kyoto.close db


let main =
  if Array.length Sys.argv != 2 && Array.length Sys.argv != 3
  then (
    Printf.fprintf stderr "usage: %s kyoto-db-path [key-prefix]\n%!" Sys.argv.(0);
  )
  else
    let path = Sys.argv.(1) in
    let prefix = if Array.length Sys.argv == 3 then Some Sys.argv.(2) else None
    in select_prefix path prefix
