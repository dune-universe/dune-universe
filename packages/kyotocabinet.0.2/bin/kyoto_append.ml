let append_pair txn k v =
  let k = String.trim k in
  let v = String.trim v in
  Kyoto.set txn k v
  
let rec loop txn =
  if Scanf.Scanning.end_of_input Scanf.Scanning.stdin
  then ()
  else (
    Scanf.scanf "%s@: %s@\n" (append_pair txn);
    loop txn
  )
   
let append_pairs path =
  Kyoto.with_db path [Kyoto.OWRITER; Kyoto.OCREATE]
  (fun db -> Kyoto.with_transaction db loop)

let main =
  if Array.length Sys.argv != 2
  then (
    Printf.fprintf stderr "usage: %s kyoto-db-path < key-value-pairs.log\n%!" Sys.argv.(0);
  )
  else
    let path = Sys.argv.(1) in
    append_pairs path
