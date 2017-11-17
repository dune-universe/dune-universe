open Cryptodbm

let file = "/tmp/test-db"
and passwd = "12345"

let read_file () =

  let table = open_read ~file ~passwd ~signwd:"" () in

  let subt1 = open_subtable table ~name:"my subtable #1" ~passwd:"" ~signwd:"" () in
  iter subt1 (fun key data -> Printf.printf " Key = %s  Data = %s\n" key data) ;

  let subt2 = open_uncrypted_subtable table ~name:"a public subtable" ~signwd:"" () in
  iter subt2 (fun key data -> Printf.printf " Key = %s  Data = %s\n" key data) ;
    
  close table ;
  ()


let () = read_file ()

