open Cryptodbm

let file = "/tmp/test-db"
and passwd = "12345"

let write_file () =

  (* The table is encrypted (since passwd is not empty) *)
  let table = open_create ~file ~overwrite:true ~passwd ~signwd:""
      ~max_extra_key:0 ~max_extra_data:0 ~max_extra_bindings:40 ~perm:0o600 ()
  in

  (* subt1 is encrypted (since the table is encrypted). *)
  let subt1 = create_subtable table ~name:"my subtable #1" ~passwd:"" ~signwd:"" ()

  (* subt2 is not encrypted *)
  and subt2 = create_uncrypted_subtable table ~name:"a public subtable" ~signwd:"" () in
      
  add subt1 ~key:"secret-info1" ~data:"secret-data1" ;
  add subt1 ~key:"secret-info2" ~data:"secret-data2" ;
  
  add subt2 ~key:"public-info1" ~data:"public-data1" ;

  close table ;

  Printf.printf "File %s written\n%!" file ;
    
  ()


let () = write_file ()

(* You can browse the generated db file using test/dbmreader.exe :

make tests

ledit _build/default/test/dbm_reader.exe

> dump "/tmp/test-db"
> ...

*)
    
