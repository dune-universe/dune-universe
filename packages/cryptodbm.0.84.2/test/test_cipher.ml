open Cryptodbm_internals
open Cipher

let trytest () =
  Printf.printf "Digest ... %!" ;
  Printf.printf "%s\n%!" (digest "testum") ;

  let passwd = mk_weak_passwd "yo"
  in

  Printf.printf "Encrypt ... %!" ;
  let encryptfun = encrypt ~passwd in
  let _ = encryptfun "a" in
  Printf.printf "done\n%!" ;

  let encrypted = encryptfun "-my message-" in
  Printf.printf "done2\n%!" ;

  Printf.printf "Decrypt ... %!" ;
  let decrypted = decrypt ~passwd encrypted in
  Printf.printf "done : %s\n%!" decrypted ;

  ()


let () =
  trytest () ;
  ()
