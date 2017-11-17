open Cryptodbm_internals
open Kinds
open Helper

(* cd .. && ocamlbuild -use-ocamlfind test/test_data_kind.native *)

let iterations = 14000

let password1 = Cipher.mk_passwd ~iterations "This is my password"
let password2 = Cipher.mk_passwd ~iterations "_"
let password3 = Cipher.mk_passwd ~iterations " o\000%*+"

let data_kinds = 
  [ mk_data Uncrypted ;
    mk_data (Encrypted (password1, 0)) ;
    mk_data (Encrypted (password1, 1)) ;
    mk_data (Encrypted (password1, 10)) ;
    mk_data (Encrypted (password1, 100)) ;
    mk_data (Encrypted (password2, 0)) ;
    mk_data (Encrypted (password2, 2)) ;
    mk_data (Encrypted (password2, 15)) ;
    mk_data (Encrypted (password2, 500)) ;
    mk_data (Encrypted (password3, 0)) ;
    mk_data (Encrypted (password3, 1)) ;
    mk_data (Encrypted (password3, 2)) ;
    mk_data (Encrypted (password3, 1000)) ]

(*** Test data encryption ***)
let test_data_encryption () =
  
  let count = ref 1 in

  (* String sizes *)
  for size = 0 to 20000 do

    (* Number of tests for each size. *)
    for tests = 0 to 100000 do
      
      let source = Utils.random_string Utils.gen size in

      let verbose = !count mod 10000 = 0 in

      if verbose then 
	Printf.printf "Data test #%d, size = %d (currently testing \"%s\")  =>  %!" !count size (convert source) ;

      List.iter

	(* Here is the unary DATA test *)
	begin fun kind ->
	  let encrypted_data = encode_data source kind in
	  let decrypted_data = decode_data encrypted_data kind in
	  assert (source = decrypted_data) ;
	end

	data_kinds ;

      if verbose then
	Printf.printf "ok\n%!" ;

      incr count ;

    done;

  done ;
  ()

let () = test_data_encryption ()

