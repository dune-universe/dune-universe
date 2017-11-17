open Cryptodbm_internals
open Kinds
open Helper

(* cd .. && ocamlbuild -use-ocamlfind test/test_key_kind.native *)

(*** Test key encryption ***)

open Exenum
open Big_int_Z
open Exenum_internals.Convenience

let iterations = 15000

(* Enumerators *)

(* We only test a few subtable numbers. *)
let subtables = [0 ; 1 ; 2 ; 3 ; max_subtable - 2 ; max_subtable - 1 ; max_subtable]
let e_subt = from_list ~name:"Subtable number" subtables

let e_pad = from_list ~name:"Padding" [0 ; 1 ; 2 ; 3 ; 10 ; 200; 500]

let uncrypted_passwd = "This special password is meaningful only for subtables."

let e_passwds = from_list ~name:"Passwords" ["" ; "_" ; "z" ; "This is : some 1 password !!!" ; uncrypted_passwd ]

(* let () = Printf.printf "Test_key_kind : change small boi value in e_keys !!!\n%!" *)

let e_keys = union [ from_list ~name:"Some keys" ["A given quite long key\n with \000 some strange characters.\001" ] ;
		     sub ~max:(boi 200000) (e_rstring ['\000' ; '\n' ; '_' ; 'a' ; 'K']) ;
		   ]

let e_location = union [ single Table_Builtin ;
			 map e_subt (fun i -> Subtable_Builtin i) ;
			 map e_subt (fun i -> Subtable_User i) ]

let e_fullkey = pair e_keys e_location

(* 2^test_size values will be considered. *)
let (test_size, fullcardinal) =
  match cardinal e_fullkey with
  | None -> assert false
  | Some n -> (get_big_int_size n 1, n)

(* A table configuration *)
type config =
    { 
      (* Passwd, pad *)
      table_encryption : string * int ;

      (* Subtable number, passwd, pad 
       * If the passwd is the special "uncrypted" passwd, then the subtable is an uncrypted one.
       * (Which is different from a subtable with no specific password.) *)
      subtables_encryption : (int * string * int) list ;

      (* Function giving the password and padding for a given subtable, using the data in subtables_encryption. *)
      subt_info : (int -> Cipher.passwd * int) }

let print_config cf =
  let (tps, tpad) = cf.table_encryption in
  Printf.printf "  Table passwd='%s', pad=%d\n" (convert tps) tpad ;
  List.iter
    begin fun (subt, sps, spad) ->
      Printf.printf "  Subtable %d : " subt ;
      if sps == uncrypted_passwd then Printf.printf "uncrypted\n"
      else Printf.printf "passwd='%s', pad=%d\n" (convert sps) spad ;
    end
    cf.subtables_encryption ;
  ()
      
let mk_config (table_encryption, subts) =

  let subtables_encryption = List.map2 (fun (a,b) i -> (i, a, b) ) subts subtables in

  let subt_info n =
    try
      let (_, r, p) = List.find (fun (i, _, _) -> i = n) subtables_encryption in
      (Cipher.mk_passwd ~iterations r, p)
    with Not_found -> assert false (* The given subtable number was not found. Impossible! *)
  in

  { table_encryption ; 
    subtables_encryption ; 
    subt_info }

let e_passwd_pad = pair e_passwds e_pad

let e_config = 
  map (pair 
	 e_passwd_pad 
	 (product (List.map (fun _ -> union [e_passwd_pad ; single (uncrypted_passwd, 0)]) subtables)))
    mk_config

module Col = Collision

let get_how config location =

  let (table_passwd, table_pad) = config.table_encryption in
  let table_passwd = Cipher.mk_passwd ~iterations table_passwd in

  match location with
  | Table_Builtin ->
      if table_passwd == Cipher.empty_passwd then (Uncrypted, Cipher.empty_passwd, Cipher.empty_passwd, 0)
      else
	(Encrypted (table_passwd, Cipher.empty_passwd, table_pad), table_passwd, Cipher.empty_passwd, 0)

  | Subtable_Builtin n
  | Subtable_User n ->
      let (subpass, subpad) = config.subt_info n in
      if Cipher.plain_passwd subpass == uncrypted_passwd then (Uncrypted, table_passwd, Cipher.empty_passwd, n)
      else
	if subpass == Cipher.empty_passwd && table_passwd == Cipher.empty_passwd then (Uncrypted, table_passwd, Cipher.empty_passwd, n)
	else (Encrypted (table_passwd, subpass, subpad), table_passwd, subpass, n)


(* Given a table config, test key encryption.
 * We also test injectivity. *)
let test_key_encryption old_collisions count config =
  
  let coltable = Col.create ~card:test_size ~fail:10 in
  let collisions = ref [] in
 
  Printf.printf "Required hashsize : %d bits\n%!" (Col.hashsize coltable) ;

  (* old collisions *)
  let old = Hashtbl.create (List.length old_collisions) in
  List.iter (fun (index, encrypted_key) -> Hashtbl.add old encrypted_key index) old_collisions ;
 
  (* Iterate over fullkeys *)
  let index = ref bigzero
  and last = pred_big_int fullcardinal
  in
  
  while !index <= last do

      let (keysource, location) = get e_fullkey !index in
      let (how, table_passwd, subt_passwd, subt_nb) = get_how config location in
      let key_kind = mk_key location how in

      let verbose = !count mod 10000 = 0 in
      if verbose then 
	Printf.printf "Test #%d (currently testing \"%s\") with kind %s =>  %!" 
	  !count (convert keysource) (kind2s key_kind) ;
      
      (* Encrypt key *)
      let encrypted_key = encode_key keysource key_kind in

      if old_collisions <> [] then
	begin
	  if Hashtbl.mem old encrypted_key then
	    begin
	      let index2 = Hashtbl.find old encrypted_key in
	      if 0 <> big_compare !index index2 then
		begin
		  Printf.printf "\n*************\nDeja vu : %s = %s\n\n%!" (string_of_big_int !index) (string_of_big_int index2) ;
		  
		  let (keysource2, location2) = get e_fullkey index2 in
		  let (how2, _, _, _) = get_how config location2 in
		  let key_kind2 = mk_key location2 how2 in
		  Printf.printf "Kind-1 = %s   source-1 = '%s'  encoded = '%s'\n" (kind2s key_kind) (convert keysource) (convert (encodedkey2s encrypted_key)) ;
		  Printf.printf "Kind-2 = %s   source-2 = '%s'  encoded = '%s'\n%!" (kind2s key_kind2) (convert keysource2)
		    (convert (encodedkey2s (encode_key keysource2 key_kind2))) ;

		  assert false ;
		end
	    end ;
	end ;

      (* Injectivity test *)
      let hashcode = hash256 (encodedkey2s encrypted_key) in
      if not (Col.insert ~hashcode coltable) then
	collisions := (!index, encrypted_key) :: !collisions ;
      
      if verbose then
	Printf.printf "(key = '%s')  %!" (convert keysource) ;
      
      (*** Decrypt key, and test equality ***)

      let subt_fun n = fst (config.subt_info n) in

      (* Decrypt using all the available information. *)
      begin match get_key_info table_passwd ~subt_pas:subt_fun encrypted_key with
      | None -> assert false (* We gave all the information. We are able to decrypt, damn! *)
      | Some (kkind, None) -> assert false (* Ditto. *)
      | Some (kkind, Some kkey) ->
	  assert (kkey = keysource) ;
	  assert (kkind.key_loc = key_kind.key_loc) ;
	  ()
      end ;
      
      (* Decrypt using only the table passwd. *)
      begin match get_key_info table_passwd ~subt_pas:(fun _ -> Cipher.empty_passwd) encrypted_key with
      | None -> assert false (* We gave the table passwd. *)
      | Some (kkind, None) ->
	  assert (subt_passwd != Cipher.empty_passwd && Cipher.plain_passwd subt_passwd != uncrypted_passwd) ;
	  assert (kkind.key_loc = key_kind.key_loc) ;
	  ()
	    
      | Some (kkind, Some kkey) ->
	  assert (subt_passwd == Cipher.empty_passwd || Cipher.plain_passwd subt_passwd == uncrypted_passwd || key_kind.key_loc = Table_Builtin) ;
	  assert (kkey = keysource) ;
	  assert (kkind.key_loc = key_kind.key_loc) ;
	  ()
      end ;
	
      (* Decrypt using only the subtable passwd. *)
      begin match get_key_info Cipher.empty_passwd ~subt_pas:subt_fun encrypted_key with
      | None -> assert (table_passwd <> Cipher.empty_passwd && Cipher.plain_passwd subt_passwd != uncrypted_passwd)
      | Some (kkind, None) -> assert false (* We gave the subtable password. *)
      | Some (kkind, Some kkey) ->
	  assert (table_passwd == Cipher.empty_passwd || key_kind.key_how = Uncrypted) ;
	  assert (kkey = keysource) ;
	  assert (kkind.key_loc = key_kind.key_loc) ;
	  ()
      end ;

      (* Decrypt using none. *)
      begin match get_key_info Cipher.empty_passwd ~subt_pas:(fun _ -> Cipher.empty_passwd) encrypted_key with
      | None -> assert (table_passwd <> Cipher.empty_passwd)
      | Some (kkind, None) ->
	  assert (table_passwd == Cipher.empty_passwd) ;
	  assert (key_kind.key_how <> Uncrypted) ;
	  assert (kkind.key_loc = key_kind.key_loc) ;
	  ()

      | Some (kkind, Some kkey) ->
	  assert (table_passwd == Cipher.empty_passwd || key_kind.key_how = Uncrypted) ;
	  assert (kkey = keysource) ;
	  assert (kkind.key_loc = key_kind.key_loc) ;
	  ()
      end ;
      
      if verbose then
	Printf.printf "ok\n%!" ;
      
      incr count ;
      index := !index +++ 1 ;

  done ;
  
  let collen = List.length !collisions in
  Printf.printf "Tests done : %d keys tested. %d collisions detected.\n%!" !count collen ;
  Printf.printf "Collision factor : %.4f\n%!" (float_of_int collen /. float_of_int !count) ;

  !collisions

  
let () = 
  let count = ref 0 in

  let start = bos "0" in

  for num = 0 to 10000 do
    Printf.printf "\n****************************************\n" ;
    Printf.printf "**       Config #%d\n" num ;
    Printf.printf "****************************************\n%!" ;

    let index = start +++ num in
    let config = get e_config index in

    print_config config ;

    let collisions = test_key_encryption [] count config in
    if collisions = [] then () (* OK *)
    else 
      (* Try again. *)
      ignore(test_key_encryption collisions count config)

  done ;

  Printf.printf "All %d tests done.\n%!" !count ;

  ()
