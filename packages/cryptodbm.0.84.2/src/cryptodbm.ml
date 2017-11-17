open Cryptodbm_internals
open Config
open Types

type read = Types.read
type full = Types.full

(* The standard dictionary is saved in subtable #0, using the default table encryption.
 * The uncrypted dictionary (for explicitly uncrypted subtables) is saved in subtable #1.
 * Extra phony bindings are stored in subtable #2. *)

let default_max_subtable = 2

(* A subtable identifier : its type (Uncrypted or Standard), and its name. *)
type sub_ident = Uncr of string | Stand of string

type 'a table = {
    (* Format version *)
    format_version: string ;

    (* Trick to mimic a gadt. 
     * I have not been able to use an ocaml gadt, though, because I would then need some form of subtyping 
     * in order to cast a full handler to a read handler. *)
    mutable status: full table status ;

    (* Lower-level dbm handler. *)
    handler: 'a Operations.handler ;

    (* Table salt. *)
    salt: string ;

    (* Subtables. *)
    read_subtables: (sub_ident, read subtable) Hashtbl.t ;
    full_subtables: (sub_ident, full subtable) Hashtbl.t ;

    (* Dictionary. *)
    diction: 'a Subtable.sub ;

    (* Uncrypted dictionary. *)
    uncrypted_diction: 'a Subtable.sub ;

    (* Extra bindings. *)
    extra_bindings: 'a Subtable.sub ;

    (* Next available subtable number (when creating new subtables). *)
    mutable next_subt: int ;

    (* Maximal extra padding for keys *)
    max_extra_key: int ;
    (* Maximal extra padding for data *)
    max_extra_data: int ;

    (* Number of extra bindings that should be added when closing the file, in percent. *)
    max_extra_bindings: int ;

    (* Global password. Empty means no encryption. *)
    table_passwd: Cipher.passwd ;

    (* Passphrase used to sign the base. Empty means no signature. *)
    signwd: Cipher.passwd ;

  }

(* Subtable with mode 'a (full or read).
 * The table mode is hidden in the type foralltable, using an (encoded) existential type. *)
and 'a subtable =
    { id  : sub_ident ;
      sub : 'a Subtable.sub ;
      own : 'c. 'c foralltable -> 'c }

and 'c foralltable = { foralltable: 'b. 'b table -> 'c }

(* (existential) Getters *)
let get_read_subtables subtable = subtable.own { foralltable = fun tbl -> tbl.read_subtables }
let get_full_subtables subtable = subtable.own { foralltable = fun tbl -> tbl.full_subtables }

let get_number subtable = Subtable.get_number subtable.sub
let get_name   subtable = Subtable.get_name subtable.sub

let get_rootfile table = Operations.get_rootfile table.handler

let error err = raiserror (err Table)

(* Kind corresponding to uncrypted bindings. *)
let table_uncrypted_kind = Kinds.(mk_key Table_Builtin Uncrypted)

(* Debug: believe it or not, this function segfaults in some cases. *)
(*
let autotest table =
  match table.status with
  | Read | Closed -> assert false
  | Full tab ->
      assert (tab == table) ;
      Printf.printf "Autotest : %!" ;
      let n = tab.max_extra_bindings in
      Printf.printf "OK (%d) !\n%!" n ;
      ()
*)

(* Check that the table is not closed. *)
let check_not_closed_aux tbl readmode = 
  match tbl.status with
  | Closed -> error is_closed
  | Full _ -> ()
  | Read -> assert readmode

let check_not_closed readmode sub = sub.own { foralltable = (fun t -> check_not_closed_aux t readmode) }

(* Like operations.get, but with a different error if the binding is not found. *)
let builtin_get ~bad_passwd handler kind ~key =
  try Operations.get handler kind ~key
  with 
  | Error (Unbound (_, Any) | Bad_password Any) ->
      if bad_passwd then error bad_password
      else raiserror (Corrupted (Table, "Missing builtin binding for key: " ^ key))

(* Find maximal subtable number in a dictionary *)
let get_max_subt dic = 
  Subtable.fold dic default_max_subtable begin fun k num acu ->
    let nb = 
      try int_of_string num 
      with _ -> raiserror (Corrupted (Table, Printf.sprintf "Corrupted dictionary for subtable %s: found %s instead of a number." k num))
    in
    max acu nb
  end
	  
(* open_dict for the uncrypted dictionary.
 * open_dict2 for the encrypted dictionary and extra bindings. *)
let open_aux handler open_dict open_dict2 ~iterations passwd ~signwd ~check_signature =

  if check_signature && signwd = "" then 
    failwith "Table.open_append: check_signature is true, but no signword is given. I need one in order to check the file signature." ;

  (* try: Closes the underlying handler if something goes wrong. *)
  try
    (* Get format and salt. *)
    let format_version = builtin_get ~bad_passwd:false handler table_uncrypted_kind ~key:Config.format_version_key
    and salt           = builtin_get ~bad_passwd:false handler table_uncrypted_kind ~key:Config.salt_key in
    
    (* Currently, it must be exactly the same format. *)
    if format_version <> Config.current_format_version then raiserror (Bad_format (Config.current_format_version, format_version)) ;

    (* Signword *)
    let signwd =
      if signwd = "" then Cipher.empty_passwd
      else Cipher.mk_passwd ~iterations (Config.add_salt salt signwd)
    in

    (* Check signature *)
    if check_signature && signwd != Cipher.empty_passwd then
      begin
	let expected_signature = Signature.table_signature handler ~table_salt:salt ~signwd
	and found_signature = 
	  try Signature.read_table_signature handler
	  with Error (Unbound (_, Any)) -> raiserror (No_signature Table)
	in
	
	if Signature.equal expected_signature found_signature then ()
	else error bad_signature ;
      end ;
    
    let uncrypted_diction = open_dict handler ~name:Config.uncrypted_dictionary ~subt:1 ~how:Kinds.Uncrypted in

    match passwd with
    | Some passwd ->
	begin
	  assert (open_dict == open_dict2) ;

	  (* Salts the password. *)
	  let table_passwd = 
	    if passwd = "" then Cipher.empty_passwd
	    else Cipher.mk_passwd ~iterations (Config.add_salt salt passwd)
	  in
	  
	  (* Kind used for table parameters: use password and salt, but no padding. *)
	  let table_encrypted_nopad_kind =
	    if table_passwd == Cipher.empty_passwd then table_uncrypted_kind
	    else Kinds.(mk_key Table_Builtin (Encrypted (table_passwd, Cipher.empty_passwd, 0)))
	  in
	  
	  (* Default extra padding for data and keys. *)
	  let max_extra_key  = int_of_string (builtin_get ~bad_passwd:true handler table_encrypted_nopad_kind ~key:Config.max_extra_key_key)
	  and max_extra_data = int_of_string (builtin_get ~bad_passwd:true handler table_encrypted_nopad_kind ~key:Config.max_extra_data_key) 
	  and max_extra_bindings = int_of_string (builtin_get ~bad_passwd:true handler table_encrypted_nopad_kind ~key:Config.max_extra_bindings_key) 
	  in
	  
	  (* Standard table kind. *)
	  let table_encrypted_kind =
	    if table_passwd == Cipher.empty_passwd then table_uncrypted_kind
	    else Kinds.(mk_key Table_Builtin (Encrypted (table_passwd, Cipher.empty_passwd, max_extra_key)))
	  in
	  
	  (* Dictionary *)
	  let diction_how =
	    let open Kinds in
	    match table_encrypted_kind.key_how with
	    | Uncrypted -> Uncrypted
	    | Encrypted (tp, _, _) -> Encrypted (tp, "")
	  in
	  let diction = open_dict handler ~name:Config.dictionary ~subt:0 ~how:diction_how
	  and extra_bindings = open_dict handler ~name:Config.extra_bindings ~subt:2 ~how:diction_how in
	  
	  let max_subt = max (get_max_subt uncrypted_diction) (get_max_subt diction) in
	  
	  (* Check the table test key *)
	  let data = builtin_get ~bad_passwd:true handler table_encrypted_kind ~key:Config.test_key in
	  if data <> Config.test_data then
	    raiserror (Corrupted (Table, Printf.sprintf "Corrupted test binding: found %s instead of %s." data Config.test_data)) ;
	  
	  { format_version ;
	    status = Read ; (* Temporary status in append mode. *)
	    handler ;
	    salt ;
	    next_subt = max_subt + 1 ;
	    diction ;
	    uncrypted_diction ;
	    extra_bindings ;
	    read_subtables = Hashtbl.create 10 ;
	    full_subtables = Hashtbl.create 10 ;
	    max_extra_key ;
	    max_extra_data ;
	    max_extra_bindings ;
	    table_passwd ;
	    signwd }
	end

    | None ->
	(* No password, we open the table in uncrypted mode only. *)
	  { format_version ;
	    status = Read ;
	    handler ;
	    salt ;
	    next_subt = 0 ; (* Unused in read mode. *)
           (* Both will be the empty subtable anyway. We need this indirection because of type unification between the two cases.*)
	    diction = open_dict2 handler ~name:Config.dictionary ~subt:0 ~how:Kinds.Uncrypted ; 
	    extra_bindings = open_dict2 handler ~name:Config.extra_bindings ~subt:2 ~how:Kinds.Uncrypted ; 
	    uncrypted_diction ;
	    read_subtables = Hashtbl.create 10 ;
	    full_subtables = Hashtbl.create 10 ;
	    max_extra_key = 0 ;
	    max_extra_data = 0 ;
	    max_extra_bindings = 0 ;
	    table_passwd = Cipher.empty_passwd ;
	    signwd }	
      
  with e ->
    Operations.close handler ;
    raise e  

let open_dict_empty _ ~name ~subt ~how = Subtable.empty name subt

let open_read ?(iterations=Config.passwd_iterations) ~file ~passwd ~signwd () =
  let handler = Operations.open_read ~file in
  let open_dict handler ~name ~subt ~how = Subtable.open_read handler ~name ~subt ~iterations ~how ~signwd:"" in
  open_aux handler open_dict open_dict ~iterations (Some passwd) ~signwd ~check_signature:(signwd <> "")

let open_only_uncrypted ?(iterations=Config.passwd_iterations) ~file ~signwd () =
  let handler = Operations.open_read ~file in  
  let open_dict handler ~name ~subt ~how = Subtable.open_read handler ~name ~subt ~iterations:0 ~how ~signwd:"" in
  open_aux handler open_dict ~iterations open_dict_empty None ~signwd ~check_signature:(signwd <> "")

let open_append ?(iterations=Config.passwd_iterations) ~file ~passwd ~signwd ~check_signature () =
  let handler = Operations.open_append ~file in
  let open_dict handler ~name ~subt ~how = Subtable.open_append handler ~name ~subt ~iterations ~how ~signwd:"" ~check_signature:false in
  let table = open_aux handler open_dict open_dict (Some passwd) ~iterations ~signwd ~check_signature in
  table.status <- Full table ;

  (* If the table was signed, we remove the signature. 
   * Otherwise, one could append data and keep the original signature. *)
  Signature.remove_table_signature table.handler ;

  table

let open_create ~file ?overwrite ?(iterations=Config.passwd_iterations) ~passwd ~signwd ?(max_extra_key=0)
    ?(max_extra_data=0) ?(max_extra_bindings=0) ~perm () =

  let handler = Operations.open_full ?overwrite ~file ~perm in

  (* try: Closes the underlying handler if something goes wrong. *)
  try

    (* Create salt *)
    let salt = Utils.random_string Utils.gen Config.salt_size in

    (* Write format and salt. *)
    Operations.add handler table_uncrypted_kind ~max_extra_data:0 ~key:Config.format_version_key ~data:Config.current_format_version ;
    Operations.add handler table_uncrypted_kind ~max_extra_data:0 ~key:Config.salt_key ~data:salt ;

    (* Salts the password. *)
    let table_passwd = 
      if passwd = "" then Cipher.empty_passwd
      else Cipher.mk_passwd ~iterations (Config.add_salt salt passwd)
    in

    (* Kind used for table parameters: use password and salt, but no padding. *)
    let table_encrypted_nopad_kind =
      if table_passwd == Cipher.empty_passwd then table_uncrypted_kind
      else Kinds.(mk_key Table_Builtin (Encrypted (table_passwd, Cipher.empty_passwd, 0)))
    in

    (* Write extra padding. *)
    Operations.add handler table_encrypted_nopad_kind ~max_extra_data:0 ~key:Config.max_extra_key_key ~data:(string_of_int max_extra_key) ;
    Operations.add handler table_encrypted_nopad_kind ~max_extra_data:0 ~key:Config.max_extra_data_key ~data:(string_of_int max_extra_data) ;
    Operations.add handler table_encrypted_nopad_kind ~max_extra_data:0 ~key:Config.max_extra_bindings_key ~data:(string_of_int max_extra_bindings) ;

    (* Standard table kind. *)
    let table_encrypted_kind =
      if table_passwd == Cipher.empty_passwd then table_uncrypted_kind
      else Kinds.(mk_key Table_Builtin (Encrypted (table_passwd, Cipher.empty_passwd, max_extra_key)))
    in

    (* Write table test key *)
    Operations.add handler table_encrypted_kind ~max_extra_data:0 ~key:Config.test_key ~data:Config.test_data ;

    (* Dictionary *)
    let diction_how =
      let open Kinds in
      match table_encrypted_kind.key_how with
      | Uncrypted -> Uncrypted
      | Encrypted (tp, _, _) -> Encrypted (tp, "")
    in
    let openf = Subtable.open_full handler ~max_extra_key:0 ~max_extra_data:0 ~iterations ~signwd:"" in
    let diction = openf ~name:Config.dictionary ~subt:0 ~how:diction_how
    and uncrypted_diction = openf ~name:Config.uncrypted_dictionary ~subt:1 ~how:Kinds.Uncrypted
    and extra_bindings = openf ~name:Config.extra_bindings ~subt:2 ~how:diction_how in

    (* Signword *)
    let signwd =
      if signwd = "" then Cipher.empty_passwd
      else Cipher.mk_passwd ~iterations (Config.add_salt salt signwd)
    in
 
    let rec table =
      { format_version = Config.current_format_version ;
	status = Full table ;
	handler ;
	salt ;
	read_subtables = Hashtbl.create 10 ;
	full_subtables = Hashtbl.create 10 ;
	next_subt = default_max_subtable + 1 ;
	diction ;
	uncrypted_diction ;
	extra_bindings ;
	max_extra_key ;
	max_extra_data ;
	max_extra_bindings ;
	table_passwd ;
	signwd }
    in
    table

  with e ->
    Operations.close handler ;
    raise e

(* Close subtable and close_all subtables. *)
let close_subtable subtable =
  check_not_closed true subtable ;
  Subtable.close subtable.sub ;

  (* Remove the subtable from the hashtable. *)
  Hashtbl.remove (get_read_subtables subtable) subtable.id ;
  Hashtbl.remove (get_full_subtables subtable) subtable.id ;
  ()

let close_all_subtables htbl = 
  (* We don't use close_subtable to avoid removing an element from a hashtable while we are iterating over it. *)
  Hashtbl.iter (fun _ subtable -> Subtable.close subtable.sub) htbl ;
  Hashtbl.clear htbl ;
  ()

(* Sign if necessary. *)
let sign table =
  match table.status with
  | Full table ->
      if table.signwd != Cipher.empty_passwd then
	Signature.sign_table table.handler ~table_salt:table.salt ~signwd:table.signwd 
  | Read -> ()
  | Closed -> error is_closed      

(* Add extra bindings if necessary. *)
let add_phony_bindings table =
  match table.status with
  | Read -> ()
  | Closed -> assert false
  | Full tab ->
      Subtable.clear tab.extra_bindings ;
      if tab.max_extra_bindings = 0 then ()
      else 
	begin
	  let stats = Stats.new_stats () in
	  Operations.iter_all tab.handler (Stats.put stats) ;
	  (* Generate bindings. *)
	  Stats.insert stats tab.max_extra_bindings (Subtable.add ~may_overwrite:true tab.extra_bindings) ;
	end

let close table =

  if table.status = Closed then error is_closed ;

  (* Close all subtables. *)
  close_all_subtables table.read_subtables ;
  close_all_subtables table.full_subtables ;
  Subtable.close table.diction ;
  Subtable.close table.uncrypted_diction ;

  (* Add phony bindings before the signature. *)
  add_phony_bindings table ;
  Subtable.close table.extra_bindings ;

  sign table ;
  table.status <- Closed ;

  (* Close the database handler. *)
  Operations.close table.handler ;
  ()

let flush ?backup ?backup_name table =
  if table.status = Closed then error is_closed ;

  (* Sign all subtables. *)
  Hashtbl.iter (fun _ subtable -> Subtable.sign subtable.sub) table.full_subtables ;

  (* Sign the table. *)
  sign table ;
  
  (* Flush *)
  Operations.flush ?backup ?backup_name table.handler ;
  ()

let add ?may_overwrite subtable ~key ~data =
  check_not_closed false subtable ;
  Subtable.add ?may_overwrite subtable.sub ~key ~data

let find subtable key =
  check_not_closed true subtable ;
  Subtable.find subtable.sub key

let delete subtable key =
  check_not_closed false subtable ;
  Subtable.delete subtable.sub key

let clear subtable =
  check_not_closed false subtable ;
  Subtable.clear subtable.sub

let iter subtable f =
  check_not_closed true subtable ;
  Subtable.iter subtable.sub f

let iterkey subtable f =
  check_not_closed true subtable ;
  Subtable.iterkey subtable.sub f

let fold subtable acu f =
  check_not_closed true subtable ;
  Subtable.fold subtable.sub acu f


(*** Opens a subtable ***)
let check_opened_subtable table id =
  let check htbl =
    if Hashtbl.mem htbl id then
      let subtable = Hashtbl.find htbl id in
      let subt = get_number subtable in
      raiserror (Is_Already_Open (Subtable (get_name subtable, subt)))
  in
  check table.read_subtables ; 
  check table.full_subtables ;
  ()

let open_subtable_aux table ~name ~iterations ~signwd open_sub htbl pass =
  
  if table.status = Closed then error is_closed ;

  let id = if pass = None then Uncr name else Stand name in  

  (* Check that it is not already open. *)
  check_opened_subtable table id ;

  (* Find the subtable number. *)
  let number =
    try
      let dic = if pass = None then table.uncrypted_diction else table.diction in
      let sval = Subtable.find dic name in
      try int_of_string sval 
      with Failure _ -> raiserror (Corrupted (Table, Printf.sprintf "Subtable %s is not bound to a number as expected. Found: %s" name sval))
    with 
    | Error (Unbound (_, _)) -> raiserror (No_subtable name)
  in

  let how =
    match pass with
    | None -> Kinds.Uncrypted
    | Some p ->
	let uncrypted = (table.table_passwd == Cipher.empty_passwd) && p = "" in
	if uncrypted then Kinds.Uncrypted
	else Kinds.Encrypted (table.table_passwd, p)
  in
  
  let sub = open_sub table.handler ~name ~subt:number ~iterations ~how ~signwd in

  let subtable =
    { id ;
      sub ;
      own = (fun f -> f.foralltable table) }
  in

  (* Register the subtable in the hashtable. *)
  Hashtbl.add htbl id subtable ;

  subtable

let open_subtable table ~name ?(iterations=Config.passwd_iterations) ~passwd ~signwd () =
  open_subtable_aux table ~name ~iterations ~signwd Subtable.open_read table.read_subtables (Some passwd)

(* 'Iterations' is used to compute the signwd. *)
let open_uncrypted_subtable ?(iterations=Config.passwd_iterations) table ~name ~signwd () =
  open_subtable_aux table ~name ~iterations ~signwd Subtable.open_read table.read_subtables None

let append_subtable table ~name ?(iterations=Config.passwd_iterations) ~passwd ~signwd ~check_signature () = 
  let subtable = open_subtable_aux table ~name ~iterations ~signwd (Subtable.open_append ~check_signature) table.full_subtables (Some passwd) in
  (* If the table was signed, we remove the signature. *)
  Subtable.remove_signature subtable.sub ;
  subtable 

let append_uncrypted_subtable ?(iterations=Config.passwd_iterations) table ~name ~signwd ~check_signature () =
  let subtable = open_subtable_aux table ~name ~iterations ~signwd (Subtable.open_append ~check_signature) table.full_subtables None in
  Subtable.remove_signature subtable.sub ;
  subtable


(*** Creates a subtable ***)

let create_subtable_aux table ~name ~iterations ~signwd ?(max_extra_key=table.max_extra_key) ?(max_extra_data=table.max_extra_data) pass =

  if table.status = Closed then error is_closed ;
  
  (* Which dictionary *)
  let dic = if pass = None then table.uncrypted_diction else table.diction in

  (* Check that it does not already exist. *)
  if Subtable.is_bound dic name then
    raiserror (Subtable_exists name) ;
  
  (* Subtable number. *)
  let number = table.next_subt in
  table.next_subt <- table.next_subt + 1 ;

  let how =
    match pass with
    | None -> Kinds.Uncrypted
    | Some p ->
	let uncrypted = (table.table_passwd == Cipher.empty_passwd) && p = "" in
	if uncrypted then Kinds.Uncrypted
	else Kinds.Encrypted (table.table_passwd, p)
  in

  let sub = Subtable.open_full table.handler ~name ~subt:number ~iterations ~how ~signwd ~max_extra_key ~max_extra_data in

  let id = if pass = None then Uncr name else Stand name in

  let subtable =
    { id ;
      sub ;
      own = (fun f -> f.foralltable table) }
  in

  (* Register the subtable in the hashtable and in the dictionary. *)
  Hashtbl.add table.full_subtables id subtable ;
  Subtable.add dic ~key:name ~data:(string_of_int number) ;

  subtable

  

let create_subtable table ~name ?(iterations=Config.passwd_iterations) ~passwd ~signwd ?max_extra_key ?max_extra_data () =
  create_subtable_aux table ~name ~iterations ~signwd ?max_extra_key ?max_extra_data (Some passwd)

let create_uncrypted_subtable ?(iterations=Config.passwd_iterations) table ~name ~signwd () =
  create_subtable_aux table ~name ~iterations ~signwd ~max_extra_key:0 ~max_extra_data:0 None

let iter_subtables table f =
  if table.status = Closed then error is_closed ;
  Subtable.iter table.diction (fun key data -> f key (int_of_string data))

let iter_uncrypted_subtables table f =
  if table.status = Closed then error is_closed ;
  Subtable.iter table.uncrypted_diction (fun key data -> f key (int_of_string data))

let iter_extra_bindings table f =
  if table.status = Closed then error is_closed ;
  Subtable.iter table.extra_bindings f


module Error = Errors

(* We purposedly assume that the binary encoding of strings is very very very unlikely to change over the years... *)
let write_string outch (s:string) = Marshal.(to_channel outch s [Compat_32])

let read_string inch = (Marshal.from_channel inch : string)

let export table ~binfile =

  let outch = open_out_bin binfile in

  Operations.iter_all table.handler
    begin fun enc_key enc_data ->
      write_string outch (Kinds.encodedkey2s enc_key) ;
      write_string outch (Kinds.encodeddata2s enc_data) ;
    end ;
  
  close_out outch ;
  ()

module DB = Kinds.LowerDB

let import ~binfile ~dbfile =
  let inch = open_in_bin binfile in

  let dbm = DB.open_dbm (DB.mk_file dbfile) `Write ~perm:0o600 in

  begin try 
    while true do
      let enc_key = Kinds.s2encodedkey (read_string inch) in
      let enc_data = Kinds.s2encodeddata (read_string inch) in
      DB.add dbm enc_key enc_data ;
    done ;
  with End_of_file -> ()
  end ;

  DB.close dbm ;
  close_in inch ;
  ()
