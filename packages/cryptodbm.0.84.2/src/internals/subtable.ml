open Types
open Kinds

type 'a sub = {
    (* Status *)
    mutable status : full Operations.handler status ;

    (* Handler. None for empty (phony) subtables. *)
    handler : 'a Operations.handler option ;

    (* Subtable name *)
    name: string ;

    (* Subtable number *)
    subt: int ;

    (* Subtable salt *)
    sub_salt: string ;

    (* Key kind for user keys. *)
    user_key_kind: key_kind ;

    (* Key kind for builtin keys. *)
    builtin_key_kind: key_kind ;

    (* Maximal padding for user data. *)
    max_extra_data: int ;

    (* Passphrase used to sign the subtable. Empty means no subtable signature. *)
    subsignwd: Cipher.passwd ;
  }

(* Fails *)
let error err sub = raiserror (err (Subtable (sub.name, sub.subt)))

let get_number sub = sub.subt
let get_name   sub = sub.name

let phony_kind = Kinds.(mk_key Table_Builtin Uncrypted)

(* Mimics an empty subtable. *)
let empty name subt = {
  status = Read ;
  handler = None ;
  name ;
  subt ;
  sub_salt = "" ;
  user_key_kind = phony_kind ;
  builtin_key_kind = phony_kind ;
  max_extra_data = 0 ;
  subsignwd = Cipher.empty_passwd ;
}

(* Extract the table password. *)
let get_table_passwds sub =
  assert (sub.handler <> None) ;
  match sub.builtin_key_kind.key_how with
  | Uncrypted -> (Cipher.empty_passwd, Cipher.empty_passwd)
  | Encrypted (pw, sw, _) -> (pw, sw)

(* Salts the given key kind. 
 * sub_pw is just a string, we transform it into a (strong) password. *)
let get_salted_key_how ~iterations salt max_extra_key = function
  | Uncrypted -> Uncrypted
  | Encrypted (tab_pw, "") -> Encrypted (tab_pw, Cipher.empty_passwd, max_extra_key)
  | Encrypted (tab_pw, sub_pw) -> Encrypted (tab_pw, Cipher.mk_passwd ~iterations (Config.add_salt salt sub_pw), max_extra_key)

let get_semi_uncrypted = function
  | Uncrypted -> Uncrypted
  | Encrypted (tab_pw, _) ->
      if tab_pw == Cipher.empty_passwd then Uncrypted
      else Encrypted (tab_pw, Cipher.empty_passwd, 0)

(* Sign if necessary. *)
let sign sub =
  match sub.status with
  | Full handler ->
      if sub.subsignwd != Cipher.empty_passwd then
	begin
	  let (passwd, _) = get_table_passwds sub in
	  Signature.sign_subtable handler ~subtable_salt:sub.sub_salt ~passwd sub.builtin_key_kind 
	    ~subt:sub.subt ~signwd:sub.subsignwd
	end
  | Read -> ()
  | Closed -> error is_closed sub

let close sub =
  sign sub ; (* Check if it closed. *)
  sub.status <- Closed ;

  (* The handler controls the full database file. We don't close it here. *)
  ()

(* Like operations.get, but with a different error if the binding is not found. *)
let builtin_get loc ~bad_passwd handler kind ~key =
  try Operations.get handler kind ~key
  with 
  | Error (Unbound (_, Any) | Bad_password Any) ->
      if bad_passwd then raiserror (Bad_password loc)
      else raiserror (Corrupted (loc, "Missing builtin binding for key: " ^ key))

let compute_and_check_signature subtable =
  if subtable.subsignwd == Cipher.empty_passwd then ()
  else
    match subtable.handler with
    | None -> assert false (* The signwd was empty. Cannot get here. *)
    | Some handler ->
	(* Compute signature *)
	let (table_passwd, _) = get_table_passwds subtable in

	let signature = Signature.subtable_signature handler ~subtable_salt:subtable.sub_salt 
	    ~passwd:table_passwd ~subt:subtable.subt ~signwd:subtable.subsignwd
	in
      
	let read_signature = 
	  try Signature.read_subtable_signature handler subtable.builtin_key_kind ~subt:subtable.subt
	  with Error (Unbound (_, Any)) -> error no_signature subtable
	in

	if Signature.equal signature read_signature then ()
	else error bad_signature subtable

let open_aux handler status ~name ~subt ~iterations ~how ~signwd ~check_signature =

  (* Key kind to get the subtable salt. *)
  let salt_key_kind = mk_key (Subtable_Builtin subt) (get_semi_uncrypted how) in

  (* Read subtable salt and max_extra_key *)
  let loc = Subtable (name, subt) in
  let sub_salt = builtin_get loc ~bad_passwd:false handler salt_key_kind ~key:Config.salt_key

  and max_extra_key =
    let sval = builtin_get loc ~bad_passwd:false handler salt_key_kind ~key:Config.max_extra_key_key in
    try int_of_string sval 
    with Failure _ -> raiserror (Corrupted (loc, Printf.sprintf "Max_key_pad is not bound to a number as expected. Found: %s" sval))

  and max_extra_data =
    if status = Read then 0
    else 
      let sval = builtin_get loc ~bad_passwd:false handler salt_key_kind ~key:Config.max_extra_data_key in
      try int_of_string sval 
      with Failure _ -> raiserror (Corrupted (loc, Printf.sprintf "Max_data_pad is not bound to a number as expected. Found: %s" sval))      
  in

  (* Signword *)
  let signwd =
    if signwd = "" then Cipher.empty_passwd
    else Cipher.mk_passwd ~iterations (Config.add_salt sub_salt signwd)
  in

  let how = get_salted_key_how ~iterations sub_salt max_extra_key how in

  let builtin_key_kind = mk_key (Subtable_Builtin subt) how
  and user_key_kind    = mk_key (Subtable_User subt) how in

  let subtable = 
    { status ;
      handler = Some handler ;
      name ;
      subt ;
      sub_salt ;
      max_extra_data ;
      user_key_kind ;
      builtin_key_kind ;
      subsignwd = signwd }
  in
  
  (* Check test binding. *)
  let data = builtin_get loc ~bad_passwd:true handler builtin_key_kind ~key:Config.test_key in
  if data <> Config.test_data then
    raiserror (Corrupted (loc, Printf.sprintf "Corrupted test binding: found %s instead of %s." data Config.test_data)) ;

  (* Check signature *)
  if check_signature then compute_and_check_signature subtable ;

  subtable

let open_read handler ~name ~subt ~iterations ~how ~signwd =
  open_aux (Operations.cast handler) Read ~name ~subt ~iterations ~how ~signwd ~check_signature:true

let open_append handler ~name ~subt ~iterations ~how ~signwd ~check_signature =
  open_aux handler (Full handler) ~name ~subt ~iterations ~how ~signwd ~check_signature

let open_full handler ~name ~subt ~iterations ~how ~signwd ~max_extra_key ~max_extra_data =

  if subt > Kinds.max_subtable then raiserror Subtable_overflow ;

  (* Key kind to write the subtable salt. *)
  let salt_key_kind = mk_key (Subtable_Builtin subt) (get_semi_uncrypted how) in

  (* Create salt *)
  let sub_salt = Utils.random_string Utils.gen Config.salt_size in

  (* Write salt and max_extras to the database. *)
  Operations.add ~may_overwrite:false handler salt_key_kind ~max_extra_data:0 ~key:Config.salt_key ~data:sub_salt ;
  Operations.add ~may_overwrite:false handler salt_key_kind ~max_extra_data:0 ~key:Config.max_extra_key_key ~data:(string_of_int max_extra_key) ;
  Operations.add ~may_overwrite:false handler salt_key_kind ~max_extra_data:0 ~key:Config.max_extra_data_key ~data:(string_of_int max_extra_data) ;

  (* Signword *)
  let signwd =
    if signwd = "" then Cipher.empty_passwd
    else Cipher.mk_passwd ~iterations (Config.add_salt sub_salt signwd)
  in

  let how = get_salted_key_how ~iterations sub_salt max_extra_key how in

  let builtin_key_kind = mk_key (Subtable_Builtin subt) how
  and user_key_kind    = mk_key (Subtable_User subt) how in

  let subtable =
    { status = Full handler ;
      handler = Some handler ;
      name ;
      subt ;
      sub_salt ;
      max_extra_data ;
      builtin_key_kind ;
      user_key_kind ;
      subsignwd = signwd }
  in

  (* Write test binding *)
  Operations.add ~may_overwrite:false handler builtin_key_kind ~max_extra_data ~key:Config.test_key ~data:Config.test_data ;  

  subtable

let check_writeable sub =
  match sub.status with
  | Closed -> error is_closed sub
  | Read -> assert false
  | Full _ -> ()

let add ?may_overwrite sub ~key ~data =
  check_writeable sub ;
  match sub.handler with
  | None -> assert false
  | Some handler ->
      try Operations.add ?may_overwrite handler sub.user_key_kind ~max_extra_data:sub.max_extra_data ~key ~data
      with Error (Overwrite (key, Any)) -> raiserror (Overwrite (key, Subtable (sub.name, sub.subt)))

let find sub key =
  if sub.status = Closed then error is_closed sub ;
  match sub.handler with
  | None -> raiserror (Unbound (key, Subtable (sub.name, sub.subt)))
  | Some handler ->
      try Operations.get handler sub.user_key_kind ~key
      with Error (Unbound (key, Any)) -> raiserror (Unbound (key, Subtable (sub.name, sub.subt)))

let delete sub key =
  check_writeable sub ;
  match sub.handler with
  | None -> assert false
  | Some handler ->
      try Operations.remove handler sub.user_key_kind ~key
      with Error (Unbound (_, Any)) -> raiserror (Unbound (key, Subtable (sub.name, sub.subt)))

let is_bound sub key =
  try let _ = find sub key in true with Error (Unbound _) -> false

let iterkey sub f =
  if sub.status = Closed then error is_closed sub ;
  match sub.handler with
  | None -> ()
  | Some handler ->
      let (table_passwd, subpass) = get_table_passwds sub in
      Operations.iter_subtable handler table_passwd ~subt:sub.subt ~subpass 
	begin fun loc key -> 
	  match loc with
	  | Kinds.Subtable_User _ -> f key
	  | _ -> ()
	end

let iter sub f = iterkey sub (fun key -> f key (try find sub key with _ -> assert false))

let fold sub acu f =
  let acu = ref acu in
  iter sub (fun key data -> acu := f key data !acu) ;
  !acu

let clear sub =
  check_writeable sub ;
  let (table_passwd, _) = get_table_passwds sub in
  let all_keys = ref (Setp.empty Pervasives.compare) in

  (* We cannot iterate over and remove the elements at the same time. *)
  match sub.handler with
  | None -> assert false (* Empty subtable is not in full mode. *)
  | Some handler ->
      Operations.iter_subtable_encrypted handler table_passwd ~subt:sub.subt
	begin fun loc encoded_key ->
	  match loc with
	  | Kinds.Table_Builtin | Kinds.Subtable_Builtin _ -> ()
	  | Subtable_User n ->
	      assert (n = sub.subt) ;
	      all_keys := Setp.add encoded_key !all_keys ;
	end ;
      
      Setp.iter (Operations.remove_encrypted handler) !all_keys ;
      ()

let remove_signature sub =
  match sub.handler with
  | None -> assert false (* Empty subtable not in full move. *)
  | Some handler ->
      Signature.remove_subtable_signature handler sub.builtin_key_kind ~subt:sub.subt
