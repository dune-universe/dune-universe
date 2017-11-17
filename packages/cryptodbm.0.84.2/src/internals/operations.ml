open Kinds
open Types

type dbm_handler = 
    { (* Handler to the underlying database. *)
      mutable dbm: LowerDB.dbm ;
      
      (* Current status. *)
      mutable status: unit status ; 
      
      (* Underlying database file *)
      dbfile: LowerDB.dbfile ;
    
      (* In write mode, file permission. *)
      perm: int }

type 'a handler = dbm_handler

let get_rootfile h = LowerDB.get_root h.dbfile

let cast h = h

let open_read ~file =
  let dbfile = LowerDB.mk_file file in
  if not (LowerDB.is_readable dbfile) then raiserror (File_not_found file) ;
  { dbm = LowerDB.open_dbm dbfile `Read ~perm:0 ;
    status = Read ;
    dbfile ;
    perm = 0 }

let open_full ?(overwrite=false) ~file ~perm =
  let dbfile = LowerDB.mk_file file in
  if perm land 0o200 = 0 then raiserror (File_not_writeable file) ;

  if LowerDB.exists dbfile then 
    if overwrite then LowerDB.delete dbfile
    else raiserror (File_overwrite file) ;

  { dbm = LowerDB.open_dbm dbfile `Write ~perm ;
    status = Full () ;
    dbfile ;
    perm }

let open_append ~file =
  let dbfile = LowerDB.mk_file file in
  if not (LowerDB.is_appendable dbfile) then raiserror (File_not_appendable file) ;
  let perm = LowerDB.get_perm dbfile in

  { dbm = LowerDB.open_dbm dbfile `Append ~perm ;
    status = Full () ;
    dbfile ;
    perm }

let close h =
  if h.status = Closed then raiserror (Is_Closed Any) ;
  LowerDB.close h.dbm ;
  h.status <- Closed ;
  ()

let create_backup ?backup_name h =
  (* This db should be closed. *)
  assert (h.status = Closed) ; 

  begin
    let backup_name = match backup_name with
    | None -> (LowerDB.get_root h.dbfile) ^ "-backup-" ^ (Utils.date_to_string (Unix.time ()))
    | Some n -> n
    in
    try LowerDB.copy h.dbfile backup_name
    with e -> raiserror (Backup_failure e)
  end ;

  ()


let flush ?(backup=false) ?backup_name h =
  close h ;
  if backup || backup_name <> None then create_backup ?backup_name h ;
  h.dbm <- LowerDB.open_dbm h.dbfile `Append ~perm:h.perm ;
  h.status <- Full () ;
  ()

let mkr = Cipher.mk_weak_passwd

(* Builds a data_kind from a key_kind, a key, and a max_data_padding *)
let build_data_kind key_kind key max_pad =
  let how =
    match key_kind.key_how with
    | Uncrypted -> Uncrypted
    | Encrypted (passwd, subpasswd, _) ->
	let sloc = loc2hash key_kind.key_loc in
	let datapasswd = Cipher.concat [mkr (sloc ^ "\003") ; passwd ; mkr ("@+" ^ key ^ "\nX") ; subpasswd] in
	Encrypted (datapasswd, max_pad)
  in
  mk_data how

let get handler key_kind ~key =
  if handler.status = Closed then raiserror (Is_Closed Any) ;
  try 
    (* Get the encoded data using the encoded key *)
    let encoded_data = LowerDB.find handler.dbm (encode_key key key_kind) in

    (* Decode the encoded data using the data kind associated to this key. *)
    decode_data encoded_data (build_data_kind key_kind key 0)

  with Not_found -> raiserror (Unbound (key, Any))

(* Check write permission on this handler. *)
let check_writable handler =
  match handler.status with
  | Read -> assert false
  | Closed -> raiserror (Is_Closed Any)
  | Full () -> ()

let add ?(may_overwrite=false) handler key_kind ~max_extra_data ~key ~data =
  check_writable handler ;

  (* Encode key *)
  let encoded_key = encode_key key key_kind

  (* Encode data using the data kind associated to this key. *)
  and encoded_data = encode_data data (build_data_kind key_kind key max_extra_data) in

  (* Add the binding to the database. *)
  try
    (if may_overwrite then LowerDB.replace else LowerDB.add) handler.dbm encoded_key encoded_data
  with Error (Overwrite (_, Any)) -> raiserror (Overwrite (key, Any))

let remove_encrypted handler encoded_key =
  check_writable handler ;
  try LowerDB.remove handler.dbm encoded_key
  with Not_found -> raiserror (Unbound ("(uncrypted key)", Any))

let remove handler kind ~key = remove_encrypted handler (encode_key key kind)

let empty_subt_pas _ = Cipher.empty_passwd

let iter_uncrypted handler passwd f =
  if handler.status = Closed then raiserror (Is_Closed Any) ;
  let explore_encoded_key encoded_key =
    match get_key_info passwd ~subt_pas:empty_subt_pas encoded_key with
    | None -> ()
    | Some (_, None) -> ()
    | Some (kind, Some key) -> f kind.key_loc key
  in
  LowerDB.iterkey explore_encoded_key handler.dbm

let iter_subtable handler passwd ~subt ~subpass f =

  if handler.status = Closed then raiserror (Is_Closed Any) ;

  let subt_pas n = if n = subt then subpass else Cipher.empty_passwd in
  
  let explore_encoded_key encoded_key =
    match get_key_info passwd ~subt_pas:subt_pas encoded_key with
    | None -> ()
    | Some (_, None) -> ()
    | Some (kind, Some key) ->
	let loc = kind.key_loc in
	begin match loc with
	| Table_Builtin -> ()
	| Subtable_Builtin n | Subtable_User n -> 
	    if n = subt then f loc key else ()
	end
  in
  LowerDB.iterkey explore_encoded_key handler.dbm

let iter_subtable_encrypted handler passwd ~subt f =
  if handler.status = Closed then raiserror (Is_Closed Any) ;

  let explore_encoded_key encoded_key =
    match get_key_info passwd ~subt_pas:(fun _ -> Cipher.empty_passwd) encoded_key with
    | None -> ()
    | Some (kind, _) ->
	let loc = kind.key_loc in
	begin match loc with
	| Table_Builtin -> ()
	| Subtable_Builtin n | Subtable_User n ->
	    if n = subt then f loc encoded_key else ()
	end
  in
  LowerDB.iterkey explore_encoded_key handler.dbm

let iter_all handler f = 
  if handler.status = Closed then raiserror (Is_Closed Any) ;
  LowerDB.iter f handler.dbm

let get_encrypted handler key =
  if handler.status = Closed then raiserror (Is_Closed Any) ;
  LowerDB.find handler.dbm key

  
