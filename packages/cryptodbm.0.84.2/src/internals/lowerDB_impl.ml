open Types

type dbfile =
    { (* Root name, used to create / open dbms. *)
      root: string ;
      files: string list }

let get_root dbfile = dbfile.root


type dbm = Dbm.t

let catcher f x =
  try f x with e -> raiserror (DB_Error e)

let open_dbm dbfile mode ~perm =
  let flags = match mode with
  | `Read -> [Dbm.Dbm_rdonly]
  | `Write -> [Dbm.Dbm_create ; Dbm.Dbm_rdwr]
  | `Append -> [Dbm.Dbm_rdwr]
  in
  catcher (Dbm.opendbm dbfile.root flags) perm
  
let close handler = catcher Dbm.close handler

let find handler key = 
  try Dbm.find handler key
  with
  | Not_found -> raise Not_found
  | e -> raiserror (DB_Error e)

let replace handler key data = catcher (Dbm.replace handler key) data

let remove handler key = 
  try Dbm.remove handler key
  with
  | Dbm.Dbm_error "dbm_delete" -> raise Not_found
  | e -> raiserror (DB_Error e)

let add handler key data = 
  try Dbm.add handler key data
  with
  | Dbm.Dbm_error "Entry already exists" -> raiserror (Overwrite (key, Any))
  | e -> raiserror (DB_Error e)

let iterkey f handler =
  try
    (* First key *)
    f (catcher Dbm.firstkey handler) ;

    (* Next keys *)
    while true do f (Dbm.nextkey handler) done

  with Not_found -> () (* Done *)
    
let iter f handler = 
  try Dbm.iter f handler
  with 
  | (Dbm.Dbm_error _) as e -> raiserror (DB_Error e)


(********************  DB-File management  *******************)

type db_mode =
  (* Only one file, named like the root. *)
  | Single_Mode

  (* Two files: root.dir and root.pag. Both files are actually the same node (one is a hard link to the other). *)
  | NDBM_Mode

(* The mode is a global variable. It is platform-dependent. *)
let mode =
  (* In order to discover the mode, we create a dummy database in a dummy directory. *)
  let dummydir = "/tmp/__dummy_cryptodbm_autotest_dir" in
  let dummyfile = dummydir ^ "/" ^ "testdb" in

  (* Clean test directory. *)
  Utils.rmrf dummydir ;
  Utils.mkdir dummydir ;

  let dbfile = { root = dummyfile ; files = [] } in
  let db = open_dbm dbfile `Write ~perm:0o600 in
  add db "testkey" "testdata" ;
  close db ;

  let finalmode = 
    if Utils.file_exists dummyfile then Single_Mode
    else if Utils.file_exists (dummyfile ^ ".pag") && Utils.file_exists (dummyfile ^ ".dir") then NDBM_Mode
    else 
      failwith (Printf.sprintf "Could not determine the underlying database layout (gdbm or ndbm). See files in the test directory: %s" dummydir)
  in

  (* Clean test directory. *)
  Utils.rmrf dummydir ;
  finalmode


type file_operations =
    { mk_file: (string -> dbfile) ;
      exists: (dbfile -> bool) ;
      delete: (dbfile -> unit) ;
      get_perm: (dbfile -> int) ;
      copy: (dbfile -> string -> unit) ;
      is_readable: (dbfile -> bool) ;
      is_appendable: (dbfile -> bool) }

let file_ops =
  match mode with
  | Single_Mode ->
      let mk_file path = { root  = path ; files = [] }
      and exists dbfile = Utils.file_exists dbfile.root
      and delete dbfile = Utils.remove [dbfile.root]
      and get_perm dbfile = Utils.read_perm dbfile.root
      and copy dbfile backup = Utils.cp dbfile.root backup
      and is_readable dbfile = Utils.is_readable dbfile.root
      and is_appendable dbfile = Utils.is_appendable dbfile.root
      in
      { mk_file ; exists ; delete ; get_perm ; copy ; is_readable ; is_appendable }

  | NDBM_Mode ->
      let mk_file path = { root = path ; files = [ path ^ ".pag" ; path ^ ".dir" ] }
      and exists dbfile = List.exists Utils.file_exists dbfile.files
      and delete dbfile = Utils.remove dbfile.files
      and get_perm dbfile = List.fold_left (fun acu file -> acu land Utils.read_perm file) 0xfff dbfile.files
      and is_readable dbfile = List.fold_left (fun acu file -> acu && Utils.is_readable file) true dbfile.files
      and is_appendable dbfile = List.fold_left (fun acu file -> acu && Utils.is_appendable file) true dbfile.files
      in
      let copy dbfile backup =
	(* Just copy the first file, the second is a hard link to the first. *)
	match dbfile.files, (mk_file backup).files with
	| [pag1 ; dir1], [pag2 ; dir2] ->
	    Utils.cp pag1 pag2 ;
	    (* Ignore the second file for the time being. *)
	    ()

	| _ -> assert false
      in
      { mk_file ; exists ; delete ; get_perm ; copy ; is_readable ; is_appendable }

let mk_file = file_ops.mk_file
let exists  = file_ops.exists
let delete  = file_ops.delete
let get_perm = file_ops.get_perm
let copy     = file_ops.copy
let is_readable = file_ops.is_readable
let is_appendable = file_ops.is_appendable

