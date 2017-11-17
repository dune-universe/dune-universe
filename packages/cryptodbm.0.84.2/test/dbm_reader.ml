open Cryptodbm_internals
open Cmdhelper
open Helper

let dump file =
  let handler = Operations.open_read ~file in
  Operations.iter_all handler 
  begin fun encoded_key encoded_data ->
    let showkey =
      match Kinds.get_key_info Cipher.empty_passwd ~subt_pas:(fun _ -> Cipher.empty_passwd) encoded_key with
      | None -> convert (Kinds.encodedkey2s encoded_key)
      | Some (kind, None) -> convert (Kinds.encodedkey2s encoded_key) ^ "//" ^ kind2s kind
      | Some (kind, Some key) -> key ^ "//" ^ kind2s kind
    in
    Printf.printf " [%s] => [%s]\n" showkey (convert (Kinds.encodeddata2s encoded_data))
  end ;

  Operations.close handler ;
  ()

let export t binfile = Cryptodbm.export t ~binfile

let import binfile dbfile = Cryptodbm.import ~binfile ~dbfile

let show_sub sub =
  let nb = Cryptodbm.get_number sub
  and nm = Cryptodbm.get_name sub in
  Cryptodbm.iter sub (fun key data -> Printf.printf "#%d (%s) [%s] => [%s]\n" nb nm (convert key) (convert data)) ;
  ()

let show_table tab =
  Printf.printf "----- Cryptodbm content -----\n\n" ;
  Cryptodbm.iter_subtables tab (fun key nb -> Printf.printf " - Subtable #%d [%s]\n" nb key) ;
  Cryptodbm.iter_uncrypted_subtables tab (fun key nb -> Printf.printf " - Uncrypted subtable #%d [%s]\n" nb key) ;
  Printf.printf "\n" ;
  Cryptodbm.iter_extra_bindings tab (fun key data -> Printf.printf " - Extra : [%s] => [%s]\n" (convert key) (convert data)) ;
  ()
  


let show_help () =
  Printf.printf "Syntax:\n" ;
  Printf.printf "  - The quotes are part of the syntax.\n" ;
  Printf.printf "  - Optional arguments, indicated by ?, can be empty, and a default value will be chosen. \n" ;
  Printf.printf "\n" ;
  Printf.printf "  ***** General commands *****\n" ;
  Printf.printf "    help\n" ;
  Printf.printf "    quit\n" ;
  Printf.printf "\n" ;
  Printf.printf "    dump \"file\"   (shows as much as possible about this file). \n" ;
  Printf.printf "\n" ;

  Printf.printf "  ***** Cryptodbm commands *****\n\n" ;
  Printf.printf "    var = ropen \"file\" \"signwd\"\n" ;
  Printf.printf "\n" ;
  Printf.printf "    var = open \"file\" \"passwd\" \"signwd\"\n" ;
  Printf.printf "    var = append \"file\" \"passwd\" \"signwd\" \"check_signature:bool\"\n" ;
  Printf.printf "    var = create \"file\" \"?overwrite:bool\" \"passwd\" \"signwd\" \"?max_extra_key\" \"?max_extra_data\" \"?max_extra_bindings\" \"perm\"\n" ;
  Printf.printf "    create \"passwd\" \"signwd\"  (shortcut: creates a table bound to variable 'tab' in a file 'table' and a subtable bound to \"sub\").\n" ;
  Printf.printf "    close \"var\"\n" ;
  Printf.printf "    flush \"?backup\" \"?backup_name\" \"var\"\n" ;
  Printf.printf "\n" ;
  Printf.printf "    show \"var\"\n  (shows this table structure)." ;
  Printf.printf "\n\n" ;

  Printf.printf "  ***** Subtable commands *****\n\n" ;

  Printf.printf "   var = screate \"table-var\" \"name\" \"passwd\" \"signwd\" \"?max_extra_key\" \"?max_extra_data\"\n" ;
  Printf.printf "   var = sopen \"table-var\" \"name\" \"passwd\" \"signwd\"\n" ;
  Printf.printf "   var = sappend \"table-var\" \"name\" \"passwd\" \"signwd\" \"check_signature:bool\"\n" ;
  Printf.printf "   close \"var\"\n" ;

  Printf.printf "\n" ;

  Printf.printf "   var = ucreate \"table-var\" \"name\" \"signwd\"   (create uncrypted subtable)\n" ;
  Printf.printf "   var = uopen \"table-var\" \"name\" \"signwd\"     (open uncrypted subtable)\n" ;
  Printf.printf "   var = uappend \"table-var\" \"name\" \"signwd\" \"check_signature\"  (append uncrypted subtable)\n" ;

  Printf.printf "\n" ;

  Printf.printf "   put \"?may_overwrite:bool\" \"subtable-var\" \"key\" \"data\"\n" ;
  Printf.printf "   get \"subtable-var\" \"key\"\n" ;
  Printf.printf "   del \"subtable-var\" \"key\"\n" ;

  Printf.printf "\n" ;

  Printf.printf "   info \"subtable-var\"\n" ;
  Printf.printf "   show \"var\"\n  (shows this subtable content).\n" ;

  Printf.printf "  ***** Durable backup *****\n\n" ;

  Printf.printf "   export \"table\" \"file\"\n" ;
  Printf.printf "   import \"binfile\" \"dbfile\"\n" ;

  Printf.printf "\n\n" ;
  ()

let commands =
  [ (builtin "help" (fun () -> show_help ())) ;
    (builtin "quit" (fun () -> exit 0)) ;

    (fun env l -> Scanf.sscanf l "info \"%s@\""
	(fun var ->
	  begin match get env var with
	  | Ftable t -> Printf.printf "%s is a table with full access.\n" var
	  | Rtable t -> Printf.printf "%s is a table with read-only access.\n" var
	  | RSub t -> Printf.printf "%s is subtable #%d (%s)\n" var (Cryptodbm.get_number t) (Cryptodbm.get_name t)
	  | FSub t -> Printf.printf "%s is subtable #%d (%s)\n" var (Cryptodbm.get_number t) (Cryptodbm.get_name t)
	  end ;
	  env)) ;

    (fun env l -> Scanf.sscanf l "dump \"%s@\"" (fun file -> dump file ; env)) ;

    (fun env l -> Scanf.sscanf l "%s = open \"%s@\" \"%s@\" \"%s@\""
	(fun var file passwd signwd ->
	  assign env var (Rtable (Cryptodbm.open_read ~file ~passwd ~signwd ())))) ;

    (fun env l -> Scanf.sscanf l "%s = ropen \"%s@\" \"%s@\""
	(fun var file signwd ->
	  assign env var (Rtable (Cryptodbm.open_only_uncrypted ~file ~signwd ())))) ;

    (fun env l -> Scanf.sscanf l "%s = append \"%s@\" \"%s@\" \"%s@\" \"%s@\""
	(fun var file passwd signwd check ->
	  let check_signature = to_bool check in
	  assign env var (Ftable (Cryptodbm.open_append ~file ~passwd ~signwd ~check_signature ())))) ;

    (fun env l -> Scanf.sscanf l "%s = create \"%s@\" \"%s@\" \"%s@\" \"%s@\" \"%s@\" \"%s@\" \"%s@\" \"%s@\""
	(fun var file overwrite passwd signwd max_extra_key max_extra_data max_extra_bindings perm ->
	  let overwrite = to_obool overwrite
	  and max_extra_key = to_oint max_extra_key
	  and max_extra_data = to_oint max_extra_data
	  and max_extra_bindings = to_oint max_extra_bindings
	  and perm = to_int ("0o" ^ perm) in
	  assign env var (Ftable (Cryptodbm.open_create ~file ?overwrite ~passwd ~signwd ?max_extra_key ?max_extra_data ?max_extra_bindings ~perm ())))) ;

    (fun env l -> Scanf.sscanf l "create \"%s@\" \"%s@\""
	(fun passwd signwd ->
	  let tab = Cryptodbm.open_create ~file:"table" ~passwd ~signwd ~perm:0o644 () in
	  let sub = Cryptodbm.create_subtable tab ~name:"sub" ~passwd:"" ~signwd:"" () in
	  let env1 = assign env "tab" (Ftable tab) in
	  let env2 = assign env1 "sub" (FSub sub) in
	  env2)) ;


    (fun env l -> Scanf.sscanf l "close \"%s@\"" 
	(fun var ->
	  match get env var with
	  | Ftable t -> Cryptodbm.close t ; env
	  | Rtable t -> Cryptodbm.close t ; env
	  | FSub t -> Cryptodbm.close_subtable t ; env
	  | RSub t -> Cryptodbm.close_subtable t ; env)) ;
(*	  | _ -> failwith "bad argument for close" )) ; *)

    (fun env l -> Scanf.sscanf l "flush \"%s@\" \"%s@\" \"%s@\""
	(fun backup backup_name var ->
	  let backup = to_obool backup
	  and backup_name = optional backup_name in
	  match get env var with
	  | Ftable t -> Cryptodbm.flush ?backup ?backup_name t ; env
	  | _ -> failwith "bad argument for flush")) ;

    (fun env l -> Scanf.sscanf l "%s = screate \"%s@\" \"%s@\" \"%s@\" \"%s@\" \"%s@\" \"%s@\""
	(fun var tablevar name passwd signwd max_extra_key max_extra_data ->
	  let max_extra_key = to_oint max_extra_key
	  and max_extra_data = to_oint max_extra_data in
	  match get env tablevar with
	  | Ftable t -> assign env var (FSub (Cryptodbm.create_subtable t ~name ~passwd ~signwd ?max_extra_key ?max_extra_data ()))
	  | _ -> failwith "bad argument for create_subtable")) ;

    (fun env l -> Scanf.sscanf l "%s = ucreate \"%s@\" \"%s@\" \"%s@\""
	(fun var tablevar name signwd ->
	  match get env tablevar with
	  | Ftable t -> assign env var (FSub (Cryptodbm.create_uncrypted_subtable t ~name ~signwd ()))
	  | _ -> failwith "bad argument for create_uncrypted_subtable")) ;

    (fun env l -> Scanf.sscanf l "%s = sopen \"%s@\" \"%s@\" \"%s@\" \"%s@\""
	(fun var tablevar name passwd signwd ->
	  match get env tablevar with
	  | Ftable t -> assign env var (RSub (Cryptodbm.open_subtable t ~name ~passwd ~signwd ()))
	  | Rtable t -> assign env var (RSub (Cryptodbm.open_subtable t ~name ~passwd ~signwd ()))
	  | _ -> failwith "bad argument for open_subtable")) ;

    (fun env l -> Scanf.sscanf l "%s = uopen \"%s@\" \"%s@\" \"%s@\""
	(fun var tablevar name signwd ->
	  match get env tablevar with
	  | Ftable t -> assign env var (RSub (Cryptodbm.open_uncrypted_subtable t ~name ~signwd ()))
	  | Rtable t -> assign env var (RSub (Cryptodbm.open_uncrypted_subtable t ~name ~signwd ()))
	  | _ -> failwith "bad argument for open_uncrypted_subtable")) ;

    (fun env l -> Scanf.sscanf l "%s = sappend \"%s@\" \"%s@\" \"%s@\" \"%s@\" \"%s@\""
	(fun var tablevar name passwd signwd check_signature ->
	  let check_signature = to_bool check_signature in
	  match get env tablevar with
	  | Ftable t -> assign env var (FSub (Cryptodbm.append_subtable t ~name ~passwd ~signwd ~check_signature ()))
	  | _ -> failwith "bad argument for append_subtable")) ;

    (fun env l -> Scanf.sscanf l "%s = uappend \"%s@\" \"%s@\" \"%s@\" \"%s@\""
	(fun var tablevar name signwd check_signature ->
	  let check_signature = to_bool check_signature in
	  match get env tablevar with
	  | Ftable t -> assign env var (FSub (Cryptodbm.append_uncrypted_subtable t ~name ~signwd ~check_signature ()))
	  | _ -> failwith "bad argument for append_uncrypted_subtable")) ;

    (fun env l -> Scanf.sscanf l "put \"%s@\" \"%s@\" \"%s@\" \"%s@\""
	(fun overwrite subtable key data ->
	  let may_overwrite = to_obool overwrite in
	  match get env subtable with
	  | FSub t -> Cryptodbm.add ?may_overwrite t ~key ~data ; env
	  | _ -> failwith "bad argument for put")) ;

    (fun env l -> Scanf.sscanf l "get \"%s@\" \"%s@\""
	(fun subtable key ->
	  match get env subtable with
	  | FSub t -> Printf.printf "  ==> %s\n" (Cryptodbm.find t key) ; env
	  | RSub t -> Printf.printf "  ==> %s\n" (Cryptodbm.find t key) ; env
	  | _ -> failwith "bad argument for get")) ;

    (fun env l -> Scanf.sscanf l "del \"%s@\" \"%s@\""
	(fun subtable key ->
	  match get env subtable with
	  | FSub t -> Cryptodbm.delete t key ; env
	  | _ -> failwith "bad argument for delete")) ;

    (fun env l -> Scanf.sscanf l "show \"%s@\""
	(fun var ->
	  begin match get env var with
	  | FSub t -> show_sub t
	  | RSub t -> show_sub t
	  | Rtable t -> show_table t
	  | Ftable t -> show_table t
	  end ;
	  env)) ;

    (fun env l -> Scanf.sscanf l "export \"%s@\" \"%s@\""
	(fun var file ->
	  begin match get env var with
	  | Rtable t -> export t file
	  | _ -> failwith "Bad argument for export"
	  end ;
	  env)) ;

    (fun env l -> Scanf.sscanf l "import \"%s@\" \"%s@\"" import ; env)
   ]

let () =
  Printf.printf "Type 'help' if necessary.\n%!" ;

  let rec loop env =
    Printf.printf "> %!" ;
    let l = read_line () in
    let env' = exec env l commands in
    loop env'
  in
  
  loop (new_env ()) 
