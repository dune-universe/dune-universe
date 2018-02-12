let _ =

  (* create a database, here a in-memory tree database. *)
  let db = Kyoto.opendb "+" [Kyoto.OWRITER; Kyoto.OCREATE] in

  (* store records *)
  Kyoto.set db "foo" "hop";
  Kyoto.set db "bar" "step";
  Kyoto.set db "baz" "jump";
  Kyoto.set db "baz2" "jump";
  Kyoto.set db "zoo" "p";

  (* retrieve records *)
  assert (Kyoto.get db "foo" = Some "hop");
  assert (Kyoto.get db "xoxox" = None);

  (* update records *)
  Kyoto.set db "bar" "step2";
  Kyoto.remove db "baz2";

  (* check records *)
  assert (Kyoto.exists db "foo");
  assert (not (Kyoto.exists db "baz2"));

  (* get stats *)
  assert (Kyoto.count db = 4L);
  assert (Kyoto.size db > 10L);
  assert (Kyoto.path db = "+");

  (* fold the whole database *)
  assert (Kyoto.fold db (fun n (k,v) -> n+1) 0 = 4);

  (* fold part of a sorted database *)
  assert (Kyoto.fold_prefix db "ba" (fun pairs (k,v) -> (k,v)::pairs) [] = ["baz","jump";"bar","step2"]);
  assert (Kyoto.fold_range db "bar" "foo" (fun pairs (k,v) -> (k,v)::pairs) [] = ["baz","jump";"bar","step2"]);

  (* use a cursor to iter over the database *)
  let cursor = Kyoto.cursor_open db in
  
  assert (Kyoto.cursor_next cursor = Some ("bar","step2"));
  assert (Kyoto.cursor_next cursor = Some ("baz","jump"));
  assert (Kyoto.cursor_next cursor = Some ("foo","hop"));
  assert (Kyoto.cursor_next cursor = Some ("zoo","p"));
  assert (Kyoto.cursor_next cursor = None);

  Kyoto.cursor_close cursor;

  (* use a cursor to find a given key *)
  let cursor = Kyoto.cursor_open db in

  Kyoto.cursor_jump cursor "bas";  
  assert (Kyoto.cursor_next cursor = Some ("baz","jump"));
  Kyoto.cursor_jump cursor "zz";  
  assert (Kyoto.cursor_next cursor = None);

  Kyoto.cursor_close cursor;

  (* working with transaction *)
  Kyoto.begin_tran db;
  Kyoto.set db "phantom" "opera";
  assert (Kyoto.get db "phantom" = Some "opera");
  Kyoto.abort_tran db;
  assert (Kyoto.get db "phantom" = None);

  Kyoto.begin_tran_sync db;
  Kyoto.set db "phantom" "opera";
  assert (Kyoto.get db "phantom" = Some "opera");
  Kyoto.commit_tran db;
  assert (Kyoto.get db "phantom" = Some "opera");

  Kyoto.with_transaction db (fun db -> Kyoto.set db "new_key" "new_value");
  assert (Kyoto.get db "new_key" = Some "new_value");

  try
    Kyoto.with_transaction db (
      fun db -> Kyoto.set db "yet_another_key" "some_value";
      if Kyoto.exists db "yet_another_key" then raise Not_found else ());
    assert (false) (* an exeption was expected *)
  with Not_found ->
    assert (Kyoto.get db "yet_another_key" = None);

  (* close the database *)
  Kyoto.close db;

  (* Working with an empty db *)
  let empty_db = Kyoto.opendb "+" [Kyoto.OWRITER; Kyoto.OCREATE] in

  assert (Kyoto.fold empty_db (fun n x -> n+1) 0 = 0);
  assert (Kyoto.fold_prefix empty_db "" (fun n x -> n+1) 0 = 0);
  assert (Kyoto.fold_range empty_db "a" "z" (fun n x -> n+1) 0 = 0);

  let cursor = Kyoto.cursor_open empty_db in 
  assert (Kyoto.cursor_next cursor = None);
  Kyoto.cursor_close cursor;

  Kyoto.close empty_db;

  (* Folding a stream into a database *)
  let db = Kyoto.opendb "+" [Kyoto.OWRITER; Kyoto.OCREATE] in
  let append = Kyoto.update db string_of_int (fun acc v -> string_of_int ((int_of_string acc) + v)) in

  append "a" 12;
  append "b" 12;
  append "a" 21;
  append "b" 1;
  append "c" 1;

  assert (Kyoto.get db "a" = Some "33");
  assert (Kyoto.get db "b" = Some "13");
  assert (Kyoto.get db "c" = Some "1");
  assert (Kyoto.get db "d" = None);

  Kyoto.close db;
