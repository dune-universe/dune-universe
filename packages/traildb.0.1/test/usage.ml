let create_empty_traildb () =
  try
    let db = TrailDB.Cons.open_w "/tmp/foo" ["measure";"value"] in
    TrailDB.Cons.finalize db
  with TrailDB.Error err -> begin
    prerr_endline (TrailDB.error_str err);
    assert false
  end

let read_empty_traildb () =
  try
    let db = TrailDB.open_r "/tmp/foo" in
    assert (TrailDB.num_trails db = 0L);
    assert (TrailDB.num_events db = 0L);
    assert (TrailDB.num_fields db = 3L);
    assert (TrailDB.get_field_name db 0L = Some "time");
    assert (TrailDB.get_field_name db 1L = Some "measure");
    assert (TrailDB.get_field db "value" = Some 2L);
    assert (TrailDB.get_field_name db 6L = None);
    assert (TrailDB.get_field db "foo" = None)
  with TrailDB.Error err -> begin
    prerr_endline (TrailDB.error_str err);
    assert false
  end

let create_simple_traildb () =
  try
    let db = TrailDB.Cons.open_w "/tmp/bar" ["measure";"value"] in
    let uuid = Uuidm.(v5 ns_oid "trail_0") in
    TrailDB.Cons.add db uuid  123456L ["temperature";"12"];
    TrailDB.Cons.add db uuid  123457L ["temperature";"13"];
    TrailDB.Cons.add db uuid  123458L ["temperature";"14"];
    TrailDB.Cons.add db uuid  123459L ["temperature";"13"];
    TrailDB.Cons.finalize db
  with TrailDB.Error err -> begin
    prerr_endline (TrailDB.error_str err);
    assert false
  end

let the = function 
  | Some x -> x
  | None -> raise Not_found

let ($) f x = f x

let read_simple_traildb () =
  try
    let db = TrailDB.open_r "/tmp/bar" in
    let uuid = Uuidm.(v5 ns_oid "trail_0") in
    assert (TrailDB.num_trails db = 1L);
    assert (TrailDB.num_events db = 4L);
    assert (TrailDB.num_fields db = 3L);
    assert (TrailDB.min_timestamp db = 123456L);
    assert (TrailDB.max_timestamp db = 123459L);
    assert (TrailDB.lexicon_size db (the $ TrailDB.get_field db "measure") = 2L); (* "" + "temperature" *)
    assert (TrailDB.lexicon_size db (the $ TrailDB.get_field db "value") = 4L); (* "" + all values *)
    assert (TrailDB.get_trail_id db uuid = Some 0L);
    assert (TrailDB.get_trail_id db (Uuidm.(v5 ns_oid "xxx")) = None);
    assert (TrailDB.get_uuid db 0L = Some uuid);
    assert (TrailDB.get_uuid db 1L = None);
    let cursor = TrailDB.Cursor.create db in
    TrailDB.Cursor.get_trail cursor 0L;
    assert (TrailDB.Cursor.get_trail_length cursor = 4L);
    TrailDB.Cursor.get_trail cursor 0L;
    let event = TrailDB.Cursor.peek cursor in
    begin
      assert ((the event).timestamp = 123456L);
      assert (List.length (the event).values = 2);
      match (the event).values with
      | [item1;item2] ->
        assert (TrailDB.get_item_field item1 = (the $ TrailDB.get_field db "measure"));
        assert (TrailDB.get_item_value db item1 = "temperature");
        assert (TrailDB.get_item_field item2 = (the $ TrailDB.get_field db "value"));
        assert (TrailDB.get_item_value db item2 = "12");
      | _ -> assert false
    end;
    let event = TrailDB.Cursor.next cursor in assert ((the event).timestamp = 123456L);
    let event = TrailDB.Cursor.next cursor in assert ((the event).timestamp = 123457L);
    let event = TrailDB.Cursor.next cursor in assert ((the event).timestamp = 123458L);
    let event = TrailDB.Cursor.next cursor in assert ((the event).timestamp = 123459L);
    let event = TrailDB.Cursor.next cursor in assert (event = None);
  with TrailDB.Error err -> begin
    prerr_endline (TrailDB.error_str err);
    assert false
  end

let _ =
  create_empty_traildb ();
  read_empty_traildb ();
  create_simple_traildb ();
  read_simple_traildb ()
