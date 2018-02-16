let uuid_of_trail = Uuidm.v5 Uuidm.ns_oid

let insert_event db = function
  | trail::timestamp::values ->
    let uuid = uuid_of_trail trail in
    let time = Int64.of_string timestamp in
    TrailDB.Cons.add db uuid time values;
    db
  | trail::_ -> raise (Invalid_argument "missing timestamp")
  | _ -> raise (Invalid_argument "missing uuid and timestamp")

let create_traildb ~file ~fields ~events =
  let db = TrailDB.Cons.open_w file fields in
  List.fold_left insert_event db events
  |> TrailDB.Cons.finalize

let read_events db cursor =
  let rec loop events =
    match TrailDB.Cursor.next cursor with
    | None -> List.rev events
    | Some event ->
      let timestamp = Int64.to_string (event.TrailDB.timestamp) in
      let values = List.map (TrailDB.get_item_value db) event.TrailDB.values in
      loop ((timestamp::values)::events)
  in
  loop []

let read_trail ~db ~id =
  let cursor = TrailDB.Cursor.create db in
  begin
    TrailDB.Cursor.get_trail cursor id ;
    read_events db cursor
  end
  
let traildb_trail ~file ~trail =
  let db = TrailDB.open_r file in
  match TrailDB.get_trail_id db (uuid_of_trail trail) with
  | None -> None
  | Some id -> Some (read_trail ~db ~id)

let traildb_metadata ~getter ~file =
  let db = TrailDB.open_r file in
  getter db

let traildb_num_trails = traildb_metadata ~getter:TrailDB.num_trails
let traildb_num_events = traildb_metadata ~getter:TrailDB.num_events
let traildb_num_fields = traildb_metadata ~getter:TrailDB.num_fields
let traildb_min_timestamp = traildb_metadata ~getter:TrailDB.min_timestamp
let traildb_max_timestamp = traildb_metadata ~getter:TrailDB.max_timestamp
