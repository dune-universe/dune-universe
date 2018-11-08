(** [Sanddb.Database] contains the type of the database and it's internal implementation.*)

(** Database.T is the (module) type of the database. 
  In SandDB every database instance will have this type regardles of the serializing method.
*)
module type T = sig
  type t
  val file_path : string
  val read_all_records : unit -> (Record_id.t * t, exn) result list Lwt.t
  val read_visible_records : unit -> (Record_id.t * t, exn) result list Lwt.t
  val insert_record : t -> Record_id.t Lwt.t
  val insert_shadowing_record : Record_id.t -> t -> Record_id.t Lwt.t
end

let database_read_all_records file_path record_serializer record_data_serializer =
  let open Lwt.Infix in
  File_io.read_from_file file_path >>= fun raw_data ->
  let records = Record_serializer.deserialize_records record_serializer record_data_serializer raw_data in
  Lwt.return records

let filter_duplicate_record record (unique_record_ids, accumulator) =
  match record with
  | Ok (id, _) ->
    if Base.Set.mem unique_record_ids id then 
      (unique_record_ids, accumulator)
    else
      (Base.Set.add unique_record_ids id, Base.List.cons record accumulator)
  | error -> (unique_record_ids, Base.List.cons error accumulator)  

let database_read_visible_records file_path record_serializer record_data_serializer =
  let open Lwt.Infix in
  database_read_all_records file_path record_serializer record_data_serializer >>= fun records ->
  let (_, visible_records) = Base.List.fold_right records
      ~f:filter_duplicate_record
      ~init:(Base.Set.empty (module Record_id), []) in
  Lwt.return visible_records

let database_insert_record file_path record_serializer record_data_serializer record_data =
  let open Lwt.Infix in
  let record_id = Record_id.create_random_id () in 
  let serialized_record = Record_serializer.serialize_record record_serializer record_data_serializer record_id record_data in
  File_io.append_to_file file_path serialized_record >>= fun () ->
  Lwt.return record_id

let database_insert_shadowing_record file_path record_serializer record_data_serializer record_id record_data =
  let open Lwt.Infix in
  let serialized_record = Record_serializer.serialize_record record_serializer record_data_serializer record_id record_data in
  File_io.append_to_file file_path serialized_record >>= fun () ->
  Lwt.return record_id

let create_database_module (type a) file_path record_serializer record_data_serializer = 
  (module struct
    type t = a
    let file_path = file_path
    let read_all_records () = database_read_all_records file_path record_serializer record_data_serializer
    let read_visible_records () = database_read_visible_records file_path record_serializer record_data_serializer
    let insert_record record_data  = database_insert_record file_path record_serializer record_data_serializer record_data
    let insert_shadowing_record record_id record_data = database_insert_shadowing_record file_path record_serializer record_data_serializer record_id record_data
  end : T with type t = a)

let create_json_database file_path json_serializer =
  let open Serializer_converter in
  let record_serializer = convert_json_serializer (module Record_j) in
  let record_data_serializer = convert_json_serializer json_serializer in
  create_database_module file_path record_serializer record_data_serializer

let create_biniou_database file_path biniou_serializer =
  let open Serializer_converter in
  let record_serializer = convert_biniou_serializer (module Record_b) in
  let record_data_serializer = convert_biniou_serializer biniou_serializer in
  create_database_module file_path record_serializer record_data_serializer