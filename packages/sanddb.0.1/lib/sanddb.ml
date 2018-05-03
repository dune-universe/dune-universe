open Lwt.Infix

module Record_Id = struct
  include Uuidm
end

module type Database = sig
  type t
  val file_path : string
  val read_all_records : unit -> (Record_Id.t * t, exn) result list Lwt.t
  val read_visible_records : unit -> (Record_Id.t * t, exn) result list Lwt.t
  val insert_record : t -> Record_Id.t Lwt.t
  val insert_shadowing_record : Record_Id.t -> t -> Record_Id.t Lwt.t
end

let deserialize_record_data (type a) record_data_serializer record_data =
  let open Serializer_converter in
  let module Record_Data_Serializer = (val record_data_serializer : Serializer with type t = a) in
  Record_Data_Serializer.t_of_string record_data

let deserialize_record_content record_data_serializer deserialized_record = 
  let open Record_t in
  let record_id = Base.Result.of_option ~error: (Failure "Record id parsing failed") (Record_Id.of_string deserialized_record.id) in
  let record_data = Base.Result.try_with (fun () -> deserialize_record_data record_data_serializer deserialized_record.data) in
  match record_id, record_data with
  | Ok id, Ok data -> Ok (id, data)
  | Error _ as error ,  _ -> error
  | _ , (Error _ as error) -> error

let deserialize_record record_serializer record_data_serializer record =
  let open Serializer_converter in
  let open Record_t in
  let module Record_Serializer = (val record_serializer : Serializer with type t = Record_t.t) in
  let unescaped_record = Caml.Scanf.unescaped record in
  let deserialized_record = Base.Result.try_with (fun () -> Record_Serializer.t_of_string unescaped_record)  in
  Base.Result.bind deserialized_record ~f:(deserialize_record_content record_data_serializer)

let database_read_all_records file_path record_serializer record_data_serializer =
  Lwt_io.with_file ~mode: Input file_path (fun channel -> Lwt_io.read channel) >>= fun raw_data ->
  let cleaned_raw_data = String.trim raw_data in
  let raw_records = String.split_on_char '\n' cleaned_raw_data in
  let serializer = deserialize_record record_serializer record_data_serializer in
  let records = Base.List.map ~f:serializer raw_records in
  Lwt.return records

let filter_duplicate_record record (unique_record_ids, accumulator) =
  match record with
  | Ok (id, data) ->
    if Base.List.mem unique_record_ids id ~equal: Record_Id.equal  then 
      (unique_record_ids, accumulator)
    else
      (id :: unique_record_ids , record :: accumulator)
  | error -> (unique_record_ids, error :: accumulator)  

let database_read_visible_records file_path record_serializer record_data_serializer =
  database_read_all_records file_path record_serializer record_data_serializer >>= fun records ->
  let (_, visible_records) = Base.List.fold_right records
      ~f:filter_duplicate_record
      ~init:([],[]) in
  Lwt.return visible_records

let serialize_record_data (type a) record_data_serializer record_data =
  let open Serializer_converter in
  let module Record_Data_Serializer = (val record_data_serializer : Serializer with type t = a) in
  Record_Data_Serializer.string_of_t record_data

let serialize_record record_serializer record_data_serializer record_id record_data =
  let open Serializer_converter in
  let open Record_t in
  let module Record_Serializer = (val record_serializer : Serializer with type t = Record_t.t) in
  let serialized_record_data = serialize_record_data record_data_serializer record_data in
  let serialized_id = Record_Id.to_string record_id in
  let record = {id = serialized_id; data = serialized_record_data} in
  let serialized_record = Record_Serializer.string_of_t record in 
  Caml.String.escaped serialized_record

let write_to_file file_path serialized_record =
  Lwt_io.with_file 
    ~flags:([Unix.O_WRONLY; Unix.O_NONBLOCK; Unix.O_APPEND; Unix.O_CREAT]) 
    ~mode: Output 
    file_path 
    (fun channel -> Lwt_io.write_line channel serialized_record)

let database_insert_record file_path record_serializer record_data_serializer record_data =
  let record_id = Record_Id.v `V4 in 
  let serialized_record = serialize_record record_serializer record_data_serializer record_id record_data in
  write_to_file file_path serialized_record >>= fun () ->
  Lwt.return record_id

let database_insert_shadowing_record file_path record_serializer record_data_serializer record_id record_data =
  let serialized_record = serialize_record record_serializer record_data_serializer record_id record_data in
  write_to_file file_path serialized_record >>= fun () ->
  Lwt.return record_id

let create_database_module (type a) file_path record_serializer record_data_serializer = 
  (module struct
    type t = a
    let file_path = file_path
    let read_all_records () = database_read_all_records file_path record_serializer record_data_serializer
    let read_visible_records () = database_read_visible_records file_path record_serializer record_data_serializer
    let insert_record record_data  = database_insert_record file_path record_serializer record_data_serializer record_data
    let insert_shadowing_record record_id record_data = database_insert_shadowing_record file_path record_serializer record_data_serializer record_id record_data
  end : Database with type t = a)

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

let read_all_records (type a) (module Database : Database with type t = a) =
  Database.read_all_records

let read_visible_records (type a) (module Database : Database with type t = a) =
  Database.read_visible_records

let insert_record (type a) (module Database : Database with type t = a) (record_data : a) =
  Database.insert_record record_data

let insert_shadowing_record (type a) (module Database : Database with type t = a) record_id (record_data : a) =
  Database.insert_shadowing_record record_id record_data