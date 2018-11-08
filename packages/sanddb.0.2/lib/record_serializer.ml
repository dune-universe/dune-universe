let deserialize_record_data (type a) record_data_serializer record_data =
  let module Record_Data_Serializer = (val record_data_serializer : Serializer.Generic_serializer with type t = a) in
  Record_Data_Serializer.t_of_string record_data

let deserialize_record_content record_data_serializer deserialized_record = 
  let open Record_t in
  let record_id = Base.Result.of_option ~error: (Failure "Record id parsing failed") (Record_id.of_string deserialized_record.id) in
  let record_data = Base.Result.try_with (fun () -> deserialize_record_data record_data_serializer deserialized_record.data) in
  match record_id, record_data with
  | Ok id, Ok data -> Ok (id, data)
  | Error _ as error ,  _ -> error
  | _ , (Error _ as error) -> error

let deserialize_record record_serializer record_data_serializer record =
  let module Record_Serializer = (val record_serializer : Serializer.Generic_serializer with type t = Record_t.t) in
  let unescaped_record = Caml.Scanf.unescaped record in
  let deserialized_record = Base.Result.try_with (fun () -> Record_Serializer.t_of_string unescaped_record)  in
  Base.Result.bind deserialized_record ~f:(deserialize_record_content record_data_serializer)

let deserialize_records record_serializer record_data_serializer raw_data =
  let cleaned_raw_data = String.trim raw_data in
  let raw_records = String.split_on_char '\n' cleaned_raw_data in
  let serializer = deserialize_record record_serializer record_data_serializer in
  Base.List.map ~f:serializer raw_records

let serialize_record_data (type a) record_data_serializer record_data =
  let module Record_Data_Serializer = (val record_data_serializer : Serializer.Generic_serializer with type t = a) in
  Record_Data_Serializer.string_of_t record_data

let serialize_record record_serializer record_data_serializer record_id record_data =
  let open Record_t in
  let module Record_Serializer = (val record_serializer : Serializer.Generic_serializer with type t = Record_t.t) in
  let serialized_record_data = serialize_record_data record_data_serializer record_data in
  let serialized_id = Record_id.to_string record_id in
  let record = {id = serialized_id; data = serialized_record_data} in
  let serialized_record = Record_Serializer.string_of_t record in 
  Caml.String.escaped serialized_record