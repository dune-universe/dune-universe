module Database = Database
module Record_id = Record_id
module Serializer = Serializer

(** [Sanddb.create_json_database file_name json_serializer] will create a json database based on the provided:
 - [file_name] which will be tha base of the database
 - [json_serializer] which will be responsible for the serialization between the database and the client
 This function will return a first class database module, which can be used to communicate with the database.
*)
let create_json_database file_path json_serializer = 
  Database.create_json_database file_path json_serializer

(** [Sanddb.create_biniou_database file_name biniou_serializer] will create a biniou database based on the provided:
 - [file_name] which will be tha base of the database
 - [biniou_serializer] which will be responsible for the serialization between the database and the client
 This function will return a first class database module, which can be used to communicate with the database.
*)
let create_biniou_database file_path biniou_serializer =
  Database.create_biniou_database file_path biniou_serializer

(** [Sanddb.read_all_records database unit] gives back every database record, both visible and shadowed ones. The result can contain duplicate record ids.
The first record in the list is the oldest record and the last one is the newest record.
Creates the database file if it doesn't exists.*)
let read_all_records (type a) (module Database : Database.T with type t = a) =
  Database.read_all_records

(** [Sanddb.read_visible_records database unit] gives back every visible(not shadowed) database record, which also means there is no duplicate record id in the result.
The first record in the list is the newest record and the last one is the oldest record.
Creates the database file if it doesn't exists.*)
let read_visible_records (type a) (module Database : Database.T with type t = a) =
  Database.read_visible_records

(** [Sanddb.insert_record database data] inserts record into the database. The record id is generated automatically and given back as a result.
Creates the database file if it doesn't exists.*)
let insert_record (type a) (module Database : Database.T with type t = a) (record_data : a) =
  Database.insert_record record_data

(** [Sanddb.insert_record database data] inserts record into the database with the given record id. 
It can shadow an older record if it has the same record id as the new one.
Creates the database file if it doesn't exists.*)
let insert_shadowing_record (type a) (module Database : Database.T with type t = a) record_id (record_data : a) =
  Database.insert_shadowing_record record_id record_data