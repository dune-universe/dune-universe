(** [Sanddb.Record_Id] is the basic record id type, which idetifies a record in the database.*)
module Record_Id : sig
  type t = Uuidm.t
  type version = [ `V3 of t * string | `V4 | `V5 of t * string ]
  val v : version -> t
  val create : version -> t
  val v3 : t -> string -> t
  val v5 : t -> string -> t
  val v4_gen : Random.State.t -> unit -> t
  val nil : t
  val ns_dns : t
  val ns_url : t
  val ns_oid : t
  val ns_X500 : t
  val compare : t -> t -> int
  val equal : t -> t -> bool
  val of_bytes : ?pos:int -> string -> t option
  val to_bytes : t -> string
  val unsafe_to_bytes : t -> string
  val of_string : ?pos:int -> string -> t option
  val to_string : ?upper:bool -> t -> string
  val pp : Format.formatter -> t -> unit
  val pp_string : ?upper:bool -> Format.formatter -> t -> unit
  val print : ?upper:bool -> Format.formatter -> t -> unit
end

(** [Sanddb.Database] is the basic module type that will be used to communicate with the database.*)
module type Database = sig
  type t
  val file_path : string
  val read_all_records : unit -> (Record_Id.t * t, exn) result list Lwt.t
  val read_visible_records : unit -> (Record_Id.t * t, exn) result list Lwt.t
  val insert_record : t -> Record_Id.t Lwt.t
  val insert_shadowing_record : Record_Id.t -> t -> Record_Id.t Lwt.t
end

(** [Sanddb.create_json_database file_name json_serializer] will create a json database based on the provided:
 - [file_name] which will be tha base of the database
 - [json_serializer] which will be responsible with the serialization between the database and the client
 This function will return a first class database module, which can be used to communicate with the database.
*)
val create_json_database : Lwt_io.file_name -> (module Serializers.Json_Serializer with type t = 'a) -> (module Database with type t = 'a)

(** [Sanddb.create_biniou_database file_name biniou_serializer] will create a biniou database based on the provided:
 - [file_name] which will be tha base of the database
 - [biniou_serializer] which will be responsible with the serialization between the database and the client
 This function will return a first class database module, which can be used to communicate with the database.
*)
val create_biniou_database : Lwt_io.file_name -> (module Serializers.Biniou_Serializer with type t = 'a) -> (module Database with type t = 'a)

(** [Sanddb.read_all_records database unit] gives back every database record, both visible and shadowed ones. The result can contain duplicate record ids.
The first record in the list is the oldest record and the last one is the newest record.
Creates the database file if it doesn't exists.*)
val read_all_records : (module Database with type t = 'a) -> unit -> (Record_Id.t * 'a, exn) result list Lwt.t

(** [Sanddb.read_visible_records database unit] gives back every visible(not shadowed) database record, which also means there is no duplicate record id in the result.
The first record in the list is the newest record and the last one is the oldest record.
Creates the database file if it doesn't exists.*)
val read_visible_records : (module Database with type t = 'a) -> unit -> (Record_Id.t * 'a, exn) result list Lwt.t

(** [Sanddb.insert_record database data] inserts record into the database. The record id is generated automatically and given back as a result.
Creates the database file if it doesn't exists.*)
val insert_record : (module Database with type t = 'a) -> 'a -> Record_Id.t Lwt.t

(** [Sanddb.insert_record database data] inserts record into the database with the given record id. 
It can shadow an older record if it has the same record id as the new one.
Creates the database file if it doesn't exists.*)
val insert_shadowing_record : (module Database with type t = 'a) -> Record_Id.t -> 'a -> Record_Id.t Lwt.t