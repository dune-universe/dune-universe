(* A TrailDB constructor handler *)
type tdb_cons

(* A TrailDB read-only handler *)
type tdb

(* A file path *)
type path = string

(* A trail uuid *)
type uuid = Uuidm.t

(* A timestamp *)
type timestamp = int64

(* A field name *)
type field_name = string

(* A field value *)
type field_value = string

(* --------------------------------- *)
(* Working with TraiDB constructors. *)
(* --------------------------------- *)

module Cons : sig
  (* Open a new TrailDB for writes. *)
  val open_w: path -> field_name list -> tdb_cons

  (* Add an event to TrailDB. *)
  val add: tdb_cons -> uuid -> timestamp -> field_value list -> unit

  (* Finalize TrailDB construction, creating a valid TrailDB file *)
  val finalize: tdb_cons -> unit

  (* Merge an existing TrailDB to this constructor.
     The fields must be equal between the existing and the new TrailDB. *)
  val append: tdb_cons -> tdb -> unit
end

(* --------------------------------- *)
(* Working with read-only TraiDB.    *)
(* --------------------------------- *)

(* Open a TrailDB for reading. *)
val open_r: path -> tdb

(* Inform the operating system that this TrailDB does not need to be kept in memory. *)
val dontneed: tdb -> unit

(* Inform the operating system that this TrailDB will be accessed soon.
   Call this after tdb_dontneed once the TrailDB is needed again. *)
val willneed: tdb -> unit

(* Get the number of trails. *)
val num_trails: tdb -> int64

(* Get the number of events. *)
val num_events: tdb -> int64

(* Get the oldest timestamp. *)
val min_timestamp: tdb -> timestamp

(* Get the newest timestamp. *)
val max_timestamp: tdb -> timestamp

(* ------------------------------------- *)
(* Working with trails and uuids.        *)
(* ------------------------------------- *)

(* A trail identifier. *)
type trail_id = int64

(* Get the UUID given a trail ID. This is a fast O(1) operation. *)
val get_uuid: tdb -> trail_id -> uuid option

(* Get the trail ID given a UUID. This is an O(log N) operation. *)
val get_trail_id: tdb -> uuid -> trail_id option

(* --------------------------------------------- *)
(* Working with events, items, fields and values *)
(* --------------------------------------------- *)

(* A field identifier. *)
type tbd_field = int64

(* A field value pair *)
type tdb_item

(* An event *)
type tdb_event = { timestamp: timestamp; values: tdb_item list }

(* Get the number of fields. *)
val num_fields: tdb -> int64

(* Get the field ID given a field name. *)
val get_field: tdb -> string -> tbd_field option

(* Get the field name given a field ID. *)
val get_field_name: tdb -> tbd_field -> string option

(* Get the number of distinct values in the given field. *)
val lexicon_size: tdb -> tbd_field -> int64

(* Extract the field ID from an item. *)
val get_item_field: tdb_item -> tbd_field

(* Get the value corresponding to an item. *)
val get_item_value: tdb -> tdb_item -> string

(* -------------------- *)
(* Working with cursor  *)
(* -------------------- *)

module Cursor : sig
  (* A cursor over events of a TrailDB read-only database. *)
  type tdb_cursor

  (* Create a new cursor handle. *)
  val create: tdb -> tdb_cursor

  (* Reset the cursor to the given trail ID. *)
  val get_trail: tdb_cursor -> trail_id -> unit

  (* Get the number of events remaining in this cursor.
     Note that this function consumes the cursor.
     You need to reset it with [tdb_get_trail] to get more events. *)
  val get_trail_length: tdb_cursor -> int64

  (* Consume the next event from the cursor. *)
  val next: tdb_cursor -> tdb_event option

  (* Return the next event from the cursor without consuming it. *)
  val peek: tdb_cursor -> tdb_event option
end

(* ---------------------------------------------------------------------------------------- *)
(* Filter events                                                                            *)
(*                                                                                          *)
(* An event filter is a boolean query over fields, expressed in conjunctive normal form.    *)
(* Once assigned to a cursor, only the subset of events that match the query are returned.  *)
(* ---------------------------------------------------------------------------------------- *)

module Filter : sig
  type tdb_event_filter

  type literal = Pos of tdb_item | Neg of tdb_item
  type disjunction = Or of literal list
  type conjunction = And of disjunction list

  val create: conjunction -> tdb_event_filter
end

(* ----------------------------------------------------- *)
(* Working with errors.                                  *)
(*                                                       *)
(* All functions may raise exceptions of the Error type. *)
(* ----------------------------------------------------- *)

(* Error codes *)
type error = 
  (* generic *)
  | TDB_ERR_NOMEM
  | TDB_ERR_PATH_TOO_LONG
  | TDB_ERR_UNKNOWN_FIELD
  | TDB_ERR_UNKNOWN_UUID
  | TDB_ERR_INVALID_TRAIL_ID
  | TDB_ERR_HANDLE_IS_NULL
  | TDB_ERR_HANDLE_ALREADY_OPENED
  | TDB_ERR_UNKNOWN_OPTION
  | TDB_ERR_INVALID_OPTION_VALUE
  | TDB_ERR_INVALID_UUID
  
  (* io *)
  | TDB_ERR_IO_OPEN
  | TDB_ERR_IO_CLOSE
  | TDB_ERR_IO_WRITE
  | TDB_ERR_IO_READ
  | TDB_ERR_IO_TRUNCATE
  | TDB_ERR_IO_PACKAGE
  
  (* tdb_open *)
  | TDB_ERR_INVALID_INFO_FILE
  | TDB_ERR_INVALID_VERSION_FILE
  | TDB_ERR_INCOMPATIBLE_VERSION
  | TDB_ERR_INVALID_FIELDS_FILE
  | TDB_ERR_INVALID_UUIDS_FILE
  | TDB_ERR_INVALID_CODEBOOK_FILE
  | TDB_ERR_INVALID_TRAILS_FILE
  | TDB_ERR_INVALID_LEXICON_FILE
  | TDB_ERR_INVALID_PACKAGE
  
  (* tdb_cons *)
  | TDB_ERR_TOO_MANY_FIELDS
  | TDB_ERR_DUPLICATE_FIELDS
  | TDB_ERR_INVALID_FIELDNAME
  | TDB_ERR_TOO_MANY_TRAILS
  | TDB_ERR_VALUE_TOO_LONG
  | TDB_ERR_APPEND_FIELDS_MISMATCH
  | TDB_ERR_LEXICON_TOO_LARGE
  | TDB_ERR_TIMESTAMP_TOO_LARGE
  | TDB_ERR_TRAIL_TOO_LONG
  
  (* querying *)
  | TDB_ERR_ONLY_DIFF_FILTER
  | TDB_ERR_NO_SUCH_ITEM
  | TDB_ERR_INVALID_RANGE
  | TDB_ERR_INCORRECT_TERM_TYPE

(* TrailDB exceptions *)
exception Error of error

(* Translate an error code to a string. *)
val error_str: error -> string
