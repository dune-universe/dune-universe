type tdb_cons
type tdb
type path = string
type uuid = Uuidm.t
type timestamp = int64
type field_name = string
type field_value = string
type trail_id = int64
type tbd_field = int64
type tdb_item = int64
type tdb_event = { timestamp: timestamp; values: tdb_item list }

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

exception Error of error

let _ =
  Callback.register_exception "traildb.error" (Error(TDB_ERR_NOMEM))

module Cons = struct
  external open_w: path -> field_name list -> tdb_cons = "ocaml_tdb_cons_open"
  external add: tdb_cons -> string -> timestamp -> field_value list -> unit = "ocaml_tdb_cons_add"
  external finalize: tdb_cons -> unit = "ocaml_tdb_cons_finalize"
  external append: tdb_cons -> tdb -> unit = "ocaml_tdb_cons_append"

  let add tdb_cons uuid timestamp fields =
    add tdb_cons (Uuidm.to_bytes uuid) timestamp fields
end

external open_r: path -> tdb = "ocaml_tdb_open"
external dontneed: tdb -> unit = "ocaml_tdb_dontneed"
external willneed: tdb -> unit = "ocaml_tdb_willneed"
external num_trails: tdb -> int64 = "ocaml_tdb_num_trails"
external num_events: tdb -> int64 = "ocaml_tdb_num_events"
external min_timestamp: tdb -> timestamp = "ocaml_tdb_min_timestamp"
external max_timestamp: tdb -> timestamp = "ocaml_tdb_max_timestamp"
external get_uuid: tdb -> trail_id -> string option = "ocaml_tdb_get_uuid"
external get_trail_id: tdb -> string -> trail_id option = "ocaml_tdb_get_trail_id"
external num_fields: tdb -> int64 = "ocaml_tdb_num_fields"
external get_field: tdb -> string -> tbd_field option = "ocaml_tdb_get_field"
external get_field_name: tdb -> tbd_field -> string option = "ocaml_tdb_get_field_name"
external lexicon_size: tdb -> tbd_field -> int64 = "ocaml_tdb_lexicon_size"
external get_item_field: tdb_item -> tbd_field = "ocaml_tdb_item_field"
external get_item_value: tdb -> tdb_item -> string = "ocaml_tdb_get_item_value"
external error_str: error -> string = "ocaml_tdb_error_str"

let get_uuid tdb trail_id =
  match get_uuid tdb trail_id with
  | None -> None
  | Some uuid -> Uuidm.of_bytes uuid

let get_trail_id tdb uuid =
  get_trail_id tdb (Uuidm.to_bytes uuid)

module Cursor = struct
  type tdb_cursor
  external create: tdb -> tdb_cursor = "ocaml_tdb_cursor_new"
  external get_trail: tdb_cursor -> trail_id -> unit = "ocaml_tdb_get_trail"
  external get_trail_length: tdb_cursor -> int64 = "ocaml_tdb_get_trail_length"
  external next: tdb_cursor -> tdb_event option = "ocaml_tdb_cursor_next"
  external peek: tdb_cursor -> tdb_event option = "ocaml_tdb_cursor_peek"
end

module Filter = struct
  type tdb_event_filter
  type literal = Pos of tdb_item | Neg of tdb_item
  type disjunction = Or of literal list
  type conjunction = And of disjunction list

  let create conjunction = raise (Invalid_argument "TODO")
end
