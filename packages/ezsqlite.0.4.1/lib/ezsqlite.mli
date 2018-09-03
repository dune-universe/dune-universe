exception Sqlite_error of string

module Value : sig
    (** Datatypes that can be stored by SQLite *)
    type value =
        | Null
        | Blob of Bytes.t
        | Text of string
        | Double of float
        | Integer of Int64.t

    type kind =
        | INTEGER
        | DOUBLE
        | TEXT
        | BLOB
        | NULL

    exception Invalid_type

    val is_null : value -> bool
    val to_string : value -> string
    val get_string : value -> string
    val get_bytes : value -> Bytes.t
    val get_float : value -> float
    val get_int : value -> int
    val get_int64 : value -> int64
    val get_bool : value -> bool
end

open Value

(** sqlite3 handle *)
type t

(** Load database from file *)
val load : string -> t

val auto_extension : (t -> unit) -> unit
val commit_hook : (unit -> int) -> unit
val update_hook : (int -> string -> string -> int64 -> unit) -> unit
val create_function : t -> string -> int -> (value array -> value) -> unit

(** sqlite3_stmt handle*)
type stmt

(** Prepare an SQL statement *)
val prepare : t -> string -> stmt

(** Reset a statement -- this does not unbind bound values *)
val reset : stmt -> unit

(** Reset and clear bindings*)
val clear : stmt -> unit

val bind : stmt -> int -> value -> unit
val bind_dict : stmt -> (string * value) list -> unit
val bind_list : stmt -> value list -> unit

(** Get the number of parameters *)
val parameter_count : stmt -> int

(** Get the index of a named parameter *)
val parameter_index : stmt -> string -> int

val text : stmt -> int -> string
val blob : stmt -> int -> Bytes.t
val int64 : stmt -> int -> int64
val int : stmt -> int -> int
val double : stmt -> int -> float

(** Get a value by index *)
val column : stmt -> int -> value

(** Execute a statement that returns no response *)
val exec : stmt -> unit

val step : stmt -> bool

(** Iterate over each step *)
val iter : stmt -> (stmt -> unit) -> unit

(** Iterate over each step returning a value each time *)
val map : stmt -> (stmt -> 'a) -> 'a list

(** Fold a function over each step returning a single accumulated value *)
val fold : stmt -> (stmt -> 'a -> 'a) -> 'a -> 'a

(** Convert a string into a statement and execute it *)
val run :  t -> string -> ?bind:value list  -> (stmt -> 'a) -> 'a list

val run_ign : t -> string -> ?bind:value list -> unit -> unit

(** Get each column as an array *)
val data : stmt -> value array

(** Get each column as a list of tuples mapping from string to value *)
val dict : stmt -> (string * value) list

(** Get a value's type by index *)
val column_type : stmt -> int -> kind

(** Get the number of columns with data *)
val data_count : stmt -> int

(** Get the name of the database a statement is attached to *)
val database_name : stmt -> int -> string

(** Get the name of a table a statement is attached to *)
val table_name : stmt -> int -> string

(** Get the name of a column a statement value is bound to *)
val origin_name : stmt -> int -> string

(* Get the name of a column by index *)
val column_name : stmt -> int -> string

val dump_sql : t -> string -> string

module Backup : sig
    type backup

    val init : t -> string -> t -> string -> backup
    val step : backup -> int -> bool
    val remaining : backup -> int
    val pagecount : backup -> int
end

module Blob : sig
    type blob

    val open_blob : t -> ?dbname:string -> ?rw:bool -> string -> string -> int64 -> blob
    val close : blob -> unit
    val reopen : blob -> int64 -> unit
    val length : blob -> int
    val read : blob -> ?offs:int -> Bytes.t -> int -> unit
    val write : blob -> ?offs:int -> Bytes.t -> unit
end

module Infix : sig
    val (|~) : t -> string -> t
    val (|-) : t -> string * value list -> t
    val (|+) : t -> string * (stmt -> 'a) -> 'a list
    val (|$) : t -> string * value list * (stmt -> 'a) -> 'a list
end
