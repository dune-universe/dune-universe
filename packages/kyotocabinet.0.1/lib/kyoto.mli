(** A key,value store database. *)
type db

(** A cursor used to iterate over (key,value) pairs. *)
type cursor

(** Exception raised when something gone wrong with the database. *)
exception Error of string

type open_flag =
|  OREADER
|  OWRITER
|  OCREATE
|  OTRUNCATE
|  OAUTOTRAN
|  OAUTOSYNC
|  ONOLOCK
|  OTRYLOCK
|  ONOREPAIR

(** Open a a database file.

  The kind of the database is infered from the path,
  and tuning parameters can trail the path.
  These conventions are inherited from kyotocabinet::PolyDB::open() :
  http://fallabs.com/kyotocabinet/api/classkyotocabinet_1_1PolyDB.html#a09384a72e6a72a0be98c80a1856f34aa

  If it is "-", the database will be a prototype hash database.
  If it is "+", the database will be a prototype tree database.
  If it is ":", the database will be a stash database.
  If it is "*", the database will be a cache hash database.
  If it is "%", the database will be a cache tree database.
  If its suffix is ".kch", the database will be a file hash database.
  If its suffix is ".kct", the database will be a file tree database.
  If its suffix is ".kcd", the database will be a directory hash database.
  If its suffix is ".kcf", the database will be a directory tree database.
  If its suffix is ".kcx", the database will be a plain text database.

  Tuning parameters can trail the name, separated by "#".
  Each parameter is composed of the name and the value, separated by "=".

  If the "type" parameter is specified,
  the database type is determined by the value in
  "-", "+", ":", "*", "%", "kch", "kct", "kcd", "kcf", and "kcx".

  The tuning parameter "log" specifies the path of the log file,
  or "-" for the standard output,
  or "+" for the standard error.

  The parameter "logkinds" specifies kinds of logged messages
  and the value can be "debug", "info", "warn", or "error".

  "logpx" specifies the prefix of each log message.
*)
external opendb: string -> open_flag list -> db = "kc_open"

(** Close the database file. *)
external close: db -> unit = "kc_close"

(** [with_db path flags f] opens a database, calls f on this db and ensures the db is finally closed. *)
val with_db: string -> open_flag list -> (db -> 'a) -> 'a

(** Return the count of key, value pairs. *)
external count: db -> int64 = "kc_count"

(** Return the size of the database file. *)
external size: db -> int64 = "kc_size"

(** Return the path of the database. *)
external path: db -> string = "kc_path"

(** Return a string status of the database. *)
external status: db -> string = "kc_status"

(** [exists db key] checks if any data is associated to the given [key] in the database [db]. *)
external exists: db -> string -> bool = "kc_exists"

(** [get db key] returns the data associated with the given [key] in the database [db], if any. *)
external get: db -> string -> string option = "kc_get"

(** [find db key] returns the data associated with the given [key], or raise Not_found if none is found. *)
external find: db -> string -> string = "kc_find"

(** [set db key data] inserts the pair ([key], [data]) in the database [db].

   If the database already contains data associated with the [key],
   that data is discarded and silently replaced by the new [data]. *)
external set: db -> string -> string -> unit = "kc_set"

(** [add db key data] inserts the pair ([key], [data]) in the database [db].

   If the database already contains data associated with the [key],
   it raises Invalid_Argument("Entry already exists"). *)
external add: db -> string -> string -> unit = "kc_add"

(** [replace db key data] inserts the pair ([key], [data]) in the database [db].

   If the database doesn't contain any data associated with the [key],
   it raises Not_found. *)
external replace: db -> string -> string -> unit = "kc_replace"

(** [remove db key] removes the data associated to the [key] in the database [db].

   If [key] has no associated data, simply do nothing.*)
external remove: db -> string -> unit = "kc_remove"

(** [update db init plus key data] updates the value associated to the given [key].

   If a previous value [v] is associated to the [key],
   then the value to be inserted is computed after [plus v data].

   If there is no data associated to the [key],
   then the value to be inserted is [init data].
*)
val update: db -> ('a -> string) -> (string -> 'a -> string) -> string -> 'a -> unit

(** [fold db combiner seed] folds the whole content of the database [db].*)
val fold: db -> ('a -> (string*string) -> 'a) -> 'a -> 'a

(** [fold db prefix combiner seed] folds the [(key,value)] pairs
    having a key with the given [prefix].

   This is meaningful only for sorted databases, i.e. tree databases. *)
val fold_prefix: db -> string -> ('a -> (string*string) -> 'a) -> 'a -> 'a

(** [fold db (Some min_key) (Some max_key) combiner seed] folds the [(key,value)] pairs
    having a key in the range [min_key] (inclusive) .. [max_key] (exclusive).

   This is meaningful only for sorted databases, i.e. tree databases. *)
val fold_range: db -> string -> string -> ('a -> (string*string) -> 'a) -> 'a -> 'a

(** Open a cursor and jump to the first key,value pair if any. *)
external cursor_open: db -> cursor = "kc_cursor_open"
(** Jump to the first key,value pair having a key greater than the given key. *)
external cursor_jump: cursor -> string -> unit = "kc_cursor_jump"
(** Read the next key,value pair if any. *)
external cursor_next: cursor -> (string*string) option = "kc_cursor_next"
(** Close the cursor. *)
external cursor_close: cursor -> unit = "kc_cursor_close"

(** [with_cursor db f] opens a cursor, calls f on this cursor and ensures the cursor is finally closed. *)
val with_cursor: db -> (cursor -> 'a) -> 'a

(** begin a transaction with no file synchronization (safe on process crash, but not system crash). *)
external begin_tran: db -> unit = "kc_begin_tran"

(** begin a transaction with file synchronization (safe on process or system crash). *)
external begin_tran_sync: db -> unit = "kc_begin_tran_sync"

(** commit the current transaction *)
external commit_tran: db -> unit = "kc_commit_tran"

(** abort the current transaction *)
external abort_tran: db -> unit = "kc_abort_tran"

(** Execution a function within a transaction. *)
val with_transaction: db -> (db -> 'a) -> 'a

(** Execution a function within a transaction with file synchronization. *)
val with_transaction_sync: db -> (db -> 'a) -> 'a
