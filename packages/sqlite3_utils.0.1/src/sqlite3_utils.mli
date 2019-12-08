module Data = Sqlite3.Data
module Rc = Sqlite3.Rc
type db = Sqlite3.db

exception RcError of Rc.t
(** Exception raised by most of the functions below when a Sqlite failure
    occurs, with the corresponding error code. *)

exception Type_error of Data.t
(** Exception raised when the declared {!Ty.t} does not match the
    actual result returned by Sqlite. *)

type t = db
(** Alias for the DB connection *)

val check_ret : 'a -> Rc.t -> ('a, Rc.t) result
(** Check return code.
    @return [Error rc] if the code is not {!Sqlite3.Rc.DONE} or {!Sqlite3.Rc.OK}. *)

val check_ret_exn : Rc.t -> unit
(** Check return code.
    @raise RcError if the code is not {!Sqlite3.Rc.DONE} or {!Sqlite3.Rc.OK}. *)

val setup_timeout : ?ms:int -> t -> unit
(** on "busy", wait [ms] milliseconds before failing. *)

val with_db :
  ?mode:[ `NO_CREATE | `READONLY ] ->
  ?mutex:[ `FULL | `NO ] ->
  ?cache:[ `PRIVATE | `SHARED ] -> ?vfs:string ->
  ?timeout:int ->
  string -> (t -> 'a) -> 'a
(** Temporarily open a DB connection.
    Parameters follow {!Sqlite3.db_open}.
    @param timeout if provided, timeout in milliseconds before a query fails
      with "BUSY".
*)

(** Values representing types to pass to a statement, or to extract from 
    a row *)
module Ty : sig
  type ('a, 'res) t

  type 'a arg

  val int : int arg
  val int64 : int64 arg
  val float : float arg
  val text : string arg
  val blob : string arg
  val any_str : string arg
  val data : Data.t arg

  val nil : ('res, 'res) t
  (** 0 arguments *)

  val (@>) : 'a arg -> ('b, 'res) t -> ('a -> 'b, 'res) t
  (** Right-associative chaining.
      [int @> float @> nil] is the same as [int (float nil)]. *)

  val p1: 'a arg -> ('a -> 'res, 'res) t
  val p2: 'a arg -> 'b arg -> ('a -> 'b -> 'res, 'res) t
  val p3: 'a arg -> 'b arg -> 'c arg -> ('a -> 'b -> 'c -> 'res, 'res) t
  val p4: 'a arg -> 'b arg -> 'c arg -> 'd arg -> ('a -> 'b -> 'c -> 'd -> 'res, 'res) t

  val id : 'a -> 'a
  val mkp2 : 'a -> 'b -> 'a * 'b
  val mkp3: 'a -> 'b -> 'c -> 'a * 'b * 'c
  val mkp4: 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
end

module Cursor : sig
  type 'a t
  (** A cursor yielding values of type ['a] *)

  val ignore : _ t -> unit
  (** Ignore this cursor *)

  val next : 'a t -> 'a option
  (** Get next value, or [None] if all values have been enumerated *)

  val iter : f:('a -> unit) -> 'a t -> unit
  (** Iterate over the values *)

  val to_seq : 'a t -> 'a Seq.t
  (** Lazy iterator over the values.
      Be careful not to let this leak outside the scope of a statement. *)

  val to_list_rev : 'a t -> 'a list
  (** Get a list of the values in the cursor, in reverse order.
      Faster than {!to_list} *)

  val to_list : 'a t -> 'a list
  (** Get a list of the values in the cursor, in normal order.
      Slower than {!to_list} *)
end

val with_stmt : t -> string -> f:(Sqlite3.stmt -> 'a) -> 'a
(** Locally make a statement out of the given string, then cleanup
    when [f] returns. *)

val exec0 : t -> string -> (unit, Rc.t) result
(** Run the query purely for its side effects. *)

val exec0_exn : t -> string -> unit
(** Run the query purely for its side effects.
    @raise RcError if the query failed.
*)

val exec_raw :
  t ->
  string ->
  f:(Data.t array Cursor.t -> 'b) ->
  ('b, Rc.t) result

val exec_raw_exn :
  t ->
  string ->
  f:(Data.t array Cursor.t -> 'b) ->
  'b

val exec_raw_args :
  t ->
  string ->
  Sqlite3.Data.t array ->
  f:(Data.t array Cursor.t -> 'b) ->
  ('b, Rc.t) result

val exec_raw_args_exn :
  t ->
  string ->
  Sqlite3.Data.t array ->
  f:(Data.t array Cursor.t -> 'b) ->
  'b

val exec :
  t -> string ->
  ty:( ('a, ('res, Rc.t) result) Ty.t * ('b, 'c) Ty.t * 'b ) ->
  f:('c Cursor.t -> 'res) -> 'a

val exec_exn :
  t -> string ->
  ty:( ('a, 'res) Ty.t * ('b, 'c) Ty.t * 'b ) ->
  f:('c Cursor.t -> 'res) -> 'a

val exec_no_params :
  t -> string ->
  ty:(('b, 'c) Ty.t * 'b) ->
  f:('c Cursor.t -> 'res) ->
  ('res,Rc.t) result

val exec_no_params_exn :
  t -> string ->
  ty:(('b, 'c) Ty.t * 'b) ->
  f:('c Cursor.t -> 'res) ->
  'res

val exec_no_cursor :
  t -> string ->
  ty:('a, (unit, Rc.t) result) Ty.t ->
  'a

val exec_no_cursor_exn :
  t -> string ->
  ty:('a, unit) Ty.t ->
  'a

val transact : t -> (t -> 'a) -> 'a

val atomically : t -> (t -> 'a) -> 'a
