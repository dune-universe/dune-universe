
(** {1 Sqlite3_utils: higher-level helpers around Sqlite3} *)

(** {2 Base Types and Aliases} *)

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

val err_string : ('a, Rc.t) result -> ('a, string) result
(** Turn the error into a string using {!Rc.to_string} *)

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

(** {2 Type Combinators} *)

(** Values representing types to pass to a statement, or to extract from 
    a row *)
module Ty : sig
  type ('a, 'res) t
  (** A representation of a type that returns ['res].
      ['a] is typically a function type with the arguments one would expect,
      for example [(int -> string -> 'res, 'res) t] would be used for
      a query that is parametrized by two values of type int and string
      respectively. *)

  type 'a arg

  val int : int arg

  val int64 : int64 arg
  val float : float arg
  val text : string arg
  val blob : string arg
  val any_str : string arg
  val data : Data.t arg

  val nil : ('res, 'res) t
  (** 0 type arguments *)

  val (@>) : 'a arg -> ('b, 'res) t -> ('a -> 'b, 'res) t
  (** Right-associative chaining.
      [int @> float @> nil] represents two arguments
      of type [int] and [float] respectively, and is the same as [int @> (float @> nil)].
  *)

  val p1: 'a arg -> ('a -> 'res, 'res) t
  (** Exactly one argument of type ['a] *)

  val p2: 'a arg -> 'b arg -> ('a -> 'b -> 'res, 'res) t
  (** Exactly two arguments of types ['a] and ['b] respectively. *)

  val p3: 'a arg -> 'b arg -> 'c arg -> ('a -> 'b -> 'c -> 'res, 'res) t
  val p4: 'a arg -> 'b arg -> 'c arg -> 'd arg -> ('a -> 'b -> 'c -> 'd -> 'res, 'res) t
  val p5: 'a arg -> 'b arg -> 'c arg -> 'd arg -> 'e arg -> ('a -> 'b -> 'c -> 'd -> 'e -> 'res, 'res) t
  val p6: 'a arg -> 'b arg -> 'c arg -> 'd arg -> 'e arg -> 'f arg -> ('a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'res, 'res) t

  val (@>>) : ('a, 'b) t -> ('b, 'res) t -> ('a, 'res) t
  (** Right-associative append. This is useful for long lists of types.
      [(p2 int float) @>> (p1 text)] is the same as  [p3 int float text],
      which is the same as [int @> float @> text @> nil].
  *)

  val id : 'a -> 'a
  (** Empty list of arguments *)

  val mkp2 : 'a -> 'b -> 'a * 'b
  (** Make a tuple. Useful in, for example, [Ty.( p2 int text, mkp2)]. *)

  val mkp3: 'a -> 'b -> 'c -> 'a * 'b * 'c
  val mkp4: 'a -> 'b -> 'c -> 'd -> 'a * 'b * 'c * 'd
  val mkp5: 'a -> 'b -> 'c -> 'd -> 'e -> 'a * 'b * 'c * 'd * 'e
  val mkp6: 'a -> 'b -> 'c -> 'd -> 'e -> 'f -> 'a * 'b * 'c * 'd * 'e * 'f
end

(** {2 Cursor API} 

    A Cursor is a special iterator over Sqlite rows of results.
    It should be consumed quickly as it will not survive the call to
    {!exec}, {!exec_raw}, etc.
*)

module Cursor : sig
  type 'a t
  (** A cursor yielding values of type ['a] *)

  val ignore : _ t -> unit
  (** Ignore this cursor *)

  val next : 'a t -> 'a option
  (** Get next value, or [None] if all values have been enumerated *)

  val iter : f:('a -> unit) -> 'a t -> unit
  (** Iterate over the values *)

  val map : f:('a -> 'b) -> 'a t -> 'b t
  (** Map over values of the cursor. Once [map ~f c] is built, [c] should
      not be used. *)

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
(** Run the query purely for its side effects, without any parameter.
    If you need to parametrize this with user inputs, use {!exec_raw}
    or {!exec_raw_args} or {!exec_no_cursor} to use a safe parametrized
    statement instead of builtin a string that is vulnerable to
    SQL injections.
*)

val exec0_exn : t -> string -> unit
(** Run the query purely for its side effects, like {!exec0}.
    @raise RcError if the query failed.
*)

val exec_raw :
  t ->
  string ->
  f:(Data.t array Cursor.t -> 'b) ->
  ('b, Rc.t) result
(** Wrapper around {!Sqlite3}'s prepare statement, returning
    {!Data.t}.
    @param f is given the cursor over results, and can use {!Cursor}
    to turn it into a list, only keep a few values, or aggregate results.
*)

val exec_raw_exn :
  t ->
  string ->
  f:(Data.t array Cursor.t -> 'b) ->
  'b
(** Same as {!exec_raw} but uses {!check_ret_exn} to unwrap errors.
    @raise RcError in case of error.
*)

val exec_raw_args :
  t ->
  string ->
  Sqlite3.Data.t array ->
  f:(Data.t array Cursor.t -> 'b) ->
  ('b, Rc.t) result
(** Similar to {!exec_raw} but also takes an array of parameters that
    will fill the [?] placeholders of the prepare statement. *)

val exec_raw_args_exn :
  t ->
  string ->
  Sqlite3.Data.t array ->
  f:(Data.t array Cursor.t -> 'b) ->
  'b
(** Same as {!exec_raw_args} but uses {!check_ret_exn} to unwrap errors.
    @raise RcError in case of error.
*)

val exec :
  t -> string ->
  ty:( ('a, ('res, Rc.t) result) Ty.t * ('b, 'c) Ty.t * 'b ) ->
  f:('c Cursor.t -> 'res) -> 'a
(** Typesafe alternative to {!exec_raw_args}.
    @param ty describes the type of paramaters and of the returned values.
    @param f is given the cursor over decoded values

    Example:
{[ # with_db ":memory:" (fun db ->
     exec0_exn db "create table person (name text, age int);";
     exec0_exn db "insert into person values ('alice', 20), ('bob', 25) ;";
     exec db "select age from person where name=? ;"
      ~ty:Ty.(p1 text, p1 int, (fun (x:int) -> x))
      "alice"
       ~f:Cursor.to_list);;
- : (int list, Rc.t) result = Ok [20]
]}
*)


val exec_exn :
  t -> string ->
  ty:( ('a, 'res) Ty.t * ('b, 'c) Ty.t * 'b ) ->
  f:('c Cursor.t -> 'res) -> 'a
(** Same as {!exec} but uses {!check_ret_exn} to unwrap the result.
    @raise RcError in case of error. *)

val exec_no_params :
  t -> string ->
  ty:(('b, 'c) Ty.t * 'b) ->
  f:('c Cursor.t -> 'res) ->
  ('res,Rc.t) result
(** Same as {!exec} but without parameters. [ty] only takes two arguments:
    the type description for result rows, and a function to map rows to ['c] *)

val exec_no_params_exn :
  t -> string ->
  ty:(('b, 'c) Ty.t * 'b) ->
  f:('c Cursor.t -> 'res) ->
  'res
(** Same as {!exec_no_params_exn} but uses {!check_ret_exn} to unwrap the result.
    @raise RcError in case of error. *)

val exec_no_cursor :
  t -> string ->
  ty:('a, (unit, Rc.t) result) Ty.t ->
  'a
(** Perform a query that doesn't return any result.
    @param ty the description of the type parameters. *)

val exec_no_cursor_exn :
  t -> string ->
  ty:('a, unit) Ty.t ->
  'a
(** Same as {!exec_no_cursor_exn} but uses {!check_ret_exn} to unwrap the result.
    @raise RcError in case of error. *)

val transact : t -> (t -> 'a) -> 'a
(** [transact db f] runs [f db] within a transaction (begin/commit/rollback).
    Useful to perform a batch of insertions or updates, as Sqlite doesn't
    write very fast. *)

val atomically : t -> (t -> 'a) -> 'a
(** Same as {!transact} but uses Sqlite's savepoint/release/rollback mechanism.
    instead of begin/commit/rolllback *)
