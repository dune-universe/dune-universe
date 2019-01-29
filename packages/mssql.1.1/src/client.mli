open Async

type t

(** Opens a connection, runs the callback, then closes the connection. Don't
    try to use the DB handle outside of the callback, since it will have been
    closed automatically. *)
val with_conn
  : host:string
  -> db:string
  -> user:string
  -> password:string
  -> port:string
  -> (t -> 'a Deferred.t)
  -> 'a Deferred.t

(** Executes an SQL query, handling parameter quoting for you. Do not use
    [sprintf] to generate queries; instead pass parameters like this:

    {|
    let params = [ Some (Mssql.Param.String "look no ' sql injection\x00!") ] in
    Mssql.execute ~params "SELECT $1"
    |}

    Returns a single result set and throws an exception if the result has more
    than one result set.
*)
val execute : ?params:Db_field.t option list -> t -> string -> Row.t list Deferred.t

(** Like [execute] but asserts that the result set will be empty. *)
val execute_unit : ?params:Db_field.t option list -> t -> string -> unit Deferred.t

(** Like [execute] but asserts that the result set will return a
    maximum of 1 row. *)
val execute_single
  : ?params:Db_field.t option list
  -> t
  -> string
  -> Row.t option Deferred.t

(** Like [execute] but returns all result sets. Useful for running multiple
    queries at once. *)
val execute_multi_result : ?params:Db_field.t option list -> t -> string -> Row.t list list Deferred.t

(** List [execute] except runs multiple identical queries at the same time,
    with different parameters. Takes a list of param lists and returns a list
    of results sets, as if you called
    [List.map params ~f:(fun params -> execute ~params db query)] *)
val execute_many
  : params:Db_field.t option list list
  -> t
  -> string
  -> Row.t list list Deferred.t

(** Begin a transaction *)
val begin_transaction : t -> unit Deferred.t

(** Commit the current transaction *)
val commit : t -> unit Deferred.t

(** Rollback the current transaction *)
val rollback : t -> unit Deferred.t

(** [with_transaction t f] runs [f] in a transaction. If the function returns
    normally, the transaction will be commited. If the function throws an
    exception, the transaction will be rolled back and the exception will be
    re-raised.

    Note regarding the [t] argument to the callback: There is logic to ensure
    that queries outside of the transaction are blocked until the transaction
    finishes, so you *must* use the [t] passed in the callback, not the original
    handle inside of the transaction. To prevent deadlocks, an exception will
    be thrown if you use the external DB handle inside of with_transaction. *)
val with_transaction : t -> (t -> 'a Deferred.t) -> 'a Deferred.t

(** Like [with_transaction] but also rolls back the transaction on [Error] *)
val with_transaction_or_error : t -> (t -> 'a Deferred.Or_error.t) -> 'a Deferred.Or_error.t

module Pool : sig
  type p

  val with_pool
    : host:string
    -> db:string
    -> user:string
    -> password:string
    -> port:string
    -> ?max_connections:int
    -> (p -> 'a Deferred.t)
    -> 'a Deferred.t

  val with_conn : p -> (t -> 'a Deferred.t) -> 'a Deferred.t
end
