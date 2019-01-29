(** Exposes functions to help with tests that use Mssql *)

(** Mssql db connection with test credentials *)
val with_conn : (Client.t -> 'a Async.Deferred.t) -> 'a Async.Deferred.t

(** Mssql db connection pool with test credentials *)
val with_pool
  : ?max_connections:int
  -> (Client.Pool.p -> 'a Async.Deferred.t)
  -> 'a Async.Deferred.t
