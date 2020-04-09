val ping :
  ?port:int -> string -> Influxdb.Protocol.ping_response Async.Deferred.t
(** Ping the host *)

val write :
  ?precision:Influxdb.Precision.t ->
  ?port:int ->
  database:string ->
  host:string ->
  Influxdb.Point.t list ->
  unit Async.Deferred.t
(** Write a list of points to the host supplied *)
