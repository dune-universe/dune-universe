(** Ping the host *)
val ping : ?port:int -> string -> Influxdb.Protocol.ping_response Async.Deferred.t

(** Write a list of points to the host supplied *)
val write : ?precision:Influxdb.Precision.t -> ?port:int -> database:string -> points:Influxdb.Point.t list -> string -> unit Async.Deferred.t