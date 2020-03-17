(** Gets the first value of a lambda stream and turns it into a deferred value *)
val first_to_async : 'a Lambda_streams.Finite.Async.t -> 'a Async_kernel.Deferred.t

(** Gets the last value of a lambda stream and turns it into a deferred value *)
val last_to_async : 'a Lambda_streams.Finite.Async.t -> 'a Async_kernel.Deferred.t

val to_async_list : 'a Lambda_streams.Finite.Async.t -> 'a list Async_kernel.Deferred.t

val from_async : 'a Async_kernel.Deferred.t -> 'a Lambda_streams.Finite.Async.t

val to_async_stream : 'a Lambda_streams.Finite.Async.t -> 'a Async_kernel.Stream.t

val from_async_stream : 'a Async_kernel.Stream.t -> 'a Lambda_streams.Finite.Async.t

val to_reader : 'a Lambda_streams.Finite.Async.t -> 'a Async_kernel.Pipe.Reader.t

val from_reader : 'a Async_kernel.Pipe.Reader.t -> 'a Lambda_streams.Finite.Async.t
