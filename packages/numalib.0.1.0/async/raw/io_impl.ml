open Async

type 'a t = 'a Deferred.t

let return = Deferred.return
let bind = Deferred.bind
let map  = Deferred.map
let (>>=) = Deferred.(>>=)
let (>>|) = Deferred.(>>|)
let wrap f v = In_thread.run (fun () -> f v)
