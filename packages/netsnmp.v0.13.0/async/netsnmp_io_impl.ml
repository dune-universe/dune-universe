open Async

type 'a t = 'a Deferred.t

let return = Deferred.return
let bind = Deferred.bind
let map  = Deferred.map
let (>>=) = Deferred.(>>=)
let (>>|) = Deferred.(>>|)

let sequencer = Throttle.Sequencer.create ~continue_on_error:true ()
let wrap f v = Throttle.enqueue sequencer (fun () -> In_thread.run (fun () -> f v))

let wrap_mt f v = In_thread.run (fun () -> f v)

let gc_finalise f t = Gc.add_finalizer_exn t (fun t -> f t |> don't_wait_for)
