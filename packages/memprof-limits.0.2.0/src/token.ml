type t = bool Atomic.t
let create () = Atomic.make false
let set x = Atomic.set x true
let is_set x = Atomic.get x
