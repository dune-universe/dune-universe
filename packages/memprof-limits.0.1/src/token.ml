type t = { mutable set : bool ; mutable armed : bool }
let make () = { set = false ; armed = true }
let set x = x.set <- true
let is_set x = x.set
let release x = x.armed <- false
let armed x = x.armed
