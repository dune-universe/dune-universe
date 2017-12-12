type 'a t = 'a

let return v = v
let bind a ~f = f a
let map a ~f  = f a
let (>>=) a f =  f a
let (>>|) a f = f a
let wrap f = f
