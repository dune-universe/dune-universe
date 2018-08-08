exception (* Error => *) Error (* <= Error *) of int

let _ = raise (Error (* ? Error *) 1)

let _ = 1 + "hello"


