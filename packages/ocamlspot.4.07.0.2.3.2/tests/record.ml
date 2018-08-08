type t = { (* t => *) foo (* <= t *) : int ; bar : float } 
    
let x = { foo (* ? t *) = 1; bar = 4.2 }
let _ = x.foo (* ? t *)
