type (* record => *) record (* <= record *)= { mutable (* x => *) x (* <= x *) : int } 

let (* r => *) r (* <= r *) = { x = 0; } (* ? record *)

let _ = r(* ? r *).x (* ? x *) <- 1 

 
