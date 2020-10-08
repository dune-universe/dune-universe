generic t = 
   Var   of [string] 
 | P     of p
 | Const of [int]
 | Add   of t * t
 | Sub   of t * t 
and p = C of [int] | V of [string] | E of t

(*
let show = GT.transform(t) (new @t[show]) () 

let _ = 
  Printf.printf "%s\n" (show (Add (P (C 1), Var "a")))
*)
