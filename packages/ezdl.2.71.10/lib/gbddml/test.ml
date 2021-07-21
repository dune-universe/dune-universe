

open Bdd
open Printf

(*let _ = Bdd.init ()
*)

let x = Bdd.idy 1
let y = Bdd.idy 2

let p = Bdd.eq (Bdd.idy 1) (Bdd.nidy 1)


let _ = if(p = Bdd.dtrue ()) then (
	printf "ko\n"
) else (
	printf "ok\n"
)
let _ = flush stdout

let z = Bdd.dand x y

let _ = Bdd.print_mons z

let sl = Bdd.list_of_support z

let _ = List.iter (printf "%d; ") sl

let _ = printf "\n"
