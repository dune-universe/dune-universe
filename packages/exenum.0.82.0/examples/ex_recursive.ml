(* Enumerating over a recursive type. *)

type term = Var of string | App of term * term | Lambda of string * term

let rec to_string = function
  | Var s -> s
  | App (t1, t2) -> "(" ^ (to_string t1) ^ " " ^ (to_string t2) ^ ")"
  | Lambda (x, t) -> "(fun " ^ x ^ " -> " ^ (to_string t) ^ ")"

open Exenum
let e_vars = from_list ~name:"variables" ["x" ; "y" ; "u" ; "v"]

let rec e_term =
  lazy begin
    (* Explicit recursion *)
    let r_term = pay e_term in
    union 
      [ map e_vars (fun v -> Var v) ;
	map (pair r_term r_term) (fun (t1,t2) -> App (t1, t2)) ;
	map (pair e_vars r_term) (fun (x,t) -> Lambda (x,t)) ]
  end

let e_term = Lazy.force e_term

(* Let us print the first terms of the enumeration. *)
let () = show e_term to_string 0 16

let index = Big_int.power_int_positive_int 10 400 (* This is 10^400. *)
let () = bigshow e_term to_string index 2
