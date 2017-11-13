open Exenum

type term = Var of string | App of term * term | Lambda of string * term

(* Standard functions. *)

let rec to_string = function
  | Var s -> s
  | App (t1, t2) -> "(" ^ (to_string t1) ^ " " ^ (to_string t2) ^ ")"
  | Lambda (x, t) -> "(fun " ^ x ^ " -> " ^ (to_string t) ^ ")"

							      
(* We restrict ourselves to four variable names. *)
let e_vars = from_list ~name:"variables" ["x" ; "y" ; "u" ; "v"]

(* Type term is recursive, hence we need a lazy enumeration first. *)
let rec e_term = lazy
    begin
      (* In order to use the enumeration recursively, we need to "pay" a cost. *)
      let r_term = pay e_term in
      
      (* Now, this is the direct translation of the datatype definition. *)
      union 
	[ map e_vars (fun x -> Var x) ;
	  map (pair r_term r_term) (fun (t1, t2) -> App (t1, t2)) ;
	  map (pair e_vars r_term) (fun (x, t) -> Lambda (x, t)) ;
	] 
    end

(* Here is the enumeration for lambda-terms. *)
let e_term = Lazy.force e_term

open Exenum_internals.Convenience

let () =

  let start = bos "10000000000000000" in

  for i = 0 to 15 do

    let index = start +++ i in
    let term = get e_term index in
    Printf.printf "Term #%s is %s\n" (sob index) (to_string term) ;
  done ;

  ()
  
