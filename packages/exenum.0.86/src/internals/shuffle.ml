open Convenience

(*** Shuffle, deterministically, the values inside parts. ***)

(* Starting from x, find a number prime with y. *)
let rec find_prime x y =
  let gcd = Z.gcd x y in
  if is_bigone gcd then x
  else find_prime (Z.succ x) y

let compute_shuffle card compute =

  (* Z doc says bigints can be compared using >  *)
  if card = Z.zero then
    (* The part is empty. *)
    (fun _ -> assert false) (* Should not be invoked. *)
      
  else
  
  (* Find a number within the order of magnitude of sqrt(card) 
   * and that is prime with card. *)
  let prime = find_prime (Z.sqrt card) card in

  fun index -> 
    let new_index = bigmod (index ** prime) card in
    compute new_index


