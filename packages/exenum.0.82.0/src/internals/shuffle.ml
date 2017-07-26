open Convenience
open Big_int

(*** Shuffle, deterministically, the values inside parts. ***)

(* Starting from x, find a number prime with y. *)
let rec find_prime x y =
  let gcd = gcd_big_int x y in
  if is_bigone gcd then x
  else find_prime (succ x) y

let compute_shuffle card compute =
  
  (* Find a number within the order of magnitude of sqrt(card) 
   * and that is prime with card. *)
  let prime = find_prime (sqrt_big_int card) card in

  fun index -> 
    let new_index = bigmod (index ** prime) card in
    compute new_index


