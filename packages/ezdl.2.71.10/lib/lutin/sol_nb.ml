(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License 
**-----------------------------------------------------------------------
**
** File: sol_nb.ml
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)



type sol_nb = (float * int)

(** This type is used to represent the number of solutions of a
   bdd. The idea is to approximate this number of solutions by
   [a.2**n] where [a] is null or a float inside the interval [[1, 2[], and
   [n] a positive integer. The reason for that logarithmic encoding
   is that the number of solutions of a formula is exponential in the
   number of variables; this means that number of solutions is likely
   to trigger overflow errors.

   In order to compute the [sol_nb] of a bdd using the ones of its
   sub-branches, we take advantage of the following equality:

     (1) [a.2**(n+p) + b.2**n = 2**(-k).(a+b.2**(-p)).2**(n+p+k)]

   where [k] is the smallest integer such that:
     [2**(-k).(a+b.2**(-p)) < 2].
*)

let float_of_sol_nb (a, n) = a *. (2.** (float_of_int n))
let string_of_sol_nb sol = string_of_float (float_of_sol_nb sol)

let rec (add_sol_nb: sol_nb -> sol_nb -> sol_nb) =
  fun (a, n) (b, m) -> 
    (* Adds two [sol_nb] [(a, n)] and [(b, m)] using the formula (1) above. *)
    if      a = 0. then (b, m) 
    else if b = 0. then (a, n)
    else let _ = assert ((a >= 1.) && (a < 2.) && (b >= 1.) && (b < 2.)) in
      if
	(n > m)
      then
	add_sol_nb (b, m) (a, n)
      else
	let p = m - n in
	let temp = b +. a *. (2.0**(float_of_int (-p))) in
	let k = (floor (((log temp) /. (log 2.0)) -. 1.)) +. 1. in
        let new_cst = temp *. (2.0)**(-. k) in
	let _ = assert ((1.0 <= new_cst) || new_cst = 0.) in
	let _ = assert ((new_cst < 2.0)  || new_cst = 0.) in
	  (new_cst, (m+(int_of_float k)))      


let _ = assert ((add_sol_nb  (1., 0) (1., 0)) = (1., 1))
let _ = assert ((add_sol_nb  (1., 1) (1., 1)) = (1., 2))
let _ = assert ((add_sol_nb  (1., 5) (1., 5)) = (1., 6))

(* 2^2+2^3 = 12 = 1.5 * 2^4 *)
let _ = assert ((add_sol_nb  (1., 2) (1., 3)) = (1.5, 3))
let _ = assert ((add_sol_nb  (1.5, 1) (1., 2)) = (1.75, 2))

let _ = assert ((add_sol_nb  (1., 0) (0., 1)) = (1., 0))
let _ = assert ((add_sol_nb  (1.453, 45) (0., 1)) = (1.453, 45))

let mult_sol_nb (a,n) (b,m) = 
  let t = a *. b in
    if t < 2. then (t, n+m)
    else (t /. 2. , n+m+1)

let _ = assert ((mult_sol_nb  (1.5, 1) (1.5, 3)) = (1.125, 5))


let div_sol_nb (a,n) (b,m) = 
  let t = a /. b in
    if t < 2. then (t, n-m)
    else (t /. 2. , n-m+1)



let zero_sol = (0.0, 1)
let one_sol = (1., 0)
let eq_sol_nb = (=)  
let two_power_of m = (1.0, m)


let sol_nb_of_float f = 
  let (x, n) = frexp f in
    (x *. 2.0, n-1)


let _ = assert ((float_of_sol_nb (sol_nb_of_float 123456.0)) = 123456.0)
let _ = assert ((float_of_sol_nb (sol_nb_of_float 0.0)) = 0.0)

let big = 2.0 ** 300.
let _ = assert ((float_of_sol_nb (sol_nb_of_float big)) = big)

let _very_big = 2.0 ** 3000.
(* let _ = assert ((float_of_sol_nb (sol_nb_of_float very_big)) = very_big) *)



(****************************************************************************)
(****************************************************************************)
(****************************************************************************)

(* A possible alternative, more time consuming (?) *)

(*
open Big_int

type sol_nb = big_int

let add_sol_nb =  add_big_int
let mult_sol_nb = mult_big_int
let div_sol_nb = div_big_int
let zero_sol = zero_big_int   
let one_sol = unit_big_int
let eq_sol_nb = (=)   

let two_power_of = power_int_positive_int 2
let float_of_sol_nb s = 
  let res = float_of_big_int s in if res = infinity then assert false else res


let string_of_sol_nb = string_of_big_int   

let sol_nb_of_float f = 
(*   zero_sol *)
(*   assert false *)
  if f = 0.0 then zero_sol else
  let (x, n) = frexp f in
    power_int_positive_int 2 n

*)

(****************************************************************************)
(****************************************************************************)
(****************************************************************************)


(* We use floats to count solution numbers because it can be very
  big.  Moreover, for big numbers, we do not really care the extra
  precision an abitrary-long-integer data-type would offer. 

   Well, floats overflow very quickly: 
   ~1200 variables, which can be handled easily otherwise... 

*)


(*
type sol_nb = float
let add_sol_nb =  (+.)   
let mult_sol_nb n m = n *. m   
let zero_sol = 0.   
let one_sol = 1.   
let eq_sol_nb = (=)   

let two_power_of n = 
  let res =  2. ** (float_of_int n) in
  let _ = assert (res <> infinity) in
    res

let float_of_sol_nb sol = sol   
let string_of_sol_nb = string_of_float   
*)

