
(*******************  Convenience functions  **********************)
let myfold l a f = List.fold_left f a l
let myrevmap l f   = List.rev_map f l
let myrevmap2 l1 l2 f  = List.rev_map2 f l1 l2

(* Builds a string from a list of items *)
let sep map sp l = List.fold_left (fun acu x -> if acu = "" then map x else acu ^ sp ^ (map x)) "" l



(*******************  Shortcuts to BIG INT  ***********************)
open Big_int

let (++) = add_big_int
let (+++) = add_int_big_int
let succ = succ_big_int

let (--) = sub_big_int
let minus = minus_big_int

let ( ** ) = mult_big_int
let ( **. ) = mult_int_big_int
let quomod = quomod_big_int
let bigmod = mod_big_int

let ( *^ ) = power_int_positive_int 

let sign = sign_big_int
let (<==) = le_big_int
let big_compare = compare_big_int

let bigzero = zero_big_int
let bigone  = unit_big_int

let is_bigone x = big_compare x bigone = 0

let sob = string_of_big_int
let bos = big_int_of_string

let boi = big_int_of_int
let iob = int_of_big_int

