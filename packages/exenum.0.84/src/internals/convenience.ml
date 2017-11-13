
(*******************  Convenience functions  **********************)
let myfold l a f = List.fold_left f a l
let myrevmap l f   = List.rev_map f l
let myrevmap2 l1 l2 f  = List.rev_map2 f l1 l2

(* Builds a string from a list of items *)
let sep map sp l = List.fold_left (fun acu x -> if acu = "" then map x else acu ^ sp ^ (map x)) "" l



(*******************  Shortcuts to Zarith  ***********************)

let (++) = Z.add
let (+++) b i = b ++ (Z.of_int i)
let succ = Z.succ

let (--) = Z.sub
let minus = Z.neg

let ( *^ ) i j = let open Z in (Z.of_int i) ** j

let ( ** ) = Z.mul
let ( **. ) i b = (Z.of_int i) ** b

let quomod = Z.div_rem
let bigmod a b = Z.rem a b

let sign = Z.sign
let big_compare = Z.compare

let bigzero = Z.zero
let bigone  = Z.one

let is_bigone x = Z.compare x bigone = 0

let sob = Z.to_string
let bos = Z.of_string

let boi = Z.of_int
let iob = Z.to_int

