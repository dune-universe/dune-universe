(* empty cases list *)
let f3 x = match x with

(* no parenthèses after of *)
type (-'a,+'b) blip = Blip of 'a -> 'b

type x = A of int list

#ifdef TOTO
let toto = true
#else
let toto = false
#endif

#ifversion >= 4.03
let at_least_4_03 = true
#else
let at_least_4_03 = false
#endif

#ifversion >= 4.04
let at_least_4_04 = true
#else
let at_least_4_04 = false
#endif
