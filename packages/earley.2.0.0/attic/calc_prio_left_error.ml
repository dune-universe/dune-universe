open Earley
open Generate_calc

(*let _ = active_debug := false*)

type calc_prio = Sum | Prod | Pow | Atom

let float_re = ''[0-9]+\([.][0-9]+\)?\([eE][-+]?[0-9]+\)?''
let float_num = parser
  f:RE(float_re) -> float_of_string f

let pow_sym = parser
  | "**" -> ( *. )
  | ERROR("'**' expected") -> assert false

let prod_sym = parser
  | '*' -> ( *. )
  | '/' -> ( /. )
  | ERROR("'*' or '/' expected")-> assert false

let sum_sym = parser
  | '+' -> ( +. )
  | '-' -> ( -. )
  | ERROR("'-' or '+' expected")-> assert false

let parser test =
  EMPTY | test 'a'

let parser expr prio =
  | f:float_num                                               -> f
  | p:'(' e:(expr Sum) { ')' | ERROR("parenthesis unclosed") }-> e
  | '-' e:(expr Pow)                        when prio <= Pow  -> -. e
  | '+' e:(expr Pow)                        when prio <= Pow  -> e
  | e:(expr Atom) pow_sym e':(expr Pow)     when prio <= Pow  -> e ** e'
  | e:(expr Prod) fn:prod_sym e':(expr Pow) when prio <= Prod -> fn e e'
  | e:(expr Sum) fn:sum_sym e':(expr Prod)  when prio <= Sum  -> fn e e'
  | ERROR("constante, '-' or '+' expected") when prio <= Pow  -> assert false
  | ERROR("constante expected")             when prio > Pow   -> assert false

(* The main loop *)
let _ = run (expr Sum)
