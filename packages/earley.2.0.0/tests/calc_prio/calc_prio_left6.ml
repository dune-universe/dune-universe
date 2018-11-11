open Generate_calc
open Earley_core.Earley

type calc_prio = Sum | Pro | Pow | Atm

let prio_to_string = function
  | Sum  -> "Sum"
  | Pro -> "Prod"
  | Pow -> "Pow"
  | Atm -> "Atom"

let float_re = ''[0-9]+\([.][0-9]+\)?\([eE][-+]?[0-9]+\)?''
let float_num = parser
  f:RE(float_re) -> float_of_string f

let pro_sym = parser
  | '*' -> ( *. )
  | '/' -> ( /. )

let sum_sym = parser
  | '+' -> ( +. )
  | '-' -> ( -. )

let parser expr @p =
  | float_num
  | '(' e:(expr Sum) ')'
  | '-' e:(expr Pow)                      when p <= Pow -> -. e
  | '+' e:(expr Pow)                      when p <= Pow -> e
  | e:(expr Atm) "**" e':(expr Pow)       when p <= Pow -> e ** e'
  | e:(expr Pro) fn:pro_sym e':(expr Pow) when p <= Pro -> fn e e'
  | e:(expr Sum) fn:sum_sym e':(expr Pro) when p <= Sum -> fn e e'


(* The main loop *)
let _ = run (expr Sum)
