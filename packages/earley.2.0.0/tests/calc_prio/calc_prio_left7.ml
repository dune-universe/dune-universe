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

let parser expr op cl @p =
  | float_num
  | CHR(op) e:(expr op cl Sum) CHR(cl)
  | '-' e:(expr op cl Pow)                            when p <= Pow -> -. e
  | '+' e:(expr op cl Pow)                            when p <= Pow -> e
  | e:(expr op cl Atm) "**" e':(expr op cl Pow)       when p <= Pow -> e ** e'
  | e:(expr op cl Pro) fn:pro_sym e':(expr op cl Pow) when p <= Pro -> fn e e'
  | e:(expr op cl Sum) fn:sum_sym e':(expr op cl Pro) when p <= Sum -> fn e e'


(* The main loop *)
let _ = run (expr '(' ')' Sum)
