
open Decap

type calc_prio = Sum | Prod | Pow | Atom

let float_re = ''[0-9]+\([.][0-9]+\)?\([eE][-+]?[0-9]+\)?''
let float_num = parser
  f:RE(float_re) -> float_of_string f

let prod_sym = parser
  | '*' -> ( *. )
  | '/' -> ( /. )

let sum_sym = parser
  | '+' -> ( +. )
  | '-' -> ( -. )

let parser test =
  EMPTY | test 'a'
	       
let parser expr =
  | DEBUG"1" f:float_num DEBUG"1b"         -> (Atom,f)
  | DEBUG"2" '(' (_,e):expr ')'  -> Atom,e
  | DEBUG"3" '-' DEBUG"3b" (p,e):expr -> if p < Pow then raise (Give_up ""); Pow, -. e
  | DEBUG"4" '+' DEBUG"4b" (p,e):expr -> if p < Pow then raise (Give_up ""); Pow, e
  | DEBUG"5" (p,e):expr DEBUG"5b" "**" DEBUG"5c" (p',e'):expr DEBUG"5d" ->
		if p <= Pow || p' < Pow then raise (Give_up ""); Pow, e ** e' 
  | DEBUG"6" (p,e):expr DEBUG"6b" fn:prod_sym DEBUG"6c" (p',e'):expr DEBUG"6d" ->
		if p < Prod || p' <= Prod then raise (Give_up ""); Prod, fn e e'
  | DEBUG"7" (p,e):expr DEBUG"7b" fn:sum_sym DEBUG"7c" (p',e'):expr DEBUG"7d" ->
		if p < Sum || p' <= Sum then raise (Give_up ""); Sum, fn e e'
(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
  try while true do
    Printf.printf ">> %!";
    let l = input_line stdin in
    let (_,r) = handle_exception (parse_string expr blank) l in
    Printf.printf "%f\n%!" r
  done with End_of_file -> ()
