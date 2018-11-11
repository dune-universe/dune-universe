open Earley

type calc_prio = Sum | Prod | Pow | Atom

let float_re = ''[0-9]+\([.][0-9]+\)?\([eE][-+]?[0-9]+\)?''
let parser float_num =
  f:RE(float_re) -> float_of_string f

let parser prod_sym =
  | '*' -> ( *. )
  | '/' -> ( /. )

let parser sum_sym =
  | '+' -> ( +. )
  | '-' -> ( -. )

let parser expr prio =
  | f:float_num          when prio = Atom -> f
  | '(' e:(expr Sum) ')' when prio = Atom -> e
  | '-' e:(expr Pow)     when prio = Pow -> -. e
  | '+' e:(expr Pow)     when prio = Pow -> e
  | e:(expr Atom) e':{"**" (expr Pow)}? when prio = Pow ->
      (match e' with None -> e | Some e' -> e ** e')
  | e:(expr Pow) l:{prod_sym (expr Pow)}*
                         when prio = Prod ->
      List.fold_left (fun acc (fn, e') -> fn acc e') e l
  | e:(expr Prod) l:{sum_sym  (expr Prod)}*
                         when prio = Sum  ->
      List.fold_left (fun acc (fn, e') -> fn acc e') e l

(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
  try while true do
    Printf.printf ">> %!";
    let l = input_line stdin in
    let r = handle_exception (parse_string (expr Sum) blank) l in
    Printf.printf "%f\n%!" r
  done with End_of_file -> ()
