open Decap

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

let parser expr prio : float grammar =
  | f:float_num          when prio = Atom -> f
  | '(' e:(expr Sum) ')' when prio = Atom -> e
  | '-' e:(expr Pow)     when prio = Pow -> -. e
  | '+' e:(expr Pow)     when prio = Pow -> e
  | e:(expr Atom) e':{"**" (expr Pow)}? when prio = Pow ->
      match e' with None -> e | Some e' -> e ** e'
  | e:(expr Pow) l:{prod_sym (expr Pow)}*
                         when prio = Prod ->
      List.fold_left (fun acc (fn, e') -> fn acc e') e l
  | e:(expr Prod) l:{sum_sym  (expr Prod)}*
                         when prio = Sum  ->
      List.fold_left (fun acc (fn, e') -> fn acc e') e l

let rec generate_expr n =
  if n <= 0 then let n = Random.float 10.0 in
		let s = string_of_float n in
		s
  else match Random.int 8 with
	 0 -> let e' = generate_expr(n-1) in
	      "(" ^ e' ^ ")"
       | 1 -> let e' = generate_expr(n-1) in
	      "+" ^ e'
       | 2 -> let e' = generate_expr(n-1) in
	      "-" ^ e'
       | 3 -> let k = Random.int n in
	      let e' = generate_expr(k) in
	      let e'' = generate_expr(n-1-k) in
	      e' ^ "**" ^ e''
       | 4 -> let k = Random.int n in
	      let e' = generate_expr(k) in
	      let e'' = generate_expr(n-1-k) in
	      e' ^ "+" ^ e''
       | 5 -> let k = Random.int n in
	      let e' = generate_expr(k) in
	      let e'' = generate_expr(n-1-k) in
	      e' ^ "-" ^ e''
       | 6 -> let k = Random.int n in
	      let e' = generate_expr(k) in
	      let e'' = generate_expr(n-1-k) in
	      e' ^ "*" ^ e''
       | 7 -> let k = Random.int n in
	      let e' = generate_expr(k) in
	      let e'' = generate_expr(n-1-k) in
	      e' ^ "/" ^ e''
       | _ -> assert false
				       
(* The main loop *)
let _ =
  let blank = blank_regexp ''[ \t\r\n]*'' in
  try let n1 = int_of_string Sys.argv.(1) and n2 = int_of_string Sys.argv.(2) in
      for i = 1 to n1 do
	let n = Random.int n2 in
	let l = generate_expr n in
	Printf.printf "Parsing: %s\n%!" l;
	let r = handle_exception (parse_string (expr Sum) blank) l in
	Printf.printf "-> %f\n%!" r
      done
  with _ -> try while true do
    Printf.printf ">> %!";
    let l = input_line stdin in
    let r = handle_exception (parse_string (expr Sum) blank) l in
    Printf.printf "%f\n%!" r
  done with End_of_file -> ()
