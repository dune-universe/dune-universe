open Earley_core
open Earley
open Common

let rec generate_expr n =
  if n <= 0 then string_of_float (Random.float 10.0) else
  match Random.int 23 with
  | 1 ->
      let e' = generate_expr (n-1) in
	    "+" ^ e'
  | 2 ->
      let e' = generate_expr (n-1) in
	    "-" ^ e'
  | 3 | 4 | 5 ->
      let k = Random.int n in
	    let e' = generate_expr k  in
	    let e'' = generate_expr (n-1-k) in
	    e' ^ "**" ^ e''
  | 6 | 7 | 8 ->
      let k = Random.int n in
	    let e' = generate_expr k  in
	    let e'' = generate_expr (n-1-k) in
	    e' ^ "+" ^ e''
  | 9 | 10 | 11 ->
      let k = Random.int n in
	    let e' = generate_expr k  in
	    let e'' = generate_expr (n-1-k) in
	    e' ^ "-" ^ e''
  | 12 | 13 | 14 ->
      let k = Random.int n in
	    let e' = generate_expr k in
	    let e'' = generate_expr (n-1-k) in
	    e' ^ "*" ^ e''
  | 15 | 16 | 17 ->
      let k = Random.int n in
	    let e' = generate_expr k in
	    let e'' = generate_expr (n-1-k) in
	    e' ^ "/" ^ e''
  | _ ->
      let e' = generate_expr (n-1) in
	    "(" ^ e' ^ ")"

(* The main loop *)
let run parse =
  let blank = blank_regexp "[ \\t\\r\\n]*" in
  if !kind <> None then
    begin

      let n1, n2 = test_cases ((100,20),(300,50),(1000,1000)) in
      for i = 1 to n1 do
	let n = Random.int n2 in
	let l = generate_expr n in
        if !output then
          Printf.printf "%s\n" l
        else begin
	    Printf.eprintf "\r%d/%d%!" i n1;
	    Printf.printf "Parsing: %s\n%!" l;
	    let r = handle_exception (parse_string parse blank) l in
	    Printf.printf "-> %f\n%!" r
          end
      done;
      Printf.eprintf " OK\n%!";
    end
  else
    try
      while true do
	Printf.printf ">> %!";
	let l = input_line stdin in
	let r = handle_exception (parse_string parse blank) l in
	Printf.printf "%f\n%!" r
      done
    with End_of_file -> ()
