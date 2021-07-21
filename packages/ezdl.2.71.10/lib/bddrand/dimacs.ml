(* Time-stamp: <modified the 02/09/2019 (at 08:56) by Erwan Jahier> *)


(********************************************************************************** 
The dimacs parser comes from:
http://www.enseignement.polytechnique.fr/informatique/INF551/TD/TD1/aux/DIMACS.ml *)

open String

let read_from_file filename =
  let lines = ref "" in
  let chan = open_in filename in
    try
      while true; do
	let line = input_line chan in
	  if (length line>0)&&(not ((line.[0]) = 'c')) then 
	    lines := (!lines)^"\n"^line
      done; ""
    with End_of_file ->
      close_in chan;
      !lines

let latexescaped = function
  | '%' | '{' | '}' as c -> "\\"^Char.escaped c
  | c -> Char.escaped c

let rec list_from_string s list_so_far n = 
  if (n>=length s) then List.rev list_so_far 
  else
    match s.[n] with 
      | ' ' | '\n' | '\t' -> list_from_string s list_so_far (n+1)
      | '-'  -> list_from_string s ("-"::list_so_far) (n+1)
      |  _   ->
          let rec word_from_string s word_so_far n =
	    if (n>=length s) then List.rev (word_so_far::list_so_far) 
	    else
	      begin
	        match s.[n] with
		| ' '| '\n' | '\t' -> list_from_string s (word_so_far::list_so_far) n
		| c    -> word_from_string s (word_so_far^(latexescaped c)) (n+1)
	      end
	  in
	  word_from_string s "" n

let rec _print_list = function
  | []   -> ()
  | s::l -> print_string (s^" "); _print_list l

module PairLit = struct
  type t = bool*int
  let _negation (b,s) = (not b,s)
end

let rec parse_cnf cnf_so_far: string list -> PairLit.t list list  = function
  | []     -> List.rev cnf_so_far
  | "0"::l -> parse_cnf ((* []:: *)cnf_so_far) l
  | l -> let rec parse_clause clause_so_far ispos = function
           | []     -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) []
           | "0"::l -> parse_cnf ((List.rev clause_so_far)::cnf_so_far) l
           | "-"::l -> parse_clause clause_so_far false l
           | s::l   -> parse_clause ((ispos,int_of_string s)::clause_so_far) true l
         in parse_clause [] true l

let parse_cnf_file l = 
  let aux _nbvar = function
    | "p"::"cnf"::nbvar::_nbclauses::l -> parse_cnf [] l,  int_of_string nbvar
    | _ -> assert false
  in 
  aux 0 l
 
type t = (bool * int) list list * int
         
let (parse: string -> t) = fun x -> try
  list_from_string (read_from_file x) [] 0
  |> parse_cnf_file
  with _e -> failwith (Printf.sprintf "Is file %s a valid dimacs file?" x)

(**********************************************************************************)
(* a printer *)
let (print: t -> string) = fun (pll,_) -> 
  let p_to_str (b,n) = if b then string_of_int n else "not("^string_of_int n^")" in
  let pl_to_str pl =
    List.fold_left
      (fun acc p -> Printf.sprintf "%s or %s" acc (p_to_str p))
      (p_to_str (List.hd pl))
      (List.tl pl)
  in
  List.fold_left
    (fun acc pl -> Printf.sprintf "%s and \n%s" acc (pl_to_str pl))
    (pl_to_str (List.hd pll))
    (List.tl pll)
        
(**********************************************************************************)
(* Translate dimacs data type into Exp.formula *)

let make_var i = Var.make "" ((string_of_int i)) Type.BoolT Var.Output

(* Returns the and of all the variables using  the dimacs var number *)
let (gen_vars : int -> Exp.var list) = fun size ->
  let rec aux acc i =
    if i > size then acc else aux ((make_var i)::acc) (i+1)
  in
  aux [] 1

let (to_formula :  t -> Exp.var list * Exp.formula) = fun (pll, size) ->
  let p_to_bdd (b,n) =
    let v = Exp.Bvar (make_var n) in
    if b then v else Exp.Not(v)
  in
  let pl_to_bdd pl =
    List.fold_left
      (fun acc p -> Exp.Or (acc, (p_to_bdd p)))
      (p_to_bdd (List.hd pl))
      (List.tl pl)
  in
  let f =
  List.fold_left
    (fun acc pl -> Exp.And (acc, (pl_to_bdd pl)))
    (pl_to_bdd (List.hd pll))
    (List.tl pll)
  in
  let vl = gen_vars size in
  vl, f
