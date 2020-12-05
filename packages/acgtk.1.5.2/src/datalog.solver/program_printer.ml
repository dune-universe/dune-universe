open Program
open Datalog_signature

module Program_printer =
  struct
    open Program

    let rec print_signature s =
      let rec aux = function
	| [] -> ""
	| (a,n)::q -> (Printf.sprintf "%s (arité %d)\n" n a)^(aux q) in
      match s with Datalog_signature.S(l,_,_) -> aux l

    let print_variable =
      function n ->
	if n>16 then
	  String.concat "" ["v." ; string_of_int n]
	else List.nth 
	  ["i";"j";"k";"l";"m";"n";"o";"p";"q";"r";"t";"u";"v";"w";"x";"y";"z"]
	  n

    let print_arg var= 
      let res =
	String.concat "," (List.map (print_variable) var)
      in
	"("^res^")"

    let print_pred sign=
      function Pred(k,var) ->
	let name = try Datalog_signature.get_name k sign with Failure "nth" -> Printf.sprintf "Erreur : %d" k
	in name^(print_arg var)
	  
    let print_clause sign = 
      function Cl(cl,l) ->
	let rhs = 
	  String.concat 
	    " , "
	    (List.map (print_pred sign) l)
	in
	let rhs = rhs^" ."
	in
	  (print_pred sign cl)^" :- "^rhs
	

    let print_program = 
      function Prog (sign,cl) ->
	(String.concat "\n"
	   (List.map
	      (print_clause sign)
	      cl
	   ))^"\n"
  end
