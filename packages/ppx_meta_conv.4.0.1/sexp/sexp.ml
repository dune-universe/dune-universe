type t = Sexplib.Sexp.t = Atom of string | List of t list 
(* should be compatible with sexplib *)

(* just for test *)
let format = 
  let open Format in
  let rec list (sep : (unit, formatter, unit) format)  f ppf = function
    | [] -> ()
    | [x] -> f ppf x
    | x::xs -> 
        fprintf ppf "@[%a@]%t%a" 
  	f x
  	(fun ppf -> fprintf ppf sep)
  	(list sep f) xs
  in
  let rec f ppf = function
    | Atom s -> pp_print_string ppf s
    | List ts -> fprintf ppf "(@[<2>%a@])" (list "@ " f) ts
  in
  f
