let verbose = ref 0

type compo_type =
  | Name of string * compo_type list
  | Abstract of string
  | Fun of compo_type list * compo_type
  | Prod of compo_type list

type func = { name : string; intypes : compo_type list; outtype : compo_type }

let ano_func f = if f.intypes = [] then f.outtype else Fun (f.intypes, f.outtype)

let deano_func name = function
  | Fun (intypes, outtype) -> { name; intypes; outtype }
  | outtype -> { name; intypes = []; outtype }

type sum_type_def = (string * compo_type option * float option) list

type sum_type = (string * string list) * sum_type_def

type special_type

type hidden_type = special_type * compo_type

type gen_function = float -> float * float

type recdefprint = (compo_type * string) list

type named_type = {
  identifier : string;
  arguments : int;
  gen_fun :
    Random.State.t ->
    int ->
    (Random.State.t -> int -> compo_type -> float -> hidden_type * int) ->
    (compo_type -> gen_function) ->
    compo_type ->
    float ->
    hidden_type * int;
  to_string :
    (compo_type -> hidden_type -> string) -> compo_type -> hidden_type -> string;
  string_of_named :
    recdefprint -> (recdefprint -> compo_type -> string) -> compo_type -> string;
  boltzman_fun : (compo_type -> gen_function) -> compo_type -> gen_function;
}

let named_type = Hashtbl.create 10

let find_type t =
  try Hashtbl.find named_type (String.trim t)
  with Not_found -> failwith ("Named type : '" ^ t ^ "' unknown")

(*print the type *)
let rec string_of_compo = function
  | Name (s, l) ->
      let ts = find_type s in
      List.fold_left
        (fun a b ->
          match b with
          | Name _ | Abstract _ -> a ^ string_of_compo b ^ " "
          | _ -> a ^ "(" ^ string_of_compo b ^ ") ")
        "" l
      ^ ts.identifier
  | Abstract s -> "'" ^ s
  | Prod [] -> ""
  | Prod (t :: q) ->
      List.fold_left
        (fun a x -> a ^ "*" ^ string_of_compo x)
        (string_of_compo t) q
  | Fun (q, out) ->
      "("
      ^ List.fold_left (fun a x -> a ^ string_of_compo x ^ "->") "" q
      ^ string_of_compo out ^ ")"

(* Run time type checking *)
let hide a (x : 'a) : hidden_type = ((Obj.magic a : special_type), x)

let reveal (a, x1) x2 =
  if x1 <> x2 then (
    Printf.eprintf "type %s attendu, recu type %s\n" (string_of_compo x2)
      (string_of_compo x1);
    assert false );
  Obj.magic a

let rec instantiate abstr concrete bt =
  let inst_list = List.map (instantiate abstr concrete) in
  match bt with
  | Abstract x when x = abstr -> concrete
  | Abstract x -> Abstract x
  | Name (s, l) -> Name (s, inst_list l)
  | Fun (args, ret) -> Fun (inst_list args, instantiate abstr concrete ret)
  | Prod l -> Prod (inst_list l)

let add_type_to_lib ?rename t =
  Hashtbl.add named_type (match rename with Some n -> n | _ -> t.identifier) t

let string_of_sum_def l =
  List.fold_left
    (fun acc (c, cto, _) ->
      (if acc <> "" then acc ^ " | " else "")
      ^ c
      ^ match cto with Some ct -> " of " ^ string_of_compo ct | None -> "")
    "" l

let string_of_sum ((n, l), def) =
  Printf.sprintf "type %s%s = %s"
    (List.fold_left (fun acc x -> acc ^ "'" ^ x ^ " ") "" l)
    n (string_of_sum_def def)

let rec print_type_list o = function
  | [] -> ()
  | t :: q -> Printf.fprintf o "%s -> %a" (string_of_compo t) print_type_list q

let print_func o f =
  Printf.fprintf o "val %s : %a%s" f.name print_type_list f.intypes
    (string_of_compo f.outtype)

let rec print_prod_aux f i = function
  | [] -> ("", "")
  | [ b ] -> (Printf.sprintf "a%i" i, Printf.sprintf "(%s a%i)" (f b) i)
  | tb :: q ->
      let def, body = print_prod_aux f (i + 1) q in
      ( Printf.sprintf "a%i,%s" i def,
        Printf.sprintf "(%s a%i)^\", \"^%s" (f tb) i body )

(*
let rec magic_print t =
  let open Obj in
  let ot = repr t in
  if is_int ot then string_of_int (magic t)
  else match tag ot with
       | x when x=string_tag -> (magic t)
       | x when x=double_tag -> string_of_float (magic t)
       | x when x=closure_tag ->
          let n = size ot in
          let os = ref ("fun<"^(string_of_int (magic (field ot 0)))) in
          for i = 1 to n-1 do
            os := !os^",";
            os := !os^(magic_print (obj (field ot i)));
          done;
          !os^">"
       | x when x=double_array_tag ->
          let n = size ot in
          let os = ref "[|" in
          for i = 0 to n-1 do
            if i>0 then os := !os^";";
            os := !os^(string_of_float (double_field ot i));
          done;
          !os^"|]"
       | x when x>=0 && x< no_scan_tag ->
         let n = size ot in
         let os = ref "(" in
         if x>0 then os:= !os^(string_of_int x)^"@";
         for i = 0 to n-1 do
           if i>0 then os := !os^",";
           os := !os^(magic_print (obj (field ot i)));
         done;
         !os^")"
       | _ -> failwith "Fail to print"
*)

(*  
let test =
  let x = [| 3.0; 5.0; 7.0 |] in
  ((Obj.magic x): float *float*float)
 *)
(*
;;
let tuple_of_list 
  magic_print [|3;3|];;            *)
