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

type poly = ((compo_type list * int) * float) list

type poly_assoc = (compo_type * poly) list

type named_type = {
  identifier : string;
  boltz_identifier : string;
  is_simple : bool;
  arguments : int;
  get_equ :
    (compo_type -> poly_assoc -> poly_assoc) ->
    poly_assoc ->
    compo_type ->
    poly_assoc;
  gen_fun :
    Random.State.t ->
    int ->
    (Random.State.t -> int -> compo_type -> float -> hidden_type * int) ->
    (compo_type -> gen_function) ->
    compo_type ->
    float ->
    hidden_type * int;
  print :
    (compo_type -> Format.formatter -> hidden_type -> unit) ->
    compo_type ->
    Format.formatter ->
    hidden_type ->
    unit;
  string_of_named :
    recdefprint -> (recdefprint -> compo_type -> string) -> compo_type -> string;
  boltzman_fun : (compo_type -> gen_function) -> compo_type -> gen_function;
}

let flatten_prod l =
  let rec aux acc = function
    | Prod l2 -> List.fold_left aux acc l2
    | x -> x :: acc
  in
  List.fold_left aux [] l

let named_type = Hashtbl.create 10

let find_type t =
  try Hashtbl.find named_type (String.trim t)
  with Not_found -> failwith ("Named type : '" ^ t ^ "' unknown")

(*let rec string_of_compo ?(use_boltz_id = false) = function
  | Name (s, l) -> "Name(" ^ s ^ "," ^ List.fold_left (fun acc t -> acc^","^string_of_compo l ^ ")"
  | Prod (t :: q) ->
      List.fold_left
        (fun a x -> a ^ "*" ^ string_of_compo x)
        (string_of_compo t) q
  | Prod [] -> ""
  | _ -> failwith "test"*)

let rec pp_compo ?(use_boltz_id = false) f t =
  let ppt = pp_compo ~use_boltz_id in
  let open Format in
  match t with
  | Name (s, l) ->
      let ts = find_type s in
      fprintf f "%a%s"
        (fun _ _ ->
          List.iter
            (function
              | (Name _ | Abstract _) as b -> fprintf f "%a " ppt b
              | x -> fprintf f "(%a) " ppt x)
            l)
        ()
        (if use_boltz_id then ts.boltz_identifier else ts.identifier)
  | Abstract s -> fprintf f "'%s" s
  | Prod l ->
      fprintf f "%a"
        (pp_print_list ~pp_sep:(fun _ _ -> pp_print_string f "*") ppt)
        l
  | Fun (q, out) ->
      fprintf f "(%a%a)"
        (fun _ _ -> List.iter (fun a -> fprintf f "%a->" ppt a) q)
        () ppt out

(*
(*print the type *)
let rec string_of_compo ?(use_boltz_id = false) = function
  | Name (s, l) ->
      let ts = find_type s in
      List.fold_left
        (fun a b ->
          match b with
          | Name _ | Abstract _ -> a ^ string_of_compo ~use_boltz_id b ^ " "
          | _ -> a ^ "(" ^ string_of_compo ~use_boltz_id b ^ ") ")
        "" l
      ^ if use_boltz_id then ts.boltz_identifier else ts.identifier
  | Abstract s -> "'" ^ s
  | Prod [] -> ""
  | Prod (t :: q) ->
      List.fold_left
        (fun a x -> a ^ "*" ^ string_of_compo ~use_boltz_id x)
        (string_of_compo ~use_boltz_id t)
        q
  | Fun (q, out) ->
      "("
      ^ List.fold_left
          (fun a x -> a ^ string_of_compo ~use_boltz_id x ^ "->")
          "" q
      ^ string_of_compo ~use_boltz_id out
      ^ ")"
*)

(* Run time type checking *)
let hide a (x : 'a) : hidden_type = ((Obj.magic a : special_type), x)

let reveal (a, x1) x2 =
  if x1 <> x2 then (
    Format.eprintf "type %a attendu, recu type %a@."
      (fun f -> pp_compo f)
      x2
      (fun f -> pp_compo f)
      x1;
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
  let boltz_id = match rename with Some n -> n | _ -> t.identifier in
  Hashtbl.add named_type boltz_id { t with boltz_identifier = boltz_id }

let rewrite = function
  | Name (n, l) ->
      let nt = find_type n in
      Name (nt.boltz_identifier, l)
  | x -> x

let string_of_sum_def f l =
  Format.pp_print_list
    ~pp_sep:(fun f _ -> Format.pp_print_string f " | ")
    (fun _ (c, cto, _) ->
      Format.pp_print_string f c;
      Option.iter
        (fun ct -> Format.fprintf f " of %a" (fun _ _ -> pp_compo f ct) ())
        cto)
    f l

let pp_sum f ((n, l), def) =
  Format.fprintf f "type %a%s = %a"
    (Format.pp_print_list (fun _ x -> Format.fprintf f "'%s " x))
    l n string_of_sum_def def

(*let rec print_type_list o = function
  | [] -> ()
  | t :: q ->
      Format.fprintf o "%a -> %a" (fun f -> pp_compo f) t print_type_list q

let print_func o f =
  Format.fprintf o "val %s : %a%a" f.name print_type_list f.intypes
    (fun f -> pp_compo f)
    f.outtype
*)

let pp_func ?(use_boltz_id = false) f func =
  Format.fprintf f "val %s : %a%a" func.name
    (Format.pp_print_list
       ~pp_sep:(fun _ _ -> ())
       (fun _ t ->
         pp_compo f t;
         Format.fprintf f " -> "))
    func.intypes (pp_compo ~use_boltz_id) func.outtype

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
