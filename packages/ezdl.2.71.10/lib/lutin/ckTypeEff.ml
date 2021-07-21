(*----------------------------------------------------------
                 TYPE/BINDING CHECK 
------------------------------------------------------------

Type "sémantique" des idents et des exps
n.b. pour l'instant, on n'a que des types de base,
mais ga pourrait changer ...

Pour les profils, on gère le polymorphisme
avec des pseudos-types "any"

Pour les types any, on peut compléter par une
   condition qui restreint le match typiquement 
   - aux types data (autres que trace) 
   - aux types numériques seulement
----------------------------------------------------------*)
open LutErrors
open Format
open Syntaxe

let dbg = Verbose.get_flag "CkType"

type basic = Syntaxe.predef_type

(** le type "weight" est purement interne *)
type t =
   TEFF_weight
|  TEFF_except
|  TEFF_trace
|  TEFF_data of basic
|  TEFF_list of basic
|  TEFF_tuple of basic list
|	TEFF_any of string * any_cond
|	TEFF_ref of basic
and  any_cond = (t -> t option) 

let lift_ref = function
	TEFF_ref a -> TEFF_data a
|	_ -> (
	raise (Failure "not a ref")
)

let is_data = function
	TEFF_data _ -> true
|	_ ->  false

let get_data_tuple tl = (
	let undata = function
		TEFF_data d -> d
	| _ -> raise (Failure "not a data")
	in
	TEFF_tuple (List.map undata tl)
)

let tuple_to_data_list t = (
	let redata = function
		d -> TEFF_data d
	in
	match t with
	TEFF_tuple bl -> (List.map redata bl)
	| _ -> raise (Failure "not a tuple")
)

let is_ref = function
	TEFF_ref _ -> true
|	_ ->  false

let basic_to_string = (
function
 	Bool -> "bool"
|	Int -> "int"
|	Real -> "real"
)
(* pretty-print des types *)
let rec to_string = ( function
 	 | TEFF_data d -> (basic_to_string d)
    |	TEFF_list d -> (basic_to_string d)^ " list"
    |	TEFF_tuple l -> String.concat "*" (List.map basic_to_string l)
    |	TEFF_ref x -> (basic_to_string x)^" ref"
    |	TEFF_trace -> "trace"
    |	TEFF_weight -> "weight"
    |	TEFF_except -> "exception"
    |	TEFF_any (s, _) -> s
  ) and prof_to_string = ( function
	   (tl, t) -> (
		  sprintf "%s->%s"
			 (list_to_string tl)
			 (list_to_string t)
	   )
  ) and list_to_string = ( function
	   [] -> ""
    |	t::[] -> to_string t
    |	t::l -> sprintf "%s*%s" (to_string t) (list_to_string l)
  )

let ref_of = function
	TEFF_data x -> TEFF_ref x 
	| z -> (
		raise (Internal_error ("CkTypeEff:ref_of",
			"unexpected ref flag on type "^(to_string z)
		))
	)

(* any data :
   accepte tout type data ou data ref,
   lifte les ref
*)
let any_data_cond = (
function 
	  TEFF_data x -> Some (TEFF_data x)
	| TEFF_ref x  -> Some (TEFF_data x)
	| _ -> None
)

let any_num_cond = (
function 
	  TEFF_data Int  -> Some (TEFF_data Int)
	| TEFF_data Real -> Some (TEFF_data Real)
	| TEFF_ref Int  -> Some (TEFF_data Int)
	| TEFF_ref Real -> Some (TEFF_data Real)
	| _ -> None
)

(* type "fonctionnel", pour les macros et les opérateurs *)
type profile =
	t list * t list

(* acceptable profile for external func *)
let is_data_profile (i,o) = (
	List.fold_left (fun a x -> a && (is_data x)) true (i @ o)
)

let res_of_prof : profile -> t list = snd 
let params_of_prof : profile -> t list = fst
let split_prof = fun x -> x 
let get_prof tinl tout = (tinl, tout)


(* TYPE USUELS *)
let boolean  = TEFF_data Bool
let booleans = TEFF_list Bool
let boolref  = TEFF_ref Bool
let intref  = TEFF_ref Int
let integer   = TEFF_data Int
let real  = TEFF_data Real
let trace = TEFF_trace

let weight = TEFF_weight

let except = TEFF_except

(* QQ TYPES ANY ... *)
let any_data1  = TEFF_any  ("'a", any_data_cond)
let _any_data2  = TEFF_any  ("'b", any_data_cond)
let any_num1 = TEFF_any  ("'n", any_num_cond)

(* PROFILS USUELS *)
   (* simples ... *)
let prof_t_t = ([trace], [trace])
let prof_tt_t = ([trace;trace], [trace])
let prof_tw_t = ([trace;weight], [trace])
let prof_it_t = ([integer;trace], [trace])
let prof_ti_t = ([trace;integer], [trace])
let prof_bt_t = ([boolean;trace], [trace])
let prof_iit_t = ([integer;integer;trace], [trace])
let prof_b_b = ([boolean], [boolean])
let prof_bb_b = ([boolean;boolean], [boolean])
let prof_bl_b = ([booleans], [boolean])
                
let prof_ii_i = ([integer;integer], [integer])
let prof_iii_i = ([integer;integer;integer], [integer])
let prof_et_t = ([except;trace], [trace])
let prof_ett_t = ([except;trace;trace], [trace])
   (* polymorphes data ... *)
let prof_bxx_x = ([boolean;any_data1;any_data1], [any_data1])
let prof_xx_b = ([any_data1;any_data1], [boolean])
   (* surchargés numériques ... *)
let prof_nn_b = ([any_num1;any_num1], [boolean])
let prof_nn_n = ([any_num1;any_num1], [any_num1])
let prof_n_n = ([any_num1], [any_num1])

let rec of_texp = ( function 
	TEXP_predef Bool -> boolean
|	TEXP_predef Int -> integer
|	TEXP_predef Real -> real
|	TEXP_trace -> trace
|	TEXP_ref x -> ref_of (of_texp (TEXP_predef x))
)


(* compatibilité des types :
	bool -> trace
	int -> weight
	int ref -> weight
	x ref -> x
*)
let lifts_to t1 t2 = (
  let res =
	 (t1 = t2)
    || ((t1 = boolref) && (t2 = boolean))
    || ((t1 = boolean) && (t2 = trace))
	 || ((t1 = boolref) && (t2 = trace))
	 || ((t1 = integer) && (t2 = weight))
	 || ((t1 = intref) && (t2 = weight))
	 || (
		match (t1,t2) with
		  (TEFF_ref x, TEFF_data y) -> (x = y)
		| _ -> false
	 )
  in
  res
)
(* compatibilité d'un profil avec une liste de types de params
   Renvoie le type eff du résultat ou lève une exception :
	Failure OU Invalid_argument (on fait dans le détail ?)
*)
let rec match_prof tel prof = (
  (* table locale pour les types any *)
  let anytab = Hashtbl.create 2 in
  match prof with
  | ([TEFF_list Bool], [TEFF_data Bool]) -> (* a special case for xor/nor/# *)
	 let doit tc =
      try match_in_type anytab tc (TEFF_ref Bool)
      with _ -> match_in_type anytab tc boolean
    in
	 let _tins = List.map doit tel in
	 let _tout = List.map (match_out_type anytab) [TEFF_data Bool] in
    _tout
  | (txl, tres) ->
	 let doit tc tx = match_in_type anytab tc tx in
	 let _tins = List.map2 doit tel txl in
	 let _tout = List.map (match_out_type anytab) tres in
	 (* ICI : on a le profil effectif, 
		 est-ce que ca peut etre utile ??? *)
	 Verbose.exe ~flag:dbg (fun _ ->
		  Printf.fprintf stderr "CkTypeEff.match [%s] with (%s) gives %s\n"
			 (list_to_string tel)
			 (prof_to_string prof)
			 (list_to_string _tout)
	   );
	 _tout
)
(*
Vérifie la compatibilité :
- d'un type obtenu (tobtd)
- d'un type attendu (texptd)
- dans un table d'assoc. des any (anytab)
*)
and match_in_type anytab tobtd texptd =
  match (tobtd, texptd) with
  | (_ , TEFF_any (k, cond)) -> (
		try (
		  let tprev = Util.hfind anytab k in
		  match_in_type anytab tobtd tprev
      )
      with Not_found -> (
			 match (cond tobtd) with
		    | Some t -> (Hashtbl.add anytab k t ; t)
          | None -> failwith "uncompatible types"
		  )
	 )
  | _ -> (
		if (lifts_to tobtd texptd) then texptd
		else failwith "uncompatible types"
	 )
and match_out_type anytab tres = (
  match tres with
	 TEFF_any (k, _) -> (
		try (
		  Util.hfind anytab k
		) with Not_found -> (
			 failwith "uncompatible types"					
		  )
	 ) | _ -> tres
)
