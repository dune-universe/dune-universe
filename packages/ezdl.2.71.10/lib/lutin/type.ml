(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: type.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr
*)



type enum_value = string
type field = string

type t =
  | BoolT
  | IntT
  | FloatT
  | UT of structured
and
  structured =
  | ArrayT  of int * t
  | StructT of (field * t) list
  | EnumT   of enum_value list


let rec (typ_to_string_param : string -> t -> string) =
  fun float vt ->
    match vt with
	BoolT -> "bool"
      | IntT -> "int"
      | FloatT -> float

      | UT(ut) -> structured_to_string_param float ut
and
  (structured_to_string_param : string -> structured -> string) =
  fun float ut ->
    match ut with
      | ArrayT(i, t) ->
	  ((typ_to_string_param float t) ^ "^" ^ (string_of_int i))
      | StructT(fl) -> (
	  (List.fold_left
	     (fun acc (fn, ft) ->
		(acc ^ fn ^":"^ (typ_to_string_param float ft) ^ "; ")
	     )
	     "{"
	     fl)
	  ^ "}")
      | EnumT(el) -> (
	  let _ = assert (el <> []) in
	    ( List.fold_left
		(fun acc e -> (acc ^ ", " ^ e))
		("(" ^ (List.hd el) )
		(List.tl el)
	    ) ^ ")")


let to_string = typ_to_string_param "float"
let to_string2 = typ_to_string_param "real"
let to_string3 = typ_to_string_param "double"

let structured_to_string = structured_to_string_param "real"


(****************************************************************************)

(* exported  *)
let rec (to_cstring : t -> string) =
  fun vt ->
    match vt with
	BoolT -> "_bool"
      | IntT -> "_int"
      | FloatT -> "_real"

      | UT(ut) -> structured_to_cstring ut
and

  (structured_to_cstring : structured -> string) =
  fun  ut ->
    match ut with
      | ArrayT(i, t) ->
	  ((to_cstring t) ^ "^" ^ (string_of_int i))
      | StructT(fl) -> (
	  (List.fold_left
	     (fun acc (fn, ft) ->
		(acc ^ fn ^":"^ (to_cstring ft) ^ "; ")
	     )
	     "{"
	     fl)
	  ^ "}")
      | EnumT(el) -> (
	  let _ = assert (el <> []) in
	    ( List.fold_left
		(fun acc e -> (acc ^ ", " ^ e))
		("(" ^ (List.hd el) )
		(List.tl el)
	    ) ^ ")")


(* exported  *)
let rec (to_cstring_bis : t -> string) =
  fun vt ->
    match vt with
	BoolT -> "bool"
      | IntT -> "int"
      | FloatT -> "real"
      | UT(ut) -> structured_to_cstring_bis ut
and
  (structured_to_cstring_bis : structured -> string) =
  fun  ut ->
    match ut with
      | ArrayT(i, t) ->
	  ((to_cstring_bis t) ^ "^" ^ (string_of_int i))
      | StructT(fl) -> (
	  (List.fold_left
	     (fun acc (fn, ft) ->
		(acc ^ fn ^":"^ (to_cstring_bis ft) ^ "; ")
	     )
	     "{"
	     fl)
	  ^ "}")
      | EnumT(el) -> (
	  let _ = assert (el <> []) in
	    ( List.fold_left
		(fun acc e -> (acc ^ ", " ^ e))
		("(" ^ (List.hd el) )
		(List.tl el)
	    ) ^ ")")

let rec (to_data_t : t -> Data.t) = 
  function
    | BoolT  -> Data.Bool
    | IntT   -> Data.Int
    | FloatT -> Data.Real
    | UT (ArrayT(n,a)) -> Data.Array(to_data_t a,n)
    | UT (StructT fl) -> Data.Struct ("struct", (List.map (fun (id,t) -> id, to_data_t t) fl))
    | UT (EnumT el) -> Data.Enum ("enum", el)


