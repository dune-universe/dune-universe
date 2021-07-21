(*-----------------------------------------------------------------------
** Copyright (C) - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: prevar.ml
** Author: erwan.jahier@univ-grenoble-alpes.fr 
*)

(** To give an internal name to pre var *)
let (create_prevar_name : int -> string -> string) =
  fun i str ->
    ("$pre" ^ (string_of_int i) ^ str)

let (is_pre_var : string -> bool) =
  fun str ->
    try (String.sub str 0 4) = "$pre"
    with _ -> false




(* exported *)
let rec (get_root_var_name: string -> string) =
  fun str ->
    try
      if
	(String.sub str 0 4) = "$pre"
      then
	remove_beginning_number (String.sub str 4 ((String.length str) - 4))
      else
	str
    with
	_ -> str

and remove_beginning_number str =
  try
    let _ = int_of_string (String.sub str 0 1) in
      remove_beginning_number (String.sub str 1 ((String.length str) - 1))
  with
      Failure _ -> str
    | _ -> assert false



(** [split_pre_var_string pren_vn] returns the number of pre that is
  applied to the variable as well as the original variable the is is
  holding on. *)
(* val split_pre_var_string : string -> int * string *)

let rec (split_pre_var_string: string -> int * string) =
  fun pren_vn ->
    let n_vn = String.sub pren_vn 4 ((String.length pren_vn) - 4) in
      try split_pre_var_string_acc "" n_vn
      with Failure _ ->
	assert false
	(* Bad format of pre var. It should be of the form, e.g., $pre32toto *)

and (split_pre_var_string_acc: string -> string -> int * string) =
  fun n_str str ->
    let i_str = String.sub str 0 1 in
      try
	let _ = int_of_string i_str in
	  split_pre_var_string_acc
	    (n_str ^ i_str)
	    (String.sub str 1 ((String.length str) - 1))
      with Failure _ ->
	((int_of_string n_str), str)


let _ = assert ((split_pre_var_string "$pre44toto") = (44, "toto"))



(* exported *)
let (format: string -> string) =
  fun vn ->
    if
      not (is_pre_var vn)
    then
      vn
    else
      (
	let (pre_nb, name) = split_pre_var_string vn in
	let pre_list = Util.unfold (fun _ -> "pre ") () pre_nb in
	  ("(" ^ (String.concat "" pre_list) ^ name) ^ ")"
      )


(* exported *)
let (give_pre_var_name : string -> string) =
  fun var ->
    if
      is_pre_var var
    then
      let (i, str) = split_pre_var_string var in
	create_prevar_name (i+1) str
    else
      create_prevar_name 1 var




(* let (format_old: string -> string) = *)
(*   fun vn ->  *)
(*     try  *)
(*       let l = String.length vn in *)
(*       let char_reg = Str.regexp "[^0-9]" in *)
(*       let i = Str.search_forward char_reg vn 4 in *)
(*       let pre_nb = (int_of_string (String.sub vn 4 (i-4))) in *)
(*       let pre_list = unfold (fun _ -> "pre ") () pre_nb in *)
(* 	("(" ^ (String.concat "" pre_list) ^ (String.sub vn i (l-i)) ^ ")") *)
(*     with  *)
(*       | _ ->  vn *)



(* exported *)
let (get_pre_var_name: string -> string) =
  fun pre_n_vn ->
    let (n, vn) = split_pre_var_string pre_n_vn in
      if (n-1) <> 0
      then ("$pre" ^ (string_of_int (n-1)) ^ vn)
      else vn

let _ = assert ((get_pre_var_name "$pre3toto") = "$pre2toto")
let _ = assert ((get_pre_var_name "$pre1toto") = "toto")
