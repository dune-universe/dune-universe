(* Time-stamp: <modified the 29/08/2019 (at 14:44) by Erwan Jahier> *)


(** (Raw) Abstract syntax tree of source Lustre V6 programs. 

    This is a syntax tree represented by Hash tables.
*)

open Lxm  (* pour la remontée au source *)
open AstCore

(**********************************************)
(** Constructeur de type "avec erreur info"   *)
(**********************************************)
(* QUESTION: pourquoi ne pas le mettre dans le module Error? *)
type 'a error  = 
    Ok of 'a
  | Error of string



(**********************************************************************************)
type t = 
    PRPackBody of string list * packbody
  | PRPack_or_models of string list * pack_or_model list
and
  pack_or_model =
    NSPack   of  pack_info srcflagged
  | NSModel  of  model_info srcflagged
and
  model_info = {
    mo_name  : Lv6Id.pack_name ;
    mo_uses  : Lv6Id.pack_name srcflagged list ;
    mo_needs : static_param srcflagged list ;
    (* N.B. CAS PARTICULIER DE item_info *)
    mo_provides : item_info srcflagged list option;
    mo_body : packbody ;
  }
and pack_info = {
    pa_name : Lv6Id.pack_name ;
    pa_def : pack_def ;
  }
and
  pack_def = 
    PackGiven of pack_given
  | PackInstance of pack_instance
and
  pack_given = { 
    pg_uses  : Lv6Id.pack_name srcflagged list ;
    (* N.B. CAS PARTICULIER DE item_info *)
    pg_provides : item_info srcflagged list option;
    pg_body : packbody ;
  }
and
  pack_instance = {
    pi_model : Lv6Id.t ;
    pi_args : (Lv6Id.t * static_arg srcflagged) list ;
  }
and
  packbody = {
    (* Collection de noeuds, types const etc.
	- une table pour chaque sorte de defs
	- une liste de defs permettant de les
	ressortir dans l'ordre                   
    *)
    pk_const_table : (Lv6Id.t, const_info srcflagged ) Hashtbl.t ;
    pk_type_table  : (Lv6Id.t, type_info  srcflagged ) Hashtbl.t ;
    pk_node_table  : (Lv6Id.t, node_info  srcflagged ) Hashtbl.t ;
    pk_def_list    : item_ident list ; 
  }


(**********************************************
Utilitaires pour fabriquer des packages
**********************************************)
let give_pack_this_name name pbdy = (
   {
      pa_name = name;
      pa_def = PackGiven {
      	pg_uses = [];
      	pg_provides = None;
      	pg_body = pbdy;
		}
   }
)


(*----------------------------------------------------------------------------*)
(*                      INTERFACE AVEC LE PARSER                              *)
(*----------------------------------------------------------------------------*)


(**********************************************
Construction d'un packbody
n.b. les tables sont copiées et donc
réutilisables par l'appelant
**********************************************)

let make_packbody ctab ttab otab dlst = (
  {
    pk_const_table = Hashtbl.copy ctab;
    pk_type_table  = Hashtbl.copy ttab;
    pk_node_table  = Hashtbl.copy otab;
    pk_def_list    = dlst
  }
)


(*---------------------------------------------------------------------
lexeme_of_left_part
-----------------------------------------------------------------------
Rôle : retourne le lexeme ``principal'' d'une expression/d'un 

Entrées : val_exp

Sorties : Lxm.t

Effets de bord :
----------------------------------------------------------------------*)

let rec lexeme_of_left_part = function
  | LeftVar sflg -> sflg.src
  | LeftField (x, _) -> lexeme_of_left_part x 
  | LeftArray (x, _) -> lexeme_of_left_part x
  | LeftSlice (x, _) -> lexeme_of_left_part x
	  

(********************************************)
let (pack_or_model_to_string: pack_or_model -> string) =
  function
    | NSPack  pi -> Lv6Id.pack_name_to_string pi.it.pa_name ^ " (pack) "
    | NSModel mi -> Lv6Id.pack_name_to_string mi.it.mo_name ^ " (model) "
