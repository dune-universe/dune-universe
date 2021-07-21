(** SYNTAXE ABSTRAITE


***********************************************
NOTE SUR LA dÉCORATION DES NOEUDS SYNTAXIQUES
***********************************************

Tous les noeuds syntaxiques "importants" CONTIENNENT
une info source : cette info est par construction UNIQUE  
on peut donc s'en servir pour associer des infos
sémantiques au noeud via une table indexée par
ces info sources (type lexeme)

--------------------------------------------------*)

open Lexeme

(*
Syntaxe (très) abstraite:
on construit les tables de symboles directement a l'analyse
*)

(**********************************************)
(* Package : collection de tables de symboles *)
(* espaces de noms :                          *)
(*   - let (abstractions fonctionelles)       *)
(*      y compris les externes                *)
(*   - nodes (systèmes réactifs)              *)
(**********************************************)
type ident = string srcflaged
and except_tab = (string, except_info) Hashtbl.t
and package = {
	pck_lettab : (string, let_info ) Hashtbl.t ;
	pck_nodetab  : (string, node_info )  Hashtbl.t ;
	pck_excepttab : except_tab;
	pck_deflist  : def_item list ;
   pck_included_files_to_handle : string list
}
and
def_item =
   LetDef of ident (** key in pk_let_table *)
|  ExternDef  of ident  (** key in pk_let_table *)
|  NodeDef  of ident  (** key in pk_node_table *)
|  ExceptDef  of ident  (** key in pk_except_table *)
(**************************************************)
(* Info associée à un identificateur d'exception  *)
(* ... l'ident lui même !!                        *)
(**************************************************)
and except_info = ident
(**************************************************)
(* Info associée à un identificateur de let       *)
(* ou de extern si pas de def                     *)
(**************************************************)
(*
N.B. Soit tr le type de sortie, on distingue :
	- les ALIAS avec lti_inputs = None
     dont le type est juste "tr"
   - les MACROS sans param avec lti_inputs = []
     dont le type est "() -> tr" 
*)
and let_info = {
	lti_ident : ident;
	lti_type : type_exp option ;
	lti_inputs : (ident * type_exp) list option ;
	lti_def : val_exp option;
}
(************************************************************)
(* Type "immédiat" : type les flots, params, constantes etc *) 
(************************************************************)
and predef_type =
	Bool
|	Int
|	Real
and type_exp =
	TEXP_trace
|	TEXP_predef of predef_type
|	TEXP_ref of predef_type
(**********************************************)
(* Info associée à un identificateur de noeud *)
(**********************************************)
and node_info = {
	ndi_ident   :   ident ;
  (* XXX il faudrait distinguer les E des S ...  *)
	ndi_inputs  :   (ident * type_exp * val_exp option * (val_exp * val_exp) option) list ;
	ndi_outputs :   (ident * type_exp * val_exp option * (val_exp * val_exp) option) list ;
	ndi_def     :   val_exp
}
(* exp *)
and val_exp = val_exp_node srcflaged 
and assert_flag = Strong | Weak
and val_exp_node = 
(* zeroaire *)
	TRUE_n
|	FALSE_n
|	ICONST_n of ident
|	RCONST_n of ident
|	IDENT_n  of ident
|	PRE_n of ident
(* traces *)
|	FBY_n of val_exp * val_exp 
|	CHOICE_n of (val_exp * val_exp srcflaged option) list 
|	PRIO_n of val_exp list 
|	LOOP_n of assert_flag *val_exp
(* |	LOOPE_n of val_exp * val_exp  *)
|	LOOPI_n of val_exp * val_exp * val_exp
|	LOOPA_n of val_exp * val_exp option * val_exp
|	ASSERT_n of assert_flag * val_exp * val_exp
|	EXIST_n of (ident * type_exp * val_exp option * (val_exp * val_exp) option) list * val_exp
|	RAISE_n of ident
|	EXCEPT_n of ident list * val_exp
|	CATCH_n of ident * val_exp * val_exp option
|	TRAP_n of ident * val_exp * val_exp option
|	TRY_n of val_exp * val_exp option
|	PARA_n of val_exp list
(* pseudo-unaire *)
|	CALL_n of ident * val_exp list
(* special : déclaration, on garde l'info source *)
|	LET_n of let_info * val_exp
|	ERUN_n of (ident * type_exp option * val_exp option) list * val_exp * val_exp
|	RUN_n of ident list * val_exp * val_exp option


(* utilitaires *)

let pack_node_list p = (
  let res = ref [] in
  let xn d =
    match d with
	     NodeDef id -> res := (id.it)::!res
      | _ -> ()	
  in
    List.iter xn p.pck_deflist ;
    List.rev !res
)

let pack_get_node p s = (
	(Util.hfind p.pck_nodetab s)
)

let pack_get_let p s = (
	(Util.hfind p.pck_lettab s)
)

let pack_except_list p = 
Hashtbl.fold (fun _ i l -> i::l) p.pck_excepttab []

let (empty_package : unit -> package) =
  fun () -> 
    {
      pck_lettab = Hashtbl.create 0 ;
      pck_nodetab  = Hashtbl.create 0 ;
      pck_excepttab = Hashtbl.create 0;
      pck_deflist = [];
      pck_included_files_to_handle = []
    }

let (union : package -> package -> package) =
  fun p1 p2 -> 
    Hashtbl.iter 
      (Hashtbl.add p1.pck_lettab)
      p2.pck_lettab;
    Hashtbl.iter 
      (Hashtbl.add p1.pck_nodetab)
      p2.pck_nodetab;
    Hashtbl.iter 
      (Hashtbl.add p1.pck_excepttab)
      p2.pck_excepttab;
    { p1 with 
        pck_deflist = p1.pck_deflist @ p2.pck_deflist;
        pck_included_files_to_handle = [];
    }

