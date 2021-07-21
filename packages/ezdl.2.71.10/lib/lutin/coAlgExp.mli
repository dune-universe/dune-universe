(** COMPILATION/EXPANSION : expressions algébriques

------------------------------------------------------------

Représentation des expressions algébriques.

----------------------------------------------------------*)

(**********************************************************)

(** 
C'est la structure utilisée pour toutes les expressions non "trace"
(grosso/modo du Lustre !).

La structure est très simple~: on distingue plusieurs cas de
feuilles, et un seul cas de noeud (opérateur).

Elle contient, dès la construction, les infos sémantiques associées :
type de valeur et "controlabilité".

Rappel~: une alg_exp est contrôlable si et seulement si elle contient
au moins une référence à une variable support contrôlable (output ou local).

*)

(* type t *)

type node =
|  AE_true
|  AE_false
|  AE_const of string
(*
|  AE_iconst of Syntaxe.ident
|  AE_rconst of Syntaxe.ident
*)
|  AE_iconst of string
|  AE_rconst of string
|  AE_ival of int
|  AE_rval of float
|  AE_support of CoIdent.t
|  AE_pre of CoIdent.t
|  AE_alias of CoIdent.t
|  AE_call of CoIdent.t * t list
|  AE_external_call of
      CoIdent.t *
      CkIdentInfo.extern_info *
      CkTypeEff.profile *
		t list
and t = {
   ae_type : CkTypeEff.t;
   ae_ctrl : bool;
   ae_val : node ;
}

val sizeof : t -> int


(** Batterie de "constructeurs" *)

val of_true : t

val of_false : t

val of_huge_weight : t

val of_const : string -> CkTypeEff.t -> t

(*
val of_iconst : Syntaxe.ident -> t
val of_rconst : Syntaxe.ident -> t
*)
val of_iconst : string -> t
val of_rconst : string -> t

val of_bval : bool -> t
val of_ival : int -> t
val of_rval : float -> t

(** Réference à une variable du support *) 
val of_support : CoIdent.t -> CkTypeEff.t -> bool -> t

(** Réference au pre d'une variable du support *) 
val of_pre : CoIdent.t -> CkTypeEff.t -> t

(** Référence à un alias *) 
val of_alias : CoIdent.t -> CkTypeEff.t -> bool -> t

(** Tout calcul est un appel, prédéfini ou non *)
val of_call : CoIdent.t -> CkTypeEff.t -> t list -> t 
val of_external_call :
		CoIdent.t -> CkIdentInfo.extern_info ->
		CkTypeEff.profile -> CkTypeEff.t -> t list -> t 

(** On donne quand même le and ... *)
val of_and : t -> t -> t

val of_big_and : t list -> t

val of_eq : t -> t -> t

val of_not : t -> t

(** ... et le ite i(il faut donner le type) *)
val of_ite : CkTypeEff.t -> t -> t -> t -> t

(** Infos *)
val is_controlable : t -> bool

val get_type : t -> CkTypeEff.t

(** Affichage prefixé sur stdout *)
val dump : t -> unit

(** Affichage prefixé sur os *)
val dumpf : out_channel -> t -> unit

(** Affichage infixé (autant que possible) sur os 
n.b. compatible Lustre/Lucky
*)
val lus_dumpf : out_channel -> t -> unit

(** Affichage infixé (autant que possible) dans une string 
n.b. compatible Lustre/Lucky
*)
val lus_dumps : t -> string 

