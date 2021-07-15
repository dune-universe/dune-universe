(* Time-stamp: <modified the 26/02/2015 (at 13:44) by Erwan Jahier> *)

(** Create packages from Model instances.  *)

(** Une seule fonctionnalité : transformer au niveau "quasi-syntaxique" des
instances de pack du style "package toto = titi(....)" en package "donné",
SI BESOIN.

(i.e. pack_info -> pack_given)

----------------------------------------------------------------------*)

(**----------------------------------------------------------------------
DESCRIPTION :

Entrée, deux tables d'infos syntaxique :
- une table ptab : (string, AstV6.pack_info srcflagged) Hashtbl.t 
- une table mtab : (string, AstV6.model_info srcflagged) Hashtbl.t 

Sortie, une table d'info de package expansées :
- une table xptab : (string, t) Hashtbl.t

Fonctionnement :
On met en relation les couples (param formel, arg effectif) :

(type t, id/type_exp) : on crée l'alias "type t = id/type_exp",
  qu'on met à la fois dans les export et dans le body
  => LES DÉCLARATIONS DE TYPES SONT EXPORTÉES

(const c : t, id/val_exp) : on crée l'alias "const c : t = id/val_exp",
  qu'on met à la fois dans les export et dans le body
  => LES DÉCLARATIONS DE CONST SONT EXPORTÉES

(node n(..)returns(...), id/node_exp) :
  - on garde le noeud "abstrait" dans export => node n(..)returns(...)
  - on définit l'alias "node n(..)returns(...) = id/node_exp" dans body


----------------------------------------------------------------------*)



(* ZZZ remplit AstTab.t par effet de bords. *)
val f : 
  (* la table des sources de modeles *)
  (Lv6Id.t, AstV6.model_info Lxm.srcflagged) Hashtbl.t ->
  (* la def de pack à traiter *)
  (AstV6.pack_info  Lxm.srcflagged) ->
  AstV6.pack_given

