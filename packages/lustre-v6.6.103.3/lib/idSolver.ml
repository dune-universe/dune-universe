(* Time-stamp: <modified the 29/08/2019 (at 14:55) by Erwan Jahier> *)

(** Utilities for managing node environements (items tables)  *)


type t = {
  id2const : Lv6Id.idref -> Lxm.t -> Lic.const;
  id2type  : Lv6Id.idref -> Lxm.t -> Lic.type_;
  id2node  : Lv6Id.idref -> Lic.static_arg list -> Lxm.t -> Lic.node_exp;

  id2var   : Lv6Id.t -> Lxm.t -> Lic.var_info;

(*
global_symbols ->
- la table à résoudre les idents SANS pack
(i.e. toto au lieu de Titi::toto) dans un pack courant :
c'est lié au mécanisme du "uses"
- normallement, elle est "cachée" dans les fonctions id2const, id2type, id2node ...
Pourtant on mise ici quand meme !!
Visiblement, sert dans 3 trucs :
- AstTabSymbol.find_pack_of_const
- AstTabSymbol.find_pack_of_type
et
- Ast2lic.get_abstract_static_params

pour les 2 premiers : pas sur a quoi ca sert, a creuser ...
pour le 3eme ->
   ca sert uniquement a trouver la "nature" attendue
   des params statiques (type const ou node) necessaire pour
   calculer les parms effectif (et donc la cle des noeud a compiler)
   dans Ast2lic.of_node 
Mais c'est un bug : impossible de trouver avec elle les
params statiques d'un noeud appele avec son nom complet (Titi::toto) !
Donc, c'est pas ça qui faut ...

Solution (pas hyper satisfaisante) :
- ajouter le AstTab global, qui permet de retrouver nímporte quelle
  source ... pas bien dans l'esprit "abstraire" mais bon...
- revoir si le global_symbols est vraiment necessaire ?

*)
  global_symbols  : AstTabSymbol.t;
  all_srcs : AstTab.t;
}

type local_env = {
  lenv_node_key : Lic.node_key ;
(*   lenv_globals : pack_env ; *)
  lenv_types : (Lv6Id.t, Lic.type_) Hashtbl.t ;
  lenv_const : (Lv6Id.t, Lic.const) Hashtbl.t ; 
  lenv_nodes : (Lv6Id.t, Lic.node_key) Hashtbl.t ; 
  lenv_vars  : (Lv6Id.t, Lic.var_info) Hashtbl.t ; 
}


let (make_local_env : Lic.node_key -> local_env) =
  fun nk ->
    let res =
      {
        lenv_node_key = nk;
        lenv_types = Hashtbl.create 0;
        lenv_const = Hashtbl.create 0;
        lenv_nodes = Hashtbl.create 0;
        lenv_vars  = Hashtbl.create 0;
      }
    in
      (* fill tables using static arg info *)
      List.iter
        (function
           | Lic.ConstStaticArgLic(id,ce) -> Hashtbl.add res.lenv_const id ce
           | Lic.TypeStaticArgLic(id,te)  -> Hashtbl.add res.lenv_types id te
           | Lic.NodeStaticArgLic(id, nk) -> Hashtbl.add res.lenv_nodes id nk
        )
        (snd nk);
      
      res

let dump_local_env oc e =
   let pt i t = Printf.fprintf oc "#    type %s = %s\n" i (Lic.string_of_type t) in
   Hashtbl.iter pt e.lenv_types;
   let pc i t = Printf.fprintf oc "#    const %s = %s\n" i (Lic.string_of_const t) in
   Hashtbl.iter pc e.lenv_const;
   (* let pn i (n,_,_) = Printf.fprintf oc "#    node %s = %s\n" i (string_of_node_key n) in *)
   let pn i nk = Printf.fprintf oc "#    node %s = %s\n" i (Lic.string_of_node_key nk) in
   Hashtbl.iter pn e.lenv_nodes;


(* Grouping those 2 ones is sometimes useful *)
type node_env = {
  local : local_env;
  global: t;
}

let (lookup_type: local_env -> Lv6Id.idref -> Lxm.t -> Lic.type_) =
  fun env id _lxm ->
    Hashtbl.find env.lenv_types (Lv6Id.of_idref false id)

let (lookup_node : local_env -> Lv6Id.idref -> Lxm.t -> Lic.node_key) =
  fun env id _lxm ->
    Hashtbl.find env.lenv_nodes (Lv6Id.of_idref false id)
    
let (lookup_const: local_env -> Lv6Id.idref -> Lxm.t -> Lic.const) =
  fun env id _lmx ->  
    Hashtbl.find env.lenv_const (Lv6Id.of_idref false id)
          
let (lookup_var: local_env -> Lv6Id.t -> Lxm.t -> Lic.var_info) = 
  fun env id _lmx ->
    Hashtbl.find env.lenv_vars id



let node_exp_of_node_key
    (id_solver: t) (node_key: Lic.node_key) (lxm : Lxm.t)
    : Lic.node_exp =
  let (id, sargs) = node_key in
  id_solver.id2node (Lv6Id.idref_of_long id) sargs lxm

let var_info_of_ident 
    (id_solver: t) (id: Lv6Id.t) (lxm : Lxm.t)
    : Lic.var_info =
  id_solver.id2var id lxm

let const_eff_of_item_key
    (id_solver: t) (id: Lic.item_key) (lxm : Lxm.t) 
    : Lic.const =
  id_solver.id2const (Lv6Id.idref_of_long id) lxm



