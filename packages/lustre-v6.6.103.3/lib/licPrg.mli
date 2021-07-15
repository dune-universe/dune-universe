(* Time-stamp: <modified the 22/05/2019 (at 16:39) by Erwan Jahier> *)

(** The data structure resulting from the compilation process *)

(** Réorganisation de la compil :

    Un LicPrg est :
    - un programme Lustre SIMPLE, vérifié et cohérent,
      à base de Lic.xx
    - du lv6, on passe à LicPrg via licTab, qui fait
      UNIQUEMENT le boulot de base :
      * dé-packaging
      * résolution de l'ordre sup statique, y compris la
        récursion, en tirant "le fil" du main node
		* SAUF pour les macros prédéfinies (non-programmables)
		* résolution de la surcharge 
	 - les transformations (expansions etc.) qui étaient faites
      dans licTab sont (appelées à devenir) des phases 
      apres coup du type LicPrg -> LicPrg

	Pas très différent des infos de licTab.t
	Sauf que on utilise des map
	
*)

type t

(** nb: previous bindings disappear *)
val add_type : Lic.item_key -> Lic.type_ -> t -> t 
val add_const : Lic.item_key -> Lic.const -> t -> t 
val add_node : Lic.node_key -> Lic.node_exp -> t -> t
val del_node : Lic.node_key -> t -> t

val empty : t

(* fold : (key -> 'a -> 'b -> 'b) -> 'a t -> 'b -> 'b *)

val fold_consts : (Lic.item_key -> Lic.const -> 'a -> 'a) -> t -> 'a -> 'a
val fold_types  : (Lic.item_key -> Lic.type_ -> 'a -> 'a) -> t -> 'a -> 'a
val fold_nodes  : (Lic.node_key -> Lic.node_exp -> 'a -> 'a) -> t -> 'a -> 'a

val list_nodes  : t -> (Lic.node_key * Lic.node_exp) list

val iter_consts : (Lic.item_key -> Lic.const -> unit) -> t -> unit
val iter_types  : (Lic.item_key -> Lic.type_ -> unit) -> t -> unit 
val iter_nodes  : (Lic.node_key -> Lic.node_exp -> unit) -> t -> unit

val to_file : Lv6MainArgs.t -> t -> Lv6Id.idref option -> unit

val find_type  : t -> Lic.item_key -> Lic.type_ option
val find_const : t -> Lic.item_key -> Lic.const option
val find_node  : t -> Lic.node_key -> Lic.node_exp option
val node_exists: t -> Lic.node_key -> bool

(** choose a user node *)
val choose_node  : t -> (Lic.node_key * Lic.node_exp) option
val find_var : Lv6Id.t -> Lic.node_exp -> Lic.var_info option

val fresh_type_id : t -> Lv6Id.pack_name -> string -> Lv6Id.long
