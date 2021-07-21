(** COMPILATION/GENERATION D'AUTOMATE : interface
------------------------------------------------------------

La génération d'automate intervient après l'expansion, qui
    a produit les infos suivantes :

	_support_tab :  (CoIdent.t, support_info) Hashtbl.t
	_alias_tab   :  (CoIdent.t, alias_info) Hashtbl.t
	_trace_tab   :  (CoIdent.t, trace_info) Hashtbl.t
	_main_trace  :  CoIdent.t


Les exceptions ont été expansées en place, par des identificateurs
uniques dans le expressions associées. Il n'y a donc pas de table
pour elles.


WARNING: 2011/01/05
Split between (full) construction (AutoGen) and
on the fly exploration (AutoExplore)

----------------------------------------------------------*)

(** le type du résultat *)
type weightexp = 
|	W_huge 
|	W_exp of CoAlgExp.t

(* obsolete
val algexp_of_weight: weightexp -> CoAlgExp.t
*)

(* gtree -> forme externe des transitions adaptée 
	à la génération de lurette *)

type gtree = string * gtree_node
and gtree_node =
|	GT_leaf of (Guard.t * string)
|	GT_choice of (weightexp option * gtree) list
|	GT_stop of string

val gtree_size : gtree -> int

(* trans -> forme externe des transitions
   (liste de trans) adaptée à la génération de lucky  *)
type trans = {
   src: string;
   wgt: weightexp option; 
   form: Guard.t;
   dest: string;
}  
(* N.b. le CoTraceExp.t est l'expression lutin
    EXPANSEE correspondant à l'état, c'est pas forcement tres
    lisible => juste pour le debug ... *)
type state_info = 
   SS_stable of Expand.tbl CoTraceExp.t
|  SS_transient
|  SS_final of string

(* Abstrait *)
type t

(* data configuration *)
type config

val make_config: string -> config

val source : t -> Expand.t
val init_control : t -> string
val transitions : t -> trans list

(* Explore le sous-graphe du state *)
val config2gtree : t -> config -> gtree * t
val config2trans : t -> config -> trans list * t

(* MUST BE INITIALIZED WITH A FUNCTION :
	CoAlgExp.t -> Exp.t
*)
val init : Expand.t -> t

(* Construit TOUT l'automate *)
val make : Expand.t -> t

val get_state_def : t -> string -> Expand.tbl CoTraceExp.t

val get_state_info : t -> string -> state_info 

(* Table des états connus *)
val states : t -> state_info Util.StringMap.t 

val dump : t -> unit

