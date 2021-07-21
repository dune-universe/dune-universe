(** COMPILATION/EXPANSION : expressions de traces

------------------------------------------------------------

Représentation des expressions de traces 


Comme son nom l'indique, sert exclusivement à représenter les
expressions de traces~!
Le "cast" des CoAlgExp.t booléennes en trace est explicite.

N.B. les exceptions sont entièrement "résolue" à

----------------------------------------------------------*)

(**********************************************************)


(** On exporte la structure du type *)
type escope = (CoIdent.t * CoAlgExp.t option) list

val new_escope : unit -> escope
val add_escope : escope -> (CoIdent.t * CoAlgExp.t option) -> escope
type src_info = CoIdent.scope_stack

type 't t =
|	TE_eps
|	TE_ref of CoIdent.t
|  TE_constraint of CoAlgExp.t * src_info
|  TE_fby of 't t * 't t
|  TE_prio of 't t list
|  TE_para of 't t list
|  TE_choice of ('t t * CoAlgExp.t option) list
|  TE_dyn_choice of int * ((int * 't t) list)
|  TE_noeps of 't t
|  TE_loop of 't t
|  TE_omega of 't t
|  TE_loopi of int * CoAlgExp.t * CoAlgExp.t * 't t * src_info
|  TE_loopa of int * CoAlgExp.t * CoAlgExp.t option * 't t * src_info
(* internal loop with inline weigth computer + compteur *)
|  TE_dyn_loop of (int -> int * int) * int * 't t
|  TE_assert of CoAlgExp.t * 't t * src_info
|  TE_strong_assert of CoAlgExp.t * 't t * src_info
|  TE_exist of escope * 't t
|  TE_raise of string 
|	TE_try of 't t * 't t option
|  TE_catch of string * 't t * 't t option
(* internal run *)
|  TE_erun of string * escope * CoAlgExp.t list * 't t
|	TE_dyn_erun      of string * Reactive.prg      * CoIdent.t list * CoAlgExp.t list * 't t 
|	TE_dyn_erun_ldbg of string * 't Reactive.prg_ldbg * CoIdent.t list * CoAlgExp.t list * 't t 
|  TE_run of string * CoAlgExp.t * escope * CoAlgExp.t list * 't t * src_info
|	TE_dyn_run of string * Reactive.prg * CoAlgExp.t * escope * CoAlgExp.t list * 't t * src_info
|	TE_dyn_run_ldbg of string * 't Reactive.prg_ldbg * CoAlgExp.t * escope * CoAlgExp.t list * 't t * src_info

(** Réinitialisation du module
    pour les compteurs de loop (au cas où) ? *)
val reset : unit -> unit

(* Le nombre de loopi/loopa crées depuis le dernier reset :
	0 .. n-1 correspondent donc aux index associés
	aux loopi et loopa	
*)
val nb_loops : unit -> int

(** Batterie de créateurs *)

val of_erun : string -> escope ->  CoAlgExp.t list -> 't t -> 't t 

(* 2nd arg is an expression AND(x = x') *)
val of_run : string -> CoAlgExp.t -> escope ->  CoAlgExp.t list -> 't t -> src_info -> 't t 

val of_constraint : CoAlgExp.t -> src_info -> 't t

val of_ref : CoIdent.t -> 't t

val of_loop : 't t -> 't t
val of_omega : 't t -> 't t

val of_loope : CoAlgExp.t -> 't t -> src_info -> 't t

val of_loopi : CoAlgExp.t -> CoAlgExp.t  -> 't t -> src_info -> 't t

val of_loopa : CoAlgExp.t -> CoAlgExp.t option  -> 't t -> src_info -> 't t

val of_fby : 't t -> 't t -> 't t

val of_prio : 't t list -> 't t

val of_para : 't t list -> 't t

val of_choice : ('t t * CoAlgExp.t option ) list -> 't t

val of_assert : CoAlgExp.t -> 't t -> src_info -> 't t
val of_strong_assert : CoAlgExp.t -> 't t -> src_info -> 't t

val of_exist : escope -> 't t -> 't t

val of_raise : string -> 't t

val of_catch : string -> 't t -> 't t option -> 't t

val of_try : 't t -> 't t option -> 't t

(** Pretty print *)
val dump : 't t -> unit
val dumps : 't t -> string

