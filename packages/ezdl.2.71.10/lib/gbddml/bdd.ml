(* ----------------------------------------------------------------
Interface ocaml pour gbdd
-------------------------------------------------------------------
On garde les noms compatibles "cudd-idl" utilisés dans lurette :

Bdd._print ??

Bdd.draw
Bdd.nbminterms
---------------------------------------------------------------- *)

(* Le type abstrait *)
type t

(* Init du module *)
external init_psz_verb   : int -> bool -> unit = "gbdd_cml_init_with_psz_verb"

let init : ?pagesize:(int) -> ?verbose:(bool) -> unit -> unit =
fun ?(pagesize=10000) ?(verbose=true) _ ->
	init_psz_verb pagesize verbose

(* Accès aux noeuds *)
external root_var  : t -> int   = "gbdd_cml_root_var"
external high_part : t -> t     = "gbdd_cml_high_part"
external low_part  : t -> t     = "gbdd_cml_low_part"

(* Tests *)

external is_leaf   : t -> bool  = "gbdd_cml_is_leaf"
external is_true   : t -> bool  = "gbdd_cml_is_true"
external is_false  : t -> bool  = "gbdd_cml_is_false"

(* Constantes *)
external dtrue   : unit -> t  = "gbdd_cml_true"
external dfalse  : unit -> t  = "gbdd_cml_false"
external null   : unit -> t  = "gbdd_cml_null"

(* Identité et Inverse *)
external idy     : int -> t   = "gbdd_cml_idy"
external nidy    : int -> t   = "gbdd_cml_nidy"

(* Opérations booléennes *)
external dnot    : t -> t  = "gbdd_cml_not"
external dor     : t -> t -> t = "gbdd_cml_or"
external dand    : t -> t -> t = "gbdd_cml_and"
external xor     : t -> t -> t = "gbdd_cml_xor"
external eq      : t -> t -> t = "gbdd_cml_eq"
external ite     : t -> t -> t -> t = "gbdd_cml_ite"

(* Infos sur la structure *)
external size : t -> int = "gbdd_cml_size"
external supportsize : t -> int = "gbdd_cml_supportsize"

(* quantification *)
external exist_local : t -> t -> t = "gbdd_cml_exist"
external forall_local : t -> t -> t = "gbdd_cml_forall"

let support_of_list vars =
  assert (vars <> []);
  List.fold_left
    (fun acc i -> dand acc (idy i))
    (idy (List.hd vars))
    (List.tl vars) 

let (exist : int list -> t -> t) =
  fun vars bdd ->
    exist_local (support_of_list vars) bdd

let (forall : int list -> t -> t) =
  fun vars bdd ->
    forall_local(support_of_list vars) bdd

(* Extra *)
external print_mons : t -> unit = "gbdd_cml_print_mons"
(* compatibilité cudd *)
external topvar  : t -> int   = "gbdd_cml_root_var"
external dthen   : t -> t     = "gbdd_cml_high_part"
external delse   : t -> t     = "gbdd_cml_low_part"
external ithvar  : int -> t   = "gbdd_cml_idy"
external is_cst : t -> bool  = "gbdd_cml_is_leaf"

external support : t -> t     = "gbdd_cml_cube"

(* Extra programmés directement en caml *)

let list_of_support (b: t) = (
	let rec los x = (
		if(is_leaf x) then []
		else (
			(topvar x)::(los (dthen x))
		)
	) in
	los (support b)
)
