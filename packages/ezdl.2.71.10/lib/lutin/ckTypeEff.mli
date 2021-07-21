(** TYPE/BINDING CHECK : typage

------------------------------------------------------------

Type effectif :
- implémente l'équivalence de type
- et aussi le typage "fonctionnel" des macros,
  en prennant en compte le polymorphisme et la sucharge
  des opérations numériques

N.B. Pour ce qui est de l'équivalence, pour l'instant c'est 
un peu du luxe, vu qu'on n'a que des types simples ...

----------------------------------------------------------*)

(**********************************************************)

(** pour typer les variables et les expressions *)
(* devrait etre abstrait ...
type t
*)
type basic = Syntaxe.predef_type
type t =
   TEFF_weight
|  TEFF_except
|  TEFF_trace
|  TEFF_data of basic
|  TEFF_list of basic
|  TEFF_tuple of basic list
|	TEFF_any of string * any_cond
|	TEFF_ref of basic
and  any_cond = (t -> t option) 


(** pour typer les macros *)
type profile

(** GESTION DES PROFILS *)

val get_prof : t list -> t list -> profile
(** création d'un profil *)

val params_of_prof : profile -> t list
(** types des paramètres *)

val res_of_prof : profile -> t list
(** type du résultats *)

val split_prof : profile -> (t list * t list)
(** décomposition (param/res) *)


(** TYPES USUELS *)

val get_data_tuple : t list -> t

val tuple_to_data_list : t -> t  list

val boolean : t
val integer : t
val real : t
val trace : t

val is_data : t -> bool

val is_data_profile : profile -> bool

val weight : t

val except : t

(** TYPE "reference" *)
val ref_of : t -> t
(* lift x ref -> x
	raise Invalid_argument si erreur
*)
val lift_ref : t -> t
val is_ref : t -> bool

(** PROFILS USUELS 
    on utilise les mnémo.
    b -> bool
    i -> int
    n -> int ou real
    x -> int ou real ou bool 
    t -> trace
    w -> weight
    e -> exception
*)

(** - simples : *)

val prof_t_t  : profile
val prof_tt_t  : profile
val prof_it_t  : profile
val prof_ti_t  : profile
val prof_bt_t  : profile
val prof_iit_t  : profile
val prof_b_b  : profile
val prof_bb_b  : profile
val prof_bl_b  : profile
val prof_ii_i  : profile
val prof_iii_i  : profile

(** - trace * weight *)
val prof_tw_t  : profile

(** - exception * trace *)
val prof_et_t : profile
val prof_ett_t : profile

(** - polymorphes : *)

val prof_bxx_x  : profile
val prof_xx_b  : profile

(** - surchargés : *)

val prof_nn_b  : profile
val prof_nn_n  : profile
val prof_n_n  : profile

 
(** EXPRESSION DE TYPE -> TYPE EFFECTIF *)

val of_texp : Syntaxe.type_exp -> t

(** PRETTY-PRINT *)

val to_string : t -> string
val prof_to_string : profile -> string
val list_to_string : t list -> string

(** COMPATIBILITÉ DES t
lifts_to x y ssi un x peut être utilisé en place d'un y
en particulier eq => lifts_to
*)

val lifts_to : t -> t -> bool

(** RÉSOLUTION DES profils :
   - renvoie le type eff du résultat si compatible
   - raise Failure ou Invalid_argument sinon
*)

val match_prof : t list -> profile -> t list

