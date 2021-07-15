(** Time-stamp: <modified the 27/09/2017 (at 09:54) by Erwan Jahier> *)


(** Donne toute les méthodes d'un composant. *)
val get_all_methods: Soc.t -> Soc.step_method list

 
(** Fonctions de représentation des objets SOC. *)
val string_of_type_ref            : Data.t -> string
val string_of_soc_key             : Soc.key -> string
val string_of_var                 : Soc.var -> string
val string_of_operation           : Soc.atomic_operation -> string
val string_of_gao                 : Soc.gao -> string
val string_of_gaos_list           : Soc.gao list -> string
val string_of_filter              : Soc.var_expr -> string
val string_of_method              : Soc.t -> Soc.step_method -> string
val string_interface_of_method    : Soc.t -> Soc.step_method -> string
val string_of_precedence          : Soc.precedence -> string
val string_of_profile             : Soc.var list * Soc.var list -> string
val string_interface_of_soc       : Soc.t -> string
val string_of_soc                 : Soc.t -> string

val string_of_soc_key_ff          : Soc.key  -> Format.formatter -> unit
val string_of_type_ref_ff         : Data.t   -> Format.formatter -> unit
val string_of_var_ff              : Soc.var  -> Format.formatter -> unit
val string_of_operation_ff        : Soc.atomic_operation      -> Format.formatter -> unit
val string_of_filter_ff           : Soc.var_expr         -> Format.formatter -> unit
val string_of_gao_ff              : Soc.gao  -> Format.formatter -> unit
val string_of_method_ff           : Soc.t    -> Soc.step_method -> Format.formatter -> unit
val string_interface_of_method_ff : Soc.t    -> Soc.step_method -> Format.formatter -> unit
val string_of_precedence_ff       : string * string list   -> Format.formatter -> unit
val string_of_profile_ff          : Soc.var list * Soc.var list    -> Format.formatter -> unit
val string_interface_of_soc_ff    : Soc.t -> Format.formatter -> unit
val string_of_soc_ff              : Soc.t -> Format.formatter -> unit


(** [output header_flag pack_name] dumps the soc list into a
    file. [header_flag] states whether or not headers (comment)
    should be printed *)
val output: bool -> string -> Soc.t list -> unit

(* Raise a compile error in not found *)
val find : Lxm.t -> Soc.key -> Soc.tbl -> Soc.t

val add: Soc.key -> Soc.t -> Soc.tbl -> Soc.tbl

                                            
(* Raise an internal error if not found *)
val find_no_exc : Soc.key -> Soc.tbl -> Soc.t

(* gen_index_list 5 = [0;1;2;3;4] *)
val gen_index_list : int -> int list
val my_string_of_float_precision : int option -> float -> string

val is_memory_less : Soc.t -> bool

(** should we omit the ctx in arg of the step function?
    yes in Heap mode when the soc is memory less.
 *)
val ctx_is_global : Soc.t -> bool

(** [filter_step_params index_list var_list]

only keeps the var present in the index list. 

For instance, 
filter_step_params [0;1;4] [v1;v2;v3;v4;v5] =  [v1;v2;v5]

nb : we suppose that the index list is in increasing order.
*)
val filter_step_params : int list -> 'a list -> 'a list

val get_rank : 'a -> ('a * 'b) list -> int

                                      
(* if var = t.[2].field, then it returns (also) t.[2] and t  *)
val get_top_var : Soc.var_expr  -> Soc.var_expr

val lustre_string_of_var_expr: Soc.var_expr -> string
