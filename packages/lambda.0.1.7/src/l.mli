type term =
	| Var of string 
	| Abs of (string * term) 
	| App of term * term
(** lambda term type definition *)

val get_var: int -> string
(** [get_var i] returns a var identified by its index i *)

type 'a set = Set of 'a list
(** set used for free variables *)

val fv: term -> string set
(** [fv t] returns free variables of t *)

val fv_l: term -> string list
(** [fv_l t] returns free variables of t as a list *)

val to_string: term -> string
(** [to_string t] converts a term to its string representation *)

val eta_conversion: term -> term
(** [eta_conversion t] applies η-conversion to t, if applicable *)

val alfa_conversion: string -> string -> term -> term
(** [conversion a b t] applies α-conversion to t replacing variable a with variable b *)

val reduce_fix: term -> term
(** [reduce_fix t] applies β-reduction until there is no other reduction applicable *)

val reduce_fix_timeout: ?n:int -> term -> term
(** [reduce_fix_timeout n t] applies β-reduction until there is no other reduction 
 * 		applicable or until n iterations reached *)

val reduce: int -> term -> term
(** [reduce n t] applies n steps of β-reduction to t *)

val has_redex: term -> bool
(** [has_redex t] returns true if t has a β-reduction step *)

val subst: string -> term -> term -> term
(** [subst v t' t] *)

val len: term -> int
(** [len t] returns the length of t *)