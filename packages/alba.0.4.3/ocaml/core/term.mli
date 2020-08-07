open Fmlib.Module_types

val bruijn_convert: int -> int -> int


module Value:
sig
    type t =
      | Int of int (* int32? *)
      | Char of int
      | String of string
      | Unary of (t -> t)
      | Binary of (t -> t -> t)

    val number_values: string -> t list
    val int_plus: t
    val int_minus: t
    val int_times: t
    val int_negate: t
    val string_concat: t
    val apply: t -> t -> t

    val is_equal: t -> t -> bool
    (** [is_equal a b] checks if [a] and [b] are the same value. *)

    val compare: t -> t -> int
    (** compare for sorting purposes. *)
end



module Sort:
sig
    type t =
      | Proposition
      | Any of int


    val compare: t -> t -> int
    (** compare for sorting purposes. *)

    (** [is_sub s1 s2] Is [s1] a subtype of [s2] (or equal)? *)
    val is_sub: t -> t -> bool

    (** [is_super s1 s2] Is [s1] a supertype of [s2] (or equal)? *)
    val is_super: t -> t -> bool

    val type_of: t -> t

    val pi_sort: t -> t -> t
end



module Lambda_info:
sig
  type t
  val name:         t -> string
  val is_anonymous: t -> bool
  val is_typed:     t -> bool

  val typed:   string -> t
  val untyped: string -> t
end


module Pi_info:
sig
  type t
  val name:         t -> string
  val is_anonymous: t -> bool
  val is_arrow:     t -> bool
  val is_typed:     t -> bool

  val arrow: t
  val typed:   string -> t
  val untyped: string -> t
end


module Application_info:
sig
    type t =
      | Normal
      | Implicit
      | Binary
      | Unary
end



type t =
  | Sort of Sort.t

  | Value of Value.t

  | Variable of int

  | Typed of t * typ

  | Appl of t * t * Application_info.t

  | Lambda of typ * t * Lambda_info.t

  | Pi of typ * typ * Pi_info.t

  | Where of string * typ * t * t
    (** Where [(name, tp, exp, def)] is equivalent to the beta redex
        [Appl ( Lambda (tp, exp, "name"), def)]
    *)

and typ = t

and formal_argument = string * typ

and inductive


type t_n   = t * int
type typ_n = typ * int


val proposition: t
val any: t
val any_uni: int -> t

val variable: int -> t

val application: t -> t -> t
val implicit_application: t -> t -> t
val binary: t -> t -> t -> t
val applications: t -> t list -> t

val lambda0: string -> bool -> typ -> t -> t
val product0: string -> bool -> typ -> typ -> typ

val lambda:  string -> typ -> t -> t
val product: string -> typ -> typ -> t
val arrow:   typ -> typ -> typ

val lambda_untyped:  string -> typ -> t -> t
val product_untyped: string -> typ -> typ -> t

val lambda_in:  formal_argument list -> t -> t
val product_in: formal_argument list -> t -> t


val expand_where: string -> typ -> t -> t -> t
(** Rewrite a where block as an application with a lambda term. *)


(** [char code] character value. *)
val char:   int -> t

(** [string str] string value. *)
val string: string -> t

val number_values: string -> t list


val type_of_sort: Sort.t -> typ


val is_sort: typ -> bool

val pi_sort: typ -> typ -> typ



val split_application: t -> t * (t * Application_info.t) list
(**
    [split_application t]
*)




val map: (int -> int) -> t -> t
(**
    [map f t]

    Map the free variables with [f]
*)



(** [up_from start delta t]: increases all free variables >= [start] in [t] by
[delta]

    {e requires}: [0 <= delta]
 *)
val up_from: int -> int -> t -> t


(** [up delta t]: increases all free variables in [t] by [delta]

    {e requires}: [0 <= delta]
 *)
val up: int -> t -> t

(** [up1 t]: increases all free variable in [t] by 1 *)
val up1: t -> t

(** [down_from start delta t]: decreases all free variables >= [start] in [t] by
[delta] if possible. Returns [None] it there is a free variable in [t] with
index [i < delta].

    {e requires}: [0 <= delta]
 *)
val down_from: int -> int -> t -> t option


(** [down delta t]: decreases all free variables in [t] by [delta] if possible.
Returns [None] if there is a free variable in [t] with index [i < delta].

    {e requires}: [0 <= delta]
 *)
val down: int -> t -> t option



val substitute0: (int -> t) -> bool -> t -> t
(** [substitute0 f beta_reduce t] *)

val substitute: (int -> t) -> t -> t
(** [substitute f term] substitutes each free variable [i] in [term] by the term
[f i]. *)



val substitute_with_beta: (int -> t) -> t -> t
(** [substitute f term] substitutes each free variable [i] in [term] by the term
[f i] and do beta reduction in case that [f i] appears in a function position
and is a function abstraction. *)



val apply: t -> t -> t



(** [apply_nargs f n mode] returns

       f (Var (n-1)) ... (Var 0)

    where all applications are done with mode [mode].
*)
val apply_nargs: t -> int -> Application_info.t -> t


val fold_free: (int -> 'a -> 'a) -> t -> 'a -> 'a


val to_index: int -> t -> t
val to_level: int -> t -> t


val has: (int -> bool) -> t -> bool
(** [has p term] does the term have a free variable satisfying [p]? *)

val forall: (int -> bool) -> t -> bool
(** [forall p term] do all free variables in [term] satisfy [p]? *)



val has_variable: int -> t -> bool



(** Monadic functions *)
module Monadic (M: MONAD):
sig
    val fold_free: (int -> 'a -> 'a M.t) -> t -> 'a -> 'a M.t
    val map_free: (int -> int M.t) -> t -> t M.t
end

(** Inductive types *)
module Inductive:
sig
    type term = t

    type t

    val make_simple_inductive:
        int
        -> formal_argument list  (** Parameters *)
        -> formal_argument       (** Type *)
        -> formal_argument list  (** Constructors *)
        -> inductive
end
