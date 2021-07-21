(*-----------------------------------------------------------------------
** Copyright (C) 2002, 2003 - Verimag.
** This file may only be copied under the terms of the CeCill
** Public License
**-----------------------------------------------------------------------
**
** File: ne.mli
** Main author: erwan.jahier@univ-grenoble-alpes.fr
*)

(** Internal (normal) representation of expressions. *)

(** Normal expressions type. *)
type t


type subst = (string * Value.num) * t


(** [is_a_constant ne] returns true iff [ne] is a constant. *)
val is_a_constant : t -> bool

(** [dimension ne] returns the number of variables involved in [ne] *)
val dimension : t -> int

(** [split ne] returns a monome [(a,x)] of [ne] (where x is not "",
    namely a is not the constant of ne) as well as the rest of the
    expr, namely, [ne-a.x]. 

    If it is a monome over integers, we return such a couple iff a divide
    the rest.

    We suppose that the dimension is > 0.

*)
type split_res =
  | Split of string * Value.num * t
      (* i.e., for expressions over floats, or for simple integer expressions *)
  | No_solution (* e.g., "2.x + 3 = 0" have no solution for integers *)
  | Dont_know   (* e.g., "a.x + b.y = 0", for which it is difficult to 
                   find a substition for integers *)

val split : t -> split_res


(** [neg_nexpr ne] returns the opposite of [ne], namely, [-ne]. *)
val neg_nexpr : t -> t


(** Various operations over normal expression. *)
val opposite: t -> t
val add: t -> t -> t
val diff: t -> t -> t
val mult: t -> t -> t
val modulo: t -> t -> t
val div: t -> t -> t


(* The same as div, except that is raises an internal error if 
   the second arg does not divide the first.

   For internal use only.
*)
val quot: t -> t -> t

(**/**)

(** [fold f ne acc0] applies f to every monome of [ne]. *)
val fold : (string -> Value.num -> 'acc -> 'acc) -> t -> 'acc -> 'acc

(** [make x a] returns the normal expr made of the monome [a.x]. *)
val make : string -> Value.num -> t

(** [find var ne] returns the coef of the variable [var] in
  [ne], [None] if [var] is not in [ne]. *)
val find : string -> t -> Value.num option
val find_constant :t -> Value.num option


(** [nexpr_add (a, x) ne] returns [ne + a.x]. *)
val nexpr_add : (Value.num * string) -> t -> t


(** [apply_subst ne s] applies [s] to [ne]. *)
val apply_subst : t -> subst -> t
val apply_substl : subst list -> t -> t

(** [apply_simple_subst ne (x, v)] substitutes [x] by [v] in [ne]. *)
val apply_simple_subst : t -> string * Value.num -> t


(** returns the vars appearing in an normal expression *)
val get_vars : t -> string list


(** Pretty printing. *)
val substl_to_string : subst list -> string
val to_string_gen : (Value.num -> string) -> string -> t -> string
val to_string : t -> string
val print : t -> unit
val eval : t -> Var.num_subst list -> Value.num

val to_expr: t -> Expr.t

(* test is a non-empty ne.t is an int *)
val is_int: t -> bool
