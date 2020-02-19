(** Smart constructors and algebraic optimization for KAT. *)

open Ast


(** {1 Constants}  *)

val ctrue : 'test bexp
val cfalse : 'test bexp

val skip : ('act, 'test) exp
val abort : ('act, 'test) exp


(** {1 (Optimizing) smart constructors}  *)

val test : 'test -> 'test bexp
val disj : 'test bexp -> 'test bexp -> 'test bexp
val conj : 'test bexp -> 'test bexp -> 'test bexp
val neg : 'test bexp -> 'test bexp

val assrt : 'test bexp -> ('act, 'test) exp
val action : 'act -> ('act, 'test) exp
val union : ('act, 'test) exp -> ('act, 'test) exp -> ('act, 'test) exp
val seq : ('act, 'test) exp -> ('act, 'test) exp -> ('act, 'test) exp
val star : ('act, 'test) exp -> ('act, 'test) exp

val ite : 'test bexp -> ('act, 'test) exp -> ('act, 'test) exp -> ('act, 'test) exp


(** {2 N-ary constructors} *)

val big_disj : 'test bexp list -> 'test bexp
val big_conj : 'test bexp list -> 'test bexp
val big_union : ('act, 'test) exp list -> ('act, 'test) exp
val big_seq : ('act, 'test) exp list -> ('act, 'test) exp


(** {1 Algebraic optimization}  *)

val optimize_bexp : ?negate:bool -> ?neg_test:('test -> 'test) -> 'test bexp -> 'test bexp
val optimize_exp : ?neg_test:('test -> 'test) -> ('act, 'test) exp -> ('act, 'test) exp


(** Given an [e: exp] in which union and seq associate strictly to the right,
    returns normalized expression [normalize_rassoc_exp e] such that:
    - union and seq associative strictly to the left
    - boolean expressions expand as far as possible in the following sense:
      - [assert b1; assert b2]  ~>  [assert (b1; b2)]
      - [assert b1 + assert b2]  ~>  [assert (b1 + b2)]

    @raise Invalid_argument if input expression is not strictly
      right-associative.
*)
val normalize_rassoc_exp : ('act, 'test) exp -> ('act, 'test) exp