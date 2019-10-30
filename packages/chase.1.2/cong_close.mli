(** Congruence closure

    A congruence relation is a monotonic equivalence class over ground
    terms.  A binary relation R is monotonic if (f(s1,...,sn),
    f(t1,...,tn)) is in R when (si, ti) is in R for i=1 to n.  Given a
    set of equations E, its congruence closure is the smallest
    conguence relation containing E.

    The module implements the Nelson-Oppen algorithm for congruence
    closure as reported in "Fast Decision Procedures Based on
    Congruence Closure", in the Journal of the Association for Computing
    Machinery, Vol. 27, No. 2, April 1980, pp 356-364.

 *)

open Sym

(** Terms are parameterized by its function symbol. *)
type term

(** [mk_term f ts] constructs a term with function
    symbol [f] and arguments [ts]. *)
val mk_term: sym -> term list -> term

val term_func: term -> sym

val term_args: term -> term list

(** An equivalence class of terms. *)
type equiv

(** The table used to translate terms to equivalence classes *)
type tbl

(** [mk_tbl ()] makes an empty table. *)
val mk_tbl: unit -> tbl

(** [intern tbl t] uses table [tbl] to add [t] to the equivalence
    relation and returns its associated equivalence class.  *)
val intern: tbl -> term -> equiv

(** [term c] returns the representative term associated with an
   equivalence class [c].  It chooses the symbol with the least
   number as the a class' canonical representative whenever possible.
   *)
val term: equiv -> term

(** Are two equivalence classes the same equivalence class? *)
val same_class: equiv -> equiv -> bool

(** Merge two equivalence classes.  Performs congruence closure. *)
val merge: equiv -> equiv -> unit
