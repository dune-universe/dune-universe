(** Geometric formulas *)

open Sym

type term =
  | C of sym                    (** Structure constant *)
  | V of sym                    (** Variable *)
  | F of sym * term list        (** Function application *)

(** Is term an application of a function? *)
val funapp : term -> bool

(** An atomic formula *)
type atom =
  | P of sym * term list        (** Predicate applied to terms *)
  | E of term * term            (** Equation *)
  | U of term                   (** Universe unary predicate *)

(** A conjunction of atomic formulas *)
type conj = And of atom list

(** A conjunction and the variables quantified.  For the antecedent,
   the quantified variables are the ones that occur in the
   conjunction.  For a disjunct in the conclusion, the quantified
   variables are the ones that occur in the conjunction but do not
   occur in the antecedent. *)
type quant = {
    vars : sym list;
    form : conj;
  }

(** A formula in geometric form *)
type form = {
    pos : Lexing.position;      (** Position of [=>] in input file *)
    tag : int;                  (** Number that identifies formula *)
    antec : quant;              (** Antecedent *)
    concl : quant list;         (** Consequent *)
    mutable enabled : bool; (** Is rule enabled?  Used by {!Solve.solve} *)
  }

(** A list of formulas and their constants *)
type axioms = {
    consts : sym list;
    forms : form list;
  }

(** Return the free variables in a conjunction *)
val free_vars : conj -> sym list

(** Make a formula *)
val mk_form : Lexing.position -> int -> conj -> conj list -> form

(** Does a formula have an empty antecedent? *)
val no_antec : form -> bool

(** Does a formula have at most one disjunct and no existentally
   quantified variables? *)
val is_lightweight : form -> bool

type subst = (sym * term) list

(** Is symbol in the domain of the substitution? *)
val in_dom : sym -> subst -> bool

(** Apply substitution to a conjection of atomic formulas. *)
val subst_conj : subst -> conj -> conj

(** Make a set axioms (a theory). *)
val mk_axioms : form list -> axioms
