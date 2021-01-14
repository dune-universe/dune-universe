
(*
copyright (c) 2013-2014, simon cruanes
all rights reserved.

redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are met:

redistributions of source code must retain the above copyright notice, this
list of conditions and the following disclaimer.  redistributions in binary
form must reproduce the above copyright notice, this list of conditions and the
following disclaimer in the documentation and/or other materials provided with
the distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE
FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR
SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER
CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*)

(** {1 Bindings to Quantor} *)

type 'a printer = Format.formatter -> 'a -> unit
type 'a sequence = ('a -> unit) -> unit

type assignment =
  | True
  | False
  | Undef

val pp_assignment : assignment printer

type quantifier =
  | Forall
  | Exists

val pp_quantifier : quantifier printer

(** {2 a QBF literal} *)
module Lit : sig
  type t = private int
  (** A boolean literal is only a non-zero integer *)

  val make : int -> t
  (** Build a literal out of an integer.
      @raise Invalid_argument if the integer is zero *)

  val neg : t -> t
  (** Negation (i.e. opposite) *)

  val to_int : t -> int
  (** The underlying atom, or name (strictly positive integer) *)

  val abs : t -> t  (** Remove sign *)
  val sign : t -> bool (** Get sign *)
  val apply_sign : bool -> t -> t (** Swap sign iff the bool is false *)
  val set_sign : bool -> t -> t  (** Positive if true, negative otherwise *)

  val fresh : unit -> t
  (** Generator for fresh, unique lits *)

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int
  val hash_fun : t -> int64 -> int64
  val print : Format.formatter -> t -> unit
end

(** {2 A Boolean Formula in CNF} *)
module CNF : sig
  type clause = Lit.t list

  type t = clause list

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val print : t printer
  val print_with : pp_lit:Lit.t printer -> t printer
end

(** {2 A Quantified Boolean Formula in CNF} *)
module QCNF : sig
  type t = private
    | Quant of quantifier * Lit.t list * t
    | Prop of CNF.t

  val forall : Lit.t list -> t -> t
  val exists : Lit.t list -> t -> t
  val quantify : quantifier -> Lit.t list -> t -> t
  val prop : CNF.t -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val print : t printer
  val print_with : pp_lit:Lit.t printer -> t printer
end

(** {2 a QBF formula}

The formula must already be prenex, i.e. it should be nested quantifiers
with a quantifier-free formula inside. *)
module Formula : sig
  type t = private
    | And of t list
    | Or of t list
    | Imply of t * t
    | XOr of t list  (** exactly one element in the list is true *)
    | Equiv of t list (** all the elements are true, or all
                              of them are false *)
    | True
    | False
    | Not of t
    | Atom of Lit.t

  val true_ : t
  val false_ : t
  val and_l : t list -> t
  val and_seq : t sequence -> t
  val or_l : t list -> t
  val or_seq : t sequence -> t
  val xor_l : t list -> t
  val equiv_l : t list -> t
  val imply : t -> t -> t
  val atom : Lit.t -> t
  val neg : t -> t

  val and_map : f:('a -> t) -> 'a sequence -> t
  (** [and_map f seq] computes [f x] for each [x] in [seq], and joins
      the resulting formulas with "and" *)

  val or_map : f:('a -> t) -> 'a sequence -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val print : t printer
  val print_with : pp_lit:Lit.t printer -> t printer

  val simplify : t -> t
  (** Simplifications *)

  val cnf : ?gensym:(unit -> Lit.t) -> t -> CNF.t * Lit.t list
  (** Convert the formula into a prenex-clausal normal form. This can use
      some Tseitin conversion, introducing new literals, to avoid the
      exponential blowup that can sometimes occur.
      @return a pair of the CNF, and the list of newly created literals
      @param gensym a way to generate new literals to avoid exponential
        blowup. Default is {!Lit.fresh}. *)
end

module QFormula : sig
  type t = private
    | Quant of quantifier * Lit.t list * t
    | Prop of Formula.t

  val forall : Lit.t list -> t -> t
  val exists : Lit.t list -> t -> t
  val quantify : quantifier -> Lit.t list -> t -> t
  val prop : Formula.t -> t

  val equal : t -> t -> bool
  val compare : t -> t -> int
  val hash : t -> int

  val print : t printer
  val print_with : pp_lit:Lit.t printer -> t printer

  val simplify : t -> t
  (** Simplifications *)

  val cnf : ?gensym:(unit -> Lit.t) -> t -> QCNF.t
  (** Same as {!CNF.cnf}, but newly created literals are quantified
      in an innermost existential scope *)
end

(** {2 Solvers} *)

type result =
  | Unknown
  | Sat of (Lit.t -> assignment)
  | Unsat
  | Timeout
  | Spaceout

val pp_result : result printer

type solver = {
  name : string;
  solve : QCNF.t -> result;
}

val solve : solver:solver -> QCNF.t -> result
(** Check whether the CNF formula is true (satisfiable) or false
    using the given solver *)
