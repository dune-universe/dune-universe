(** Generic AST for Kleene Algebra with Tests (KAT). *)


(** {1 Expressions} *)

(** Boolean expressions over primitive tests ['test]. *)
type 'test bexp =
  | True
  | False
  | Test of 'test
  | Disj of ('test bexp) * ('test bexp)
  | Conj of ('test bexp) * ('test bexp)
  | Neg of 'test bexp

(** KAT expressions over actions ['act] and primitive tests ['test]. *)
and ('act, 'test) exp =
  | Assert of 'test bexp
  | Action of 'act
  | Union of ('act, 'test) exp * ('act, 'test) exp
  | Seq of ('act, 'test) exp * ('act, 'test) exp
  | Star of ('act, 'test) exp
  [@@deriving sexp, compare, equal, hash]


(** N-ary expressions (modulo associativity). *)
module Nary : sig

  type ('act, 'test) exp =
    | Assert of 'test bexp
    | Action of 'act
    | Union of ('act, 'test) exp list
    | Seq of ('act, 'test) exp list
    | Star of ('act, 'test) exp
    [@@deriving sexp, compare, equal, hash]
  (** [Seq []] and [Union []] are equivalent to [Assert True] and
      [Assert False], respectively. *)

end

