open Base

type 'test bexp =
  | True
  | False
  | Test of 'test
  | Disj of ('test bexp) * ('test bexp)
  | Conj of ('test bexp) * ('test bexp)
  | Neg of 'test bexp
  [@@deriving sexp, compare, equal, hash]

type ('act, 'test) exp =
  | Assert of 'test bexp
  | Action of 'act
  | Union of ('act, 'test) exp * ('act, 'test) exp
  | Seq of ('act, 'test) exp * ('act, 'test) exp
  | Star of ('act, 'test) exp
  [@@deriving sexp, compare, equal, hash]


module Nary = struct
  type ('act, 'test) exp =
    | Assert of 'test bexp
    | Action of 'act
    | Union of ('act, 'test) exp list
    | Seq of ('act, 'test) exp list
    | Star of ('act, 'test) exp
    [@@deriving sexp, compare, equal, hash]
end
