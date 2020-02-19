open Base

type test =
  (* [Interval (v, a, b)] tests whether [v] satisfies [a<=v<=b] where [a], [b],
     and [v] are interpreted as integers *)
  | Interval of string * Bitstring.t * Bitstring.t
  (* [Test (v, z, n)] tests that [v_i=0] for all i such that [z_i=1] and that 
     [v_i=1] for all i such that [n_i=1] *)
  | Test of string * Bitstring.t * Bitstring.t

(* [act (v, z, n)] is a vector [v'] such that [v'_i=0] for all i such that 
   [z_i=1] and [v_i=1] for all i such that [n_i=1] *)
and act = string * Bitstring.t * Bitstring.t
and bexp = test Kat.Ast.bexp
and exp = (act, test) Kat.Ast.exp 
   [@@deriving sexp]
