(* Time-stamp: <modified the 29/08/2019 (at 14:59) by Erwan Jahier> *)

(** Performs static evaluations of predefined operators in constant expressions *)

exception EvalConst_error of string

val type_error_const : Lic.const list -> string -> 'a
val arity_error_const : Lic.const list -> string -> 'a

type const_evaluator = Lic.const AstPredef.evaluator

(* That function says how to statically evaluate constants *)
val f: IdSolver.t -> AstPredef.op -> Lxm.t -> Lic.static_arg list -> const_evaluator

