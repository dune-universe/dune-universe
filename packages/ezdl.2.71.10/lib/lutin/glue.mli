
(*
Glue between lutin (CoAlgExp, CoIdent etc) and lucky ( Type Var Exp)

Necessary to reuse the lucky solver
*)

(* Conversion type lutin -> type lucky *)
val lucky_type_of : CkTypeEff.t  -> Type.t

(* lucky Exp.t utils *)

val lucky_exp_zero : Exp.t
val lucky_exp_of_value : Value.t -> Exp.t

(* lucky var to its reference exp *)
val lucky_exp_var_ref : Exp.var -> Exp.t

(* translators CoAlgExp to Exp take as argument
   the function in charge of translating ident refs:
   the argument MUST be a id ref (AE_pre, AE_support, AE_alias)
   first arg true -> partial eval
*)
type id2exp = bool -> CoAlgExp.node -> Exp.t

(* lutin support var to lucky var *)
val lucky_var_of : id2exp -> Expand.support_info -> Exp.var 


(* translator lutin exp -> lucky exp *) 
val lucky_exp_of : bool -> id2exp -> CoAlgExp.t -> Exp.t


(* val check_satisfiablity : Exp.formula -> bool *)
