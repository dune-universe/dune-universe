(* Time-stamp: <modified the 29/08/2019 (at 14:58) by Erwan Jahier> *)

(** Performs static evaluations of predefined operators in type expressions *)

type typer = Lic.type_ AstPredef.evaluator

exception EvalType_error of string
(* val type_error : Lic.type_ list -> string -> 'a *)
val raise_arity_error : string -> int -> int -> 'b

val raise_type_error : Lic.type_ list -> Lic.type_ list -> string -> 'a

(** Provides the type profile of predef operators. More precisely,
    given an operator and a list of types, This function checks that
    the provided types are ok, and returns the list of the operator
    output types.
*)
val f : IdSolver.t -> AstPredef.op -> Lxm.t -> typer


(* Does not work for NOR_n and DIESE_n! *)
(** PIS ALLER : 2 versions
   - une pour les macros, qui nécessite un IdSolver.t pour traiter les Lic.static_arg list
   - l'autre pour les noeuds simple qui peut être utilisée statiquement
*)
(* [make_node_exp_eff id_solver has_mem is_safe op size lxm]  *)
val make_node_exp_eff : 
  IdSolver.t -> bool option -> bool -> AstPredef.op -> Lxm.t -> Lic.node_exp

val make_simple_node_exp_eff : 
  bool option -> bool -> AstPredef.op -> Lxm.t -> Lic.node_exp
