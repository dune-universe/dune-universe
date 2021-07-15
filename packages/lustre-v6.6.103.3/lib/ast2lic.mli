(* Time-stamp: <modified the 12/02/2013 (at 18:06) by Erwan Jahier> *)


(** Translate Ast to lic. 

    Basically, it 

    - calls [EvalConst.f] wherever it is necessary, i.e., for items
    that are required to be static, such as arrays sizes, or array
    indexes.  
    - recursively calls itself for translating sub-terms
    - checks the arguments and the parameters are compatible (i.e., that they unify)
*)

val of_type  : IdSolver.t -> AstCore.type_exp  -> Lic.type_
val of_clock : IdSolver.t -> AstCore.var_info -> Lic.id_clock

(**
   A [node_exp] is a name plus a list of static arguments.

   The goal of [node] is to 
   - compute the effective type of static arguments
   - check they are compatible with the node signature 
   check the type of the static arguments (
*)
val of_node : IdSolver.t -> AstCore.node_exp Lxm.srcflagged -> 
  Lic.node_exp

val of_eq : IdSolver.t -> AstCore.eq_info Lxm.srcflagged -> 
  Lic.eq_info Lxm.srcflagged

val of_assertion : IdSolver.t -> AstCore.val_exp Lxm.srcflagged -> 
  Lic.val_exp Lxm.srcflagged
