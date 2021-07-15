(* Time-stamp: <modified the 12/02/2013 (at 18:07) by Erwan Jahier> *)

(** Static evaluation of types. *)
 
(** Evaluates the type of an expression, and return a val_exp with
    its "type_" field updated (and ditto for the type of its sub expr).
    
Modif 12/07 :
- Travaille au niveau Lic. donc on vire IdSolver.t qui
  travaille au niveau syntaxique

*)
val f : IdSolver.t -> Lic.val_exp -> Lic.val_exp * Lic.type_ list

