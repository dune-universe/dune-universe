
(* Try to explain why a constraint is not satisfiable by providing a smaller
   sub-expression of the expression encoded in the bdd.  

   Such an expressions can be unsatisfiable for :

   - a pure Boolean reason, in which case we provide a (minimal)
   expression representing the list of monomes that makes it false);
   - Or there do exist Boolean solution, but there do not hold to
   satisfiable numeric constraint, in which case we provide an
   expression representing the list of monomes that makes its true *)

(* [get_info bdd bdd1 (bdd2,e2)] tries to explain as concisely as
   possible why bdd is not satisfiable

   hyp: 
   - bbd1 has sol
   - bbd2 has sol
   - bdd=bdd1^bdd2 has no sol 
   - bdd2=to_bdd(e2).
*)
val get_info : Bddd.t -> Bdd.t -> Bdd.t ->  (Expr.t * Bdd.t) -> Expr.t
