(** Flattening tranformation *)

(** On input, the flattener expects a formula in which two
   existentially quantified variables are never equated.  The
   {!module:Check} module ensures this. *)

(** On output, the flattener produces a logically equivalent formula in
   which atomic formulas have one of three forms:
   - P(X1, ... , Xn) where P is a predicate symbol
                   and X1, ... , Xn are variables.
   - f(X1, ... , Xn) = X0, where f is a function symbol
                         and X0, ... , Xn are variables.
   - X = Y where X and Y are different variables that occur in the
         antecedent, and this equation occurs in the conclusion.

   Whenever a variable is eliminated in favor of another, the variable
   with the largest integer is eliminated.
 *)

(** Flatten formula *)
val flatten : Formula.form ->  Formula.form
