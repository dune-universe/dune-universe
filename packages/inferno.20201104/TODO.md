* Use `odoc` to publish a documentation.

* Offer a combinator `decode` of type `variable -> ty co` which requests
  the decoding of a type variable. If we have this, then we can remove
  or simplify the combinators that perform decoding. E.g., `exist` can
  be removed, and `exist_` can be renamed to `exist`. Same thing for
  `construct` and `construct_`. (I don't see how to simplify `instance`.)

* Ask the user to provide a `traverse` function
  (over the CPS applicative)
  instead of (in addition to)
  `map`, `fold`, `iter`?

* Document the requirement that the function passed to `map` must be pure.
  Use this hypothesis to remove the useless evaluation of `k1 env` in the
  definition of `^^`.

* Think about the treatment of unreachable type variables. Could it
  be simplified? Do we really need a let form that binds several names at once?

* The unifier could be more abstract: it doesn't need to know that the ranks
  are integers, does it? It could be just (abstract type of ranks, abstract
  min function). But then, why not merge the structure and rank fields
  (again)? Maybe because the unifier needs to know the difference between no
  structure and some structure.

* Think about type error reporting.
  - Keep track of type variable names.
  - Possibly keep track of source code ranges in the type structure
    (not just in the constraints)?
  - Possibly implement several solvers with different strategies.
  - Possibly continue after a unification error (just not unifying this
    particular pair of types) and report a list of mismatches at the end.
  - Compute a minimal type error slice?
  - Of course let-polymorphism makes this more difficult.
    Could we naively expand every (unannotated) let construct
    so as to eliminate this difficulty?
  - Is it conceivable to solve a constraint twice using two distinct
    solvers? That might involve removing all of the mutable state
    that explicitly hangs off a constraint.

* Add a proper test suite, including positive and negative tests!

* Possible extensions:
  - support universal quantification
  - support rows
  - support the relaxed value restriction

* Note that although decoded types cannot be obtained until the whole constraint
  solving process is over, some information could be obtained earlier (e.g.,
  where the type abstractions and type applications are placed, as it becomes
  available during the run of the solver). Not sure if this is important, though.
