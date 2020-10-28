## To do

### Boring

* Write a summary of the point that each demo illustrates.

* Grep for `TODO` in `demos/working/map`.

### Short Term

* Proof-read the integer generation functions in `Gen`
  and check if they are correct and complete.
  Think about overflows.
  If possible, remove the restrictive assertions in
  `semi_open_interval` and `closed_interval`.

* Because `deconstruct` is not tail-recursive, deconstructing a large
  value, such as a long list, can fail with a stack overflow.

* `sec/Engine.ml` still has a `TODO` in the treatment of `SpecNondet`.

* If Monolith itself crashes with a `Stack_overflow` exception, or
  any other exception, then the backtrace is printed to the standard
  error channel and is not shown by `make show`.

* Define a `pick` or `forall` combinator that generates a value and
  lets the rest of the spec depend on it.

* Do something about arrays. If we are talking immutable arrays, then it is
  good enough to directly declare them as constructible/deconstructible. If
  we are talking mutable arrays, define `declare_mutable_array` which views
  arrays views them as an abstract type equipped with `make/get/set`.

* Measure the performance impact of `normalize`,
  to see whether it is worth fighting to get rid of it.

* Move to static checking of (de)constructibility.

* If a (sub)pattern does not bind any names and does not cause a failure, then
  it could be replaced with a wildcard pattern. (Less informative, but
  possibly much more compact.)

### Longer Term

* In `make random`, after a bug is found, could we reduce the search space to
  the subset of operations that were used in this scenario?

* In `make test`, should we at the beginning offer the fuzzer an opportunity
  to choose a subset of the operations that should be tested?

* Investigate the idea of getting rid of the need for `Tag.equal` by storing
  the environment, in a decentralized manner, as one list of variable-value
  pairs per abstract type.

* Define a combinator that defines a new type as isomorphic to an existing
  type. (Use `map_into` and `map_outof` and `ifpol` to convert both ways.)
  Use it e.g. to define triples.

* Introduce a way of declaring that an operation returns a *preexisting*
  value of an abstract data type. Instead of recording a new dual value
  in the environment, Monolith would check that this dual value already
  exists in the environment. Use this feature to specify the `sequence`
  operation on Sek iterators.

* Work on minimizing (shrinking) scenarios.

* Try suppressing the recording of a trace (the construction of syntax)
  and see if this increases performance (under Linux). If so, then
  recording a trace and printing a scenario should be optional (and
  would be enabled by `make show` but not by `make test`).

* Make it easy to use memoization to manufacture functions that have
  extensional behavior? Dually, make it easy to verify that a function
  has extensional behavior (wrap it in a tester that uses memoization).

* Think about generating functions, not by generating their code ahead
  of time, but by simulating their behavior *when they are invoked*.
  That would allow us to retain the key aspects of our current engine.
  That said, we would have to work with a goal type, and it is not
  clear how to achieve the goal, with a limited amount of fuel, and
  without backtracking (which is not possible, as we cannot undo the
  side effects of an operation that we have already executed).
