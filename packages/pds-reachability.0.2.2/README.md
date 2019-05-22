# pds-reachability

This repository contains a library which can be used to perform *reachability queries* on large, abstractly-specified push-down systems.  (Reachability in a PDS is equivalent to reachability in a PDA.)  These reachability queries are performed *by empty stack*: rather than explicitly specifying accepting states, any state which can be reached with an empty stack is considered to accept.

The user of this library provides a basis module defining how transitions are computed from states and arriving stack elements; the library provides an interface by which the states reachable from a given start state and stack.  If the basis module's operations are computable and the state and stack grammars are finite, then reachability is decidable and so this library will always produce an answer in finite time.

For examples of use, please see the unit tests in the `test/test_reachability.ml` file.

## Installing

This library can be installed via [OPAM](http://opam.ocaml.org/).

## Building

This project uses [Dune](https://github.com/ocaml/dune) as a build tool.  After cloning this repository, it should be sufficient to perform the following steps:

  1. Install dependencies.

    `opam install jbuilder batteries jhupllib monadlib ocaml-monadic ppx_deriving ppx_deriving_yojson yojson`

  2. Build the project.

    `make && make test`

## Performance Foci

This library aims to answer reachability queries quickly even on very large push-down systems.  The following points were taken into consideration in pursuit of this goal:

  * The user specifies how transitions may be computed for each state and arriving stack element and generally does not add states or transitions directly.  The library will construct only the part of the PDS necessary to answer the presented reachability questions.

  * The transitions themselves are abstracted via "dynamic pop" functions.  A "dynamic pop" edge is permitted to compute how a transition should proceed based upon the top element of the stack.  This is useful, for instance, to encode a step which checks that the top element of the stack is not a particular value: rather than enumerating stack elements which may be popped and then re-pushed, a single dynamic pop can address all of these cases.

  * The reachability analysis can reuse the information it gathers over multiple queries.  The same query performed on the same structure twice will, the second time it is made, take on the order of constant time to answer.  Queries which rely upon similar proofs or otherwise overlap in their reachability closure can share work.
