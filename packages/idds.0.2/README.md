# IDDs: Identity-suppressed Decision Diagrams [![Build Status](https://travis-ci.org/netkat-lang/idds.svg?branch=master)](https://travis-ci.org/netkat-lang/idds)
This package implements hash-consed binary decision diagrams (BDDs) and identity-suppressed decision diagrams (IDDs).
[The API is documented here](https://netkat-lang.github.io/idds/).
## Overview

An IDD, like a BBD, can be seen as representing a transition relation R on a state space of boolean vectors. I.e. boolean vector pair (v1, v2) belongs to R if and only if evaluating the IDD-representation of R in the environment given by (v1, v2) yields true.

The main motivation for IDDs is that they represent the identity relation in a constant amount of space instead of in an amount of space that is linear in the size of the boolean vectors.

## Provided operations
### BDDs
* Constructors: true, false, if-then-else
* Operations: equality, negation, disjunction, conjunction

### IDDs
* Constructors: identity relation, empty relation, test, set, branch
* Operations: equality, subset test, apply algorithm, union, sequential composition, transitive-reflexive closure

## Example session
Assuming you have utop installed like so,
```
opam install utop
```
you can start an interactive session by executing the following from the root directory of the project:
```
dune utop .
```

You can then experiment with the library in utop and render decision diagrams to inspect them, provided you have `graphviz` installed.
```
open Idd_;;

(* DDs *)
let mgr = Dd.manager ();;
let d0 = Dd.branch mgr (Var.out 1) Dd.ctrue Dd.cfalse;;
Dd.render d0;;
let d1 = Dd.branch mgr (Var.inp 1) d0 d0;;
Dd.render d1;;

(* IDDs *)
let mgr = Idd.manager();;
Dd.render (Idd.test mgr 3 true :> Dd.t);;
Dd.render (Idd.set mgr 3 true :> Dd.t);;
let flip = Idd.(union mgr (seq mgr (test mgr 3 true) (set mgr 3 false))
                          (seq mgr (test mgr 3 false) (set mgr 3 true)));;
Dd.render (flip :> Dd.t)
```

## Build and test
To build from source execute
```
dune build
```
To run all tests execute
```
dune runtest
```
