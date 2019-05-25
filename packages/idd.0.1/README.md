# Overview
This package implements hash-consed binary decision diagrams (BDDs) and identity-suppressed decision diagrams (IDDs).

An IDD, like a BBD, can be seen as representing a transition relation R on a state space of boolean vectors. I.e. boolean vector pair (v1, v2) belongs to R if and only if evaluating the IDD-representation of R in the environment given by (v1, v2) yields true.

The main motivation for IDDs is that they represent the identity relation in a constant amount of space instead of in an amount of space that is linear in the size of the boolean vectors.

# Provided operations
## BDDs
* Constructors: true, false, if-then-else
* Operations: equality, negation, disjunction, conjunction

## IDDs
* Constructors: identity relation, empty relation, test, set, branch
* Operations: equality, subset test, apply algorithm, union, sequential composition, transitive-reflexive closure


