# Tyabt

Tyabt is an OCaml implementation of many-sorted abstract binding trees.
Abstract binding trees (ABTs) are similar to abstract syntax trees, but also
keep track of variable scopes. Many-sorted ABTs support multiple syntactic
classes, known as sorts. This library uses GADTs and phantom types to
statically ensure that only syntactically valid ABTs are representable.

ABTs are described in Robert Harper's textbook, [Practical Foundations for
Programming Languages](https://www.cs.cmu.edu/~rwh/pfpl/index.html), and are
inspired by the notation of the [Nuprl proof development system](
http://www.nuprl.org/).
