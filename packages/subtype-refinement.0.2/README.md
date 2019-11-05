# subtype-refinement

Refinement types encoded with private types in OCaml.

[![Build Status](https://travis-ci.org/marcoonroad/subtype-refinement.svg?branch=master)](https://travis-ci.org/marcoonroad/subtype-refinement)


## Installation

Production/release version:
```
$ opam install subtype-refinement
```

Development/snapshot version (on this project directory):
```
$ opam install .
```


## Usage

This package provides statically checked refinement types, but casts into such refined types
are deferred until runtime (this is due the use of functions as constraints, so these constraints
must be evaluated before they are applied to "type-check" some value). By "statically checked", I mean that this
library generates fresh types for constraints of type `t -> bool`, where `t` is the type we are going
to refine. This refinement, so, produces a subtype for such abstract type `t`. We can anyways inject this abstract
type `t` as it were a type class (that is, applying a functor), and indeed it carries with itself a constraint operation
(being `t -> bool`). The result fresh "subtype" is wrapped inside a result module, and the functor used to produce it is seen
as a Dependent Pi Type. Such result module will have the following result type:

```ocaml
module Result : sig
  type super
  type t = private super

  exception FailedDownCast of super

  val downcast : super -> t
  val upcast   : t -> super
end;;
```

Where `super` is the previously injected type `t` (don't confuse with this `t` type, which is the fresh subtype). As you can notice
easily, the sole exception in this library occurs while downcasting (honestly, when the constraint returns `false`). On the other
hand, our local type class will have the following signature:

```ocaml
module TypeClass : sig
  type t

  val where : t -> bool
end;;
```

Where `where` is the refinement constraint carried together. To apply the refinement functor, we will rather use the pattern below (assuming
that you have installed this library, it is required as `"subtype-refinement"` and provides the `Subtype_refinement` module):

```ocaml
open Subtype_refinement;;

module Result = Refine (TypeClass);;
```

The fresh subtype is somewhat opaque, it doesn't inherit operations from its supertype (it's really hard, anyways, to know which kind of
operations preserve the constraint invariants, for example, if the constraint refines integers into naturals through `fun x -> x >= 0`,
the subtraction operation will need further constraints for naturals such `fun (x, y) -> x >= y`, where `(x, y)` is the pair which we are going
to perform subtraction). Said that, the only valid operations for this subtype are the casts, more specifically, upcasts and downcasts. So, to
use super type's operations into this subtype, we need to perform casts to ensure the preservation of invariants (it would be really nice if
implicits casts were provided in OCaml as they're provided in Scala).

Singletons are degenerate cases of refinements where the constraint compares against a known value. If this comparison succeeds, we certainly
have a singleton value/"subtype" and we can have the cookie & the cake as well (that is, we can have nice things). To use singletons directly
without doing such comparison, the type class must be somehow modified in something like that below:

```ocaml
module SingletonTypeClass : sig
  type t

  val value : t
end;;
```

Where `value` is our known value to compare against. A different functor for singletons is also provided (but the result module's signature is
the same):

```ocaml
open Subtype_refinement;;

module SingletonResult = Singleton (SingletonTypeClass);;
```

'Cause OCaml provides first-class modules through explicit packing/unpacking, a shorter version of the refinement functor is provided as:

```ocaml
open Subtype_refinement;;

let result = refine constraint;;
```

To unpack it, we just need to say:

```ocaml
module Module = (val result : Subtype with type super = t);;
```

Where `t` is the type used in the constraint. The short counterpart of singleton refinements is not provided due my laziness, so maybe
tomorrow I'll find my way home.


## Contributors

- [zapashcanon](https://github.com/zapashcanon)

