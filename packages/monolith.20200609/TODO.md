## To do

### Fixes

* Test `make test` under Linux.

### Comfort Features and Cleanup

* Write a tutorial, and make it a blog post as well.

* Add generators and printers for `char`, `string`, `array`, if needed.

* Can we offer `option`, `list`, `array` at a type of the form `spec -> spec`,
  while restricting these combinators to concrete types?

* In the tutorial, explain how to deal with higher-order functions.
  Functions passed as arguments can be generated (the `sort` demo
  gives an example) or a fixed function can be used (a harness can
  be written, or the `map` combinator can be used). When a function
  is returned as a result, the user can disguise it as an abstract
  type with an `apply`. Write a `memoize` demo.

### Extensions

* Allow `map` in negative position, so it can be used to transform an argument
  before it is passed to an operation. Use this to convert between nested
  pairs and triples, both ways, both in input and output positions.

* Add a `top` spec that works at all (pairs of) types and imposes no
  observation. Useful to ignore a result.

* Add support for constructing and deconstructing sum types (e.g., `option`,
  `result`). For deconstruction, it should suffice to delay the generation of
  the pattern until the moment where the candidate produces a value.

* Add support for testing a postcondition. (Allow using `%` in positive position.)
  (This could be used e.g. to indicate that it's OK to raise an exception
  when the input is zero but not otherwise. That would be more efficient
  than providing two separate specs, each of which would have a precondition?)

* Offer an option to equip an abstract type with a notion of identity.
  The library should then populate a (weak?) hash table with pairs of
  a reference address and a candidate address. This would allow checking
  that the result produced by an operation is correct up to physical
  identity. Use it to test the `sequence` operation on Sek iterators.

* Work on minimizing counter-examples.
