## To Do

* Test `Emulated.List`.

* Test `get_writable_segment` and its variants.
  (Not easy; may require extensions to Monolith.)

* For operations that may raise an exception, try working
  with a single spec (wrapping the result using `map`).
  This requires Monolith to offer an `option` combinator.

* Test that every operation raises `Invalid_argument` when
  invoked with invalid arguments?
