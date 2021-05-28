## To Do

* Can we require an appropriate version of `monolith`? How?
  (Sek itself does not depend on Monolith.)

* Test `E.to_seq` and `Iter.to_seq`, with invalidation in mind.

* Test `Emulated.List`.

* Test `get_segment` and friends by viewing a read-only segment as an abstract
  type. Its `check` function could check that the candidate array segment is
  well-formed, nonempty, and that its content matches the reference segment.

* Test `get_writable_segment` and its variants.
  Declare a writable array segment to be an abstract type
  and equip it with `read` and `write` operations.

* Test that every operation raises `Invalid_argument` when
  invoked with invalid arguments?
