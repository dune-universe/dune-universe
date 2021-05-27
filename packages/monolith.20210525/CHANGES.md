# Changes

## 2021/05/25

* Fix a bug where the exception `PleaseBackOff` was unintendedly caught by the
  combinator `^!>` and therefore did not work as intended. (Reported and fixed
  by Nicolas Osborne.)

## 2020/10/26

* The functions `declare_concrete_type`, `&&&` and `&&@` disappear.
  They are replaced with two new combinators, `constructible` and `deconstructible`.
  The combinator `ifpol` (described further on) can be used to construct a
  specification that is both constructible and deconstructible.
  The idiom `int &&& range` is replaced with `int_within range`.

* The function `declare_abstract_type` no longer requires the type's name
  as an argument; it takes a unit argument instead.

* The arrow combinator `^^>` is renamed `^>`.
  The dependent arrow combinator `^&>` is renamed `^>>`.

* New specification combinator `^!>`. This combinator describes a function
  that can raise an exception, and requires the reference implementation
  and the candidate implementation to raise exactly the same exception.

* New specification combinator `^?>`. This combinator describes a
  nondeterministic function. `spec1 ^?> spec2` is a short-hand for
  `spec1 ^> nondet spec2`.

* New specification combinator `^!?>`. This combinator describes a function
  that can raise an exception, and can decide in a nondeterministic way
  whether it wishes to raise an exception and what exception it wishes to
  raise. The reference implementation is allowed to observe this behavior
  and to react accordingly.

* New specification combinators `^!>>`, `^?>>`, and `^!?>>`. These are
  the dependent variants of the previous three combinators.

* New specification combinators `option`, `result`, and `list`. These
  combinators make it easy to describe an operation that expects or returns an
  option, a result, or a list.

* New  specification combinator `declare_seq`. This (effectful) combinator
  makes it relatively easy to describe an operation that expects or returns
  a sequence.

* New specification combinator `map_into`. This combinator is typically
  used to wrap an operation or to transform the result of an operation.

* New specification combinator `map_outof`. This combinator is typically
  used to construct an argument to an operation via a transformation.

* Removed the combinator `map`, whose type was fishy.

* New specification combinators `rot2` and `rot3`. (`rot2` is also known as
  `flip`.) These combinators move a distant argument to the front. This is
  useful when a dependency between arguments runs contrary to the order of
  the arguments. These combinators can be used only in a positive position.

* New specification combinators `curry` and `uncurry`. These combinators
  transform a function that expects a pair into a function that expects
  two separate arguments, and vice-versa. These combinators can be used
  only in a positive position.

* New specification combinator `ignored`. This combinator is used to describe
  the output (or part of the output) of an operation, and indicates that
  this output should be ignored.

* Changed the type of the specification combinator `nondet`.
  Removed the limitation that this combinator can be applied only to a
  concrete type.

* New specification constant `exn`. The new function `override_exn_eq`
  allows overriding the notion of equality that is associated with the
  concrete type `exn`. This is useful when the reference implementation
  and candidate implementation raise different exceptions.

* New specification combinator `ifpol`, which allows distinguishing between
  negative and positive occurrences in a specification. This low-level
  combinator can be useful in the definition of higher-level abstractions.

* New specification combinator `fix`, which allows building recursive
  specifications.

* New specification combinator `abstract`, which declares an abstract type
  on the fly, together with a conversion operation out of this abstract type
  to its concrete definition. This can be used to deal with functions that
  return functions.

* New exception `PleaseBackOff`, which the reference implementation is allowed
  to raise when an operation (or some particular case of an operation) is
  illegal or is not implemented. This causes the Monolith engine to silently
  ignore this scenario. No side effects must be performed by the reference
  implementation before raising this exception.

* New exception `Unimplemented`, which the candidate implementation is allowed
  to raise when an operation (or some particular case of an operation) is not
  implemented. This causes the Monolith engine to silently ignore this
  scenario.

* New definition of the type `'a code` as a synonym for `'a * appearance`. New
  constructors for the type `appearance`, including `constant`, `document`,
  and `infix`.

* The functions `interval` and `interval_` are renamed
  `semi_open_interval` and `closed_interval`.

* Significant internal changes, leading to simpler and possibly faster
  code.

* The file `Makefile.monolith` is now installed with the library, so it need
  no longer be copied into one's project, unless one wishes to adapt it.

* In `Makefile.monolith`, new entries `make multicore` and `make tmux`, both
  of which implement parallel fuzzing with multiple `afl-fuzz` processes.

* In `Makefile.monolith`, new entry `make random`, which implements random
  testing.

* On 64-bit machines, the function `bits` did not generate the full range of
  63-bit integers; it was inadvertently limited to 32-bit integers. A number
  of other functions that depend on it, such as `interval`, were also wrong.
  Fixed (hopefully).

## 2020/06/09

* Initial release.
