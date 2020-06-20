# Implementation Notes

## Destructive Operations on Ephemeral Sequences

Some operations, namely `append` and `destructive_snapshot`, destroy one of
their arguments. We have three options:

* Leave this data structure in an invalid state, and rely on the user to
  no longer use it. This is efficient but can lead to surprising behavior.
  There are in fact two variants of this approach:

  * with a dynamic check, which protects against the use of an invalid data
    structure (in that case, an exception is raised);

  * without a dynamic check (in that case, meaningless behavior can be
    observed).

* Reinitialize this data structure so that it represents a valid empty
  sequence. This is safe but inefficient; in particular, we pay the cost
  of allocating and initializing two chunks.

* Pretend that this data structure represents a valid empty sequence,
  but postpone reinitialization until the data structure is actually
  used (which may never happen). This is safe and reasonably efficient (we pay the cost of a dynamic test).
  It is in fact closely related to the first approach above, except that
  instead of raising an exception when an invalid sequence is used,
  we silently reinitialize this sequence.

We choose the last option, which allows us to publish a simple specification
and implement it efficiently.

One potential drawback of this option is that an invalid sequence can contain
outdated pointers and therefore can cause a memory leak.

## Binary Iteration Operations on Sequences

Several operations iterate over two sequences. These operations include
`iter2, `fold_left2`, `fold_right2`, `map2`, `for_all2`, `exists2`, and so on.
The question arises, how should these operations when the two sequences do not
have the same length?

* The functions in OCaml's standard library raise `Invalid_argument _`. This
  is the "strict" behavior. It is not entirely clear whether this means that
  the user is at fault (i.e., the precondition of the function was violated)
  or whether this is a normal situation that the user is allowed to exploit
  (i.e., the user is allowed to catch this exception). Thus, there are really
  two different plausible specifications for this behavior.

* One might argue that (at least in some situations) *not* raising an
  exception, and ignoring a suffix of the longer sequence, is the desired
  behavior. This is the "lax" behavior.

In total, we have three possible specifications. One might wonder how many
functions the library should expose, and with what specifications. We choose
the following route:

* Offer just one variant of each function, not more. The library has many
  functions already; we do not wish to multiply them needlessly.

* Offer the lax variant, because it offers a useful behavior that the strict
  variants do not offer (namely, stop at the end of the shortest sequence).
  We lose a feature of the strict variant (namely, the ability to detect a
  length mismatch), but this feature can be easily recovered by the client,
  since the `length` function has constant time complexity.

* Finally, we point out that, because our sequences offer efficient iterators,
  the functions `iter2` and friends are entirely unnecessary in principle; the
  user can reconstruct them (and many variants of them) if desired. So it is
  safe to err on the side of providing fewer functions, rather than too many.
