# Implementation Notes

## Implementation of Ephemeral Sequences

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
