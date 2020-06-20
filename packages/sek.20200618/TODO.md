## For the Next Release

## For Later Releases

* Optimize `flatten_map` to linear time by using a vector to push
  the first T elements, before switching to a different representation.
  Other functions, such as `filter`, might benefit from a similar trick.

* State a claim on the complexity of a series of push and pop operations,
  to formalize the idea that it is O(1) amortized. Also, verify in Coq
  that consecutive calls to `P.push` have amortized cost O(1).
  Likewise for `P.pop`.

* `init` is nice but forces us to work element per element.
  What is a good way of generalizing `init` to work on segments?
  A possibility would be to first create the target sequence using `make`,
  then blit data into it, segment by segment, using a generalized `blit`
  that allows transforming data on the fly (a combination of `blit` and
  `map`). Very little new code would be required. This method allocates
  no superfluous arrays.
  How would this method compare to the method(s) based on `push_segment`?

* Add a variant of `E.sub` that shares only the schunks of interest.
  Add a variant of `E.sub` that shares all schunks.
    (Currently named `_unchecked_sharing_sub` in the code.)
    (Use an optional `mode` parameter.)
  Same thing for `E.blit`, `E.fill`, `E.make`, etc.
  We need to be able to mark certain schunks as now-shared.

* `snapshot` could have a `~mode` parameter, as it can use either kind
  of `copy`. Its default value should be `Share`, I think, as the word
  "snapshot" implies a cheap copy.

* `edit` could (should?) also have a `~mode` parameter.

* The module `Emulated.Array` should use an optimized representation
  for short sequences. For arrays of size less than `capacity`, it
  is not hard to store data in an array of just the right size, in
  the front chunk; and use a dummy back chunk. All of this can be
  trivially implemented without polluting the code of ephemeral sequences.

* Benchmark and measure the cost of `check_iterator_validity`.
  Also, benchmark against a simpler scheme where we maintain
  just a nonnegative version number (no Boolean flag).

* Write a test to verify that every invalid argument to every public operation
  is properly rejected.

* Investigate the representation of chunks of pairs (resp. tuples) using
  two arrays `data1` and `data2` in the record implementing chunks.
  This would allow compact representation of sequences of pairs (or arrays)
  without numerous indirections. It might be able to typecheck the code
  without any hack: if the sequence functor can be made to depend on
  the chunk representation, then all we have to do is to implement an
  alternative chunk module, and all the rest would be reused.

* In `E.Iter`, do we need functions `sub` and `copy` that preserve
  the validity of the iterator?

* If it is known (somehow) that a sequence has unique ownership of
  all of its chunks, then we can guarantee that `set` does not
  invalidate any iterators. Otherwise, we are forced to advertise
  that `set` invalidates all iterators.

* I think that `unchecked_iter_fill` is not as efficient as it
  could be, because it modifies the middle sequence at every chunk,
  but in fact it would be possible using a split/concat to replace
  all the chunks from the middle sequence at once. If the option
  ````Share``` is used, the performance impact would be significant,
  because there is no need to fill individual arrays, so all the
  cost involved is associated with updating the middle sequence.
  Same idea for `blit`.

* Fuzzer: set up our own minimization algorithm.

* Implement in-place operations (`rev`, `map`, `filter`, `uniq`,
  `sort`, etc.) on ephemeral sequences. Place them in a submodule
  so as to obtain a uniform naming scheme?

* Optimize sort.

* Decide if we could be smarter than using a dummy `brand` in
  ShareableSequence and re-allocating a block with the correct
  `owners` information in PersistentSequence.
  (This approach offers the advantage of clarity!)

* Add support for `delete` and `insert` operations on iterators.

* Without taking the trouble of using the fuzzer to test the code
  in `Emulated`, we could perhaps develop a suite of `cram` tests.

* Create an iterator directly on a short sequence and avoid the
  indirection in ShortPersistentSequence.

* For a stack usage, i.e., a sequence accessed only at back side, the
  code can be tweaked so that there is no need to allocate the front
  chunk. Moreover, a few tests can be saved, e.g. in `is_empty`.
  The functor settings "back_access_only" should be added to control
  this behavior. At any time, the structure can be turned back into
  a normal structure by allocating a proper front chunk. We need to
  decide whether the front chunk would then be silently restored when
  needed, or whether we want to force the programmer to call an explicit
  conversion function to restore the front chunk of the sequence.

* Add a constructor `Single` to ShareableSequence.

* Document, dynamically-check, and exploit the invariant that an ephemeral
  sequence may or may not be owner of all of its schunks. Maybe add a field
  in the ephemeral sequence record to keep track of the fact that the user
  wishes to maintain this property, or (better?) just of the fact that this
  property is currently true or not. Maintaining the invariant on `concat`
  operations requires particular care.

* Benchmark to see if the free list is useful (in some scenarios)
  and how much it costs (in general). Remove it if it is useless.
  If we keep it, tune its maximum length. Try LIFO and FIFO scenarios.

* Benchmark: show point clouds or confidence intervals.

* Think about reference counting. Think about transactions (take a snapshot,
  do some updates, then decide to revert or commit; can we keep unique
  ownership?). Think about backtracking scenarios.
  Can we implement `checkpoint` and `revert`?
  What dynamic checks are required in order to detect incorrect usage?

* What about an `approximate_split` function that splits near the middle, and
  is more efficient than `split`? (Be careful to control the ratio of
  imbalance. Maybe sufficient to simply never a split a chunk at the outermost
  level.)

* Think about setting up sequential and parallel benchmarks under Multicore OCaml.

* Benchmark to measure the benefits and costs of inner buffers.

* Emulate some `Vector` (which one?).
  Benchmark against `CCVector`, `CCFun_vec`, `BatVect`, `Clarity.Vector`...
  See [the benchmarks in ocaml-containers](https://github.com/c-cube/ocaml-containers/blob/d34b7588b028f3618cc44d3f4c6417295db586c8/benchs/run_benchs.ml#L112).

* Benchmark our emulated `Stack`, `Queue`, `Vector`, `Array`, `List`, `Buffer`
  against the originals. (Measure time and space.)

* Think about adding a sense field to obtain O(1) reversal of sequences.
  Each schunk should be equipped with one "direction" bit, stored together
  with the view (using one spare bit). The interpretation of the bit is:
  "the elements are stored in reverse order, and if the elements are chunks,
  then the bit applies to those chunks (and recursively). The `rev` operation
  can be implemented in `O(log n)` by flipping the front and back schunk at
  every level, and flipping the direction bit associated with these schunks.
  To implement a "directed" operation such as `push`, one need to compute the
  `xor` of the direction of the operation and of that of the schunk in order
  to determine what actually needs to be performed (e.g., push-front vs push-back).
  Overall, the diff should be quite small, because only schunk are affected.
  The runtime overhead of reading the direction bits and computing the xor
  should be almost zero, but if it is not the case, we could use a settings flag
  to turn off the support for log-time reverse operations, in exchange for
  slightly faster other operations.

* Implement `push_segment` to push an array segment into the sequence.
  (Include a variant where the array has the size of a chunk and the
  sequence can take ownership of it.)
  (Think about a variant where the client specifies just a size and
  the sequence returns a segment of at most that size that can be
  written. Analogous to `get_writable_segment`, except we want to
  push a new segment, then write it.)
  Use `push_segment` to speed up some of the derived operations (e.g.,
  `flatten`, `flatten_map`, etc.).

* Likewise, implement `pop_segment`, which pops and returns an array
  segment. (The user has read-only access.)
  (Include a variant where the user can take ownership of the segment?)

* Optimize `E.fill` using `Array.fill`.
  Optimize `E.blit` using `Array.blit`.

* Add `blit` functions between sequences and between a subarray and a
  sequence.
  ac: with a version that uses sharing and a version that does not
  introduce any sharing.
  fp: When the destination is an ephemeral sequence, can (must) blit
  without sharing. When it is an array, implement blitting using an
  iterator on the source, `get_segment_and_jump`, and `Array.blit`.
  When it is a persistent sequence, we don't need `blit` at all;
  using `concat`, `split`, `make`, etc., the user can do whatever
  they want efficiently.

* Does the generated code change when we expose the fact that
  the type `segment` is equal to `int`?
  This might allow the compiler to remove the write barrier
  when updating a `view` field.
  Is performance affected? Benchmark.

* Add relaxed operations where the data structure does not represent
  a sequence, but a bag. In that case, all `push` operations affect
  the back chunk. One can maintain the invariant that all chunks in
  the middle sequence are full. The `bag_concat` function can be
  simplified and optimised. A `bag_split` operation splits the bag
  into two parts of a specified size.

* Optimize `P.make`, with more sharing.

* Define `insert`, `delete`, etc. (either using `split` and `concat`, or
  directly).

* Evaluate the interest of having owners on nodes from ShareableSequences.
  The benefit would be to update weights in place on insertion operations.

* Report `odoc` bug where a heading in an included signature (`@inline`)
  does not appear in the table of contents of the current page.

* Using an iterator and `init`, can implement `compact`.
  Or do it by popping segments off one sequence and pushing them into another.

* Think about more aggressive density invariants (e.g. fuse 3 chunks into 2,
  etc.).

* Think about an optimized implementation of Hashtables that use packed
  representations using (liked list of fixed-sized) arrays instead of
  linked lists. Actually, need two arrays, to avoid array of pairs.
