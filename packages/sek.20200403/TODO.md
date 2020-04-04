## After the Initial Release

* Check if `Segment.iter` is properly inlined/specialised in `find_weight_index`.

* Fuzz under `afl+flambda`, for fun.

* Optimize shareable sequence operations in case of a unit-weight measure.

* Consider other values for `max_length_of_free_list`; benchmark with
  LIFO and FIFO scenarios.

* Iterators: optimize `move` when the schunk has unit weight. Benchmark.

* Benchmark: show point clouds or confidence intervals.

* Think about reference counting. Think about transactions (take a snapshot,
  do some updates, then decide to revert or commit; can we keep unique
  ownership?). Think about backtracking scenarios.
  Can we implement `checkpoint` and `revert`?
  What dynamic checks are required in order to detect incorrect usage?

* Optimize find_weight_index, which is critical for constant factors.

* Avoid copy-on-write in [set] by doing nothing if the value written
  is physically equal to the value already at that index.

* Implement the "checkpoint" and "revert_to" interface.
  Add dynamic checks to ensure safe usage.

```
   type 'a t = { ... ; last_checkpoint : ('a checkpoint) option }

   type 'a checkpoint = 'a t (* but abstract type in the API *)

   (** [checkpoint s] takes a snapshot that can only be used once,
       for restoring [s] to its current state.
       If the result of [checkpoint s] is never needed, it may be
       silently dropped at any time.

       The checkpoint mechanism is incompatible with calls to [copy], [sub],
       [snapshot], [snapshot_and_clear], and incompatible with [concat]
       of structures that have not always been ephemeral. If these
       functions are called, the checkpoint object will be invalidated.

       The functions [copy_without_sharing] and [sub_without_sharing]
       remain available, however they execute in linear time.

       It is possible to build a series of checkpoints, and revert
       to any of the checkpoints made. Once a revert is executed, it
       remains possible to revert to prior checkpoints. This pattern
       can be useful for backtracking algorithms, in particular. *)

    (* The fields [last_checkpoint] represents the chained list of valid checkpoints. *)
    (* Invalidation is achieved by executing [s.last_checkpoint <- None]. *)
    (* Technically, we could tolerate [concat] on [s] after [checkpoint],
       as long as the version number of the concatenated sequence is small
       enough, but we don't want to expose version numbers in the specifications.) *)

   let checkpoint (s : 'a t) : 'a checkpoint =
      let r = copy of s, including a copy of the last_checkpoint field
              but without modifying incrementing any version number in
      s.last_checkpoint <- r;
      s.owner <- Owner.above s.owner;
      r
      (* Remark: morally, all the chunks in [r] and [s] are shared;
         however, we leave [r.owner] to its current value to capture
         the fact that, if we revert to [r], then [r] will recover
         unique ownership of all its chunks. This property is true as
         long as [s] has not leaked any of its chunks (which [s] views
         as shared). Hence, the need to disable any function that
         shares chunks from [s]. *)

   (* Internal: [is_valid_checkpoint r s] returns [true] if the operation [revert_to r s]
       is legitimate. The checkpoint [r] must be an ancestor of [s], and
       no operation that invalidate the checkpoints must have been performed
       on [s] since the creation of [r]. *)

   let rec is_valid_checkpoint r s =
      (* Implemented by walking up the chain of checkpoints; the total cost is O(1)
         amortized, since traversing checkpoints means giving up on using them. *)
      match s.last_checkpoint with
      | None -> false
      | Some r' -> (r == r') || (is_valid_checkpoint r r')

   (** [revert_to r s] sets [s] to the state [r], that is, to the state that
       [s] had when [r] was built. *)

   let revert_to (r : 'a checkpoint) (s : 'a t) =
      assert (is_valid_checkpoint r s);
      assign s r (* including the last_checkpoint field *)

   (* In fact, we could be more clever perhaps, and keep track of the last
      checkpoint that the user views as potentially useful; this way, we
      would know which chunks from [s] are safe to share, and which ones are
      not. But this seems somewhat too complicated. *)
```

* Once [odoc issue #417](https://github.com/ocaml/odoc/issues/417) is fixed,
  merge the branch `supply_default`. Or find a way of reformulating the code
  to work around the issue. (Avoid `include` followed with shadowing.)

* What about an `approximate_split` function that splits near the middle, and
  is more efficient than `split`? (Be careful to control the ratio of
  imbalance. Maybe sufficient to simply never a split a chunk at the outermost
  level.)

* Implement `take` and `drop`, i.e., specialized versions of `split` where
  only one part of the sequence is actually constructed.

* Benchmark to see if the free list is useful (in some scenarios)
  and how much it costs (in general). Remove it if it is useless.
  If we keep it, tune its maximum length.

* Think about setting up sequential and parallel benchmarks under Multicore OCaml.

* Benchmark to measure the benefits and costs of inner buffers.

* Explore other ways of representing a fubar sequence (e.g. by replacing the
  front and back chunks with dummy chunks) and benchmark.

* Emulate `Array`.

* Emulate some `Vector` (which one?).

* Complete the `Stack` and `Queue` wrappers with `to_seq`, `add_seq`, `of_seq`.

* Benchmark our emulated `Stack`, `Queue`, `Vector`, `Array`, `List`, `Buffer`
  against the originals. (Measure time and space.) (Be sure to use the library
  in release mode.) Use `core_bench`?

* Should we expose `update`, `xchg`, `get_and_update`?

* A `reverse` function is missing in the public API.
  Should think also about `to_array_rev`.
  Think about adding a sense field to obtain O(1) reversal of sequences.

* Add conversions to `'a Seq.t`.

* Iterators do not appear in the public API.
  Clean up the `Iter` submodules.
  Do whatever is necessary to expose this functionality.
  Document iterator invalidation. Detect it at runtime?
  Add a `version` field in ephemeral sequences.
  Use a functor parameter to enable these dynamic checks.

* Implement `pushn_back` to push a range of values from an external array into
  the sequence. Likewise, `popn` to pop and blit into an external array.
  (We already have `of_array_segment`. Avoid excessive redundancy.)

* Allow blitting a subsequence into a subarray (`to_array_segment`).
  (Use the iterator to do this?) (Related to above.)

* Add blit functions between Sek objects, with a version that
  uses sharing and a version that does not introduce any sharing.

* Encoding `sub` in terms of `split` can be expensive;
  propose direct implementations of `sub`. (Related to above.)

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

* `map` and `mapi` functions are missing.
  Add in-place `map` and `mapi` for ephemeral sequences.

* `take`, `drop`, and other comfort functions on sequences.

* What about binary functions (`iter2`, `map2`, etc.)?
  They should probably be defined using iterators.

* Implement ephemeral sequences in a simpler way just by using an owner
  field and a shareable sequence. (Either lose the free list, or move it
  to the implementation of shareable sequences.) Benchmark (to make sure
  that the speed difference is large.)

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
