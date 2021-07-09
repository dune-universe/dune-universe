# FAQ

#### “Is it wise to rely on the statistical nature of Memprof? If I set an allocation limit of 100 KB, and run a function that allocates exactly 50 KB, then the function might fail, due to the random nature of Memprof.”

Memprof-limits is provided with a [statistical
analysis](https://gitlab.com/gadmm/memprof-limits/-/blob/master/doc/statistical.md)
meant to help you chose appropriate values for the limit depending on
a target safe allocation value.

Long story short, memprof-limits starts being accurate-enough starting
around a safe allocation value of 20kw with the default sampling rate
(meaning a limit from 150kw to 500kw depending on chosen precision),
with the ratio between the maximal safe allocation and the limit
parameter dropping very quickly for higher values. Correctly, the
analysis shows that limits under 60kw (around 500KB) are unreliable.

I have found that the statistical nature of Memprof makes it very easy
to reason about its application and not have to factor in runtime
implementation details. In addition, Memprof is nevertheless
deterministic, which is (essential and) useful for reproducing runs in
test scenarios.

#### “But can we really program with memprof-limits, that is, not only write programs but also reason about them, given the probabilistic nature of the guarantees?”

Yes, if we make two additional hypotheses:

1. Allocation limits (as used in Haskell) are used by determining peak
   reasonable allocation usage empirically and picking a limit at a
   comfortable margin over it, rather than computing a precise memory
   bound to be used as a limit. In very controlled environments where
   the latter would be possible, there probably would be better
   solutions, and the language this is inspired from makes it very
   hard to make predictions on memory use.
2. The programmer is fine with a very unlikely possibility of a false
   positive; indeed the program is already designed to let true
   positives fail without bringing down mission-critical parts of the
   program. For instance they can prefer to see a legitimate client
   having a connexion closed once every 10*ⁿ* year for *n* of their
   choosing, if that is the price to pay for avoiding being subject to
   DOS on maliciously-crafted requests.

Under these hypotheses, the statistical limit is just as reliable as
the precise limits à la Haskell.

#### “Can this be used in combination with Lwt and Async?”

Memprof-limits is compatible with _detached threads_ as offered by
[`Lwt_preemptive`](https://ocsigen.org/lwt/5.3.0/api/Lwt_preemptive)
and [`Mwt`](https://github.com/hcarty/mwt), which use OCaml system
threads underneath.

Regarding Async/Lwt promises, to the best of my knowledge, they are
not meant to play well when interrupted by asynchronous exceptions as
used by memprof-limits. But otherwise it would be straightforward to
make memprof-limits parametric in the notion of _thread id_ used to
track per-thread limits to accomodate promises. If you are interested
in experimenting, please get in touch.

#### “Is it possible to also implement _local_ memory limits, to bound the memory consumption of a particular function?”

Yes, it might be possible to implement local memory limits, but read
on.

[Yang & Mazières
(2014)](https://dl.acm.org/doi/10.1145/2594291.2594341) presents an
_allocator-pays_ model of cost attribution, and note its similarity
with memory profiling. In this model, it is possible for instance to
process untrusted user input under some memory limit, before the
result is distributed to the rest of the program.

Implementing memory limits based on the allocator-pays model, by
adapting allocation limits to take into account deallocations, would
be very easy thanks to the facilities provided by Memprof. Moreover,
the statistical analysis of allocation limits can be transposed, and
should guarantee similar accuracy at a low runtime cost for limits
greater than 100KB.

There is one surprising difficulty, though, which has to do with the
way the GC works. The GC has a space overhead: memory that is wasted
because unreachable values are not collected immediately. This
overhead has to be taken into account when choosing the limit.
However, this overhead is non-local and dependent on the _total_ major
heap size: one cannot just say “take the double of the desired limit”.
Indeed, active threads will pay for memory that has been allocated in
the past and kept alive. More experimentation is needed to provide
guidance on how to take the space overhead into account before this
feature can be proposed.
