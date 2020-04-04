# Fuzzing Sek

This code compares two implementations of (mutable and immutable) sequences
using `afl-fuzz`. A tiny domain-specific language (DSL) is introduced; the
programs expressed in this language perform operations on sequences, such as
`create`, `push`, `pop`, `copy`, and so on. A potentially incorrect candidate
implementation is compared against a reference implementation by generating a
program in the DSL and executing this program under both implementations. If
the observable results of these executions differ, then a bug has been found.
The DSL program is then displayed, and the program point where the two
executions exhibit an observable difference is pinpointed. `afl-fuzz`
hopefully drives the generation of DSL programs in such a way that all code
paths are exercised.

Of course, the external tool `afl-fuzz` must be installed. Here is
[a suggested installation script](install-afl-fuzz.sh).

Then, an appropriate `opam` switch, such as `4.09.1+afl`, must be created.
This is done only once, and can be done as follows:

```
  make setup               # this takes a few minutes
```

If you are using MacOS, you should disable the system's built-in crash
reporter:

```
  make unload
```

After these steps, you are ready to fuzz the library!
Fuzzing is carried out as follows:

```
  make test                # or: make multicore
```

After a while, stop the fuzzer with `Ctrl-C`. If any crashes have been
found, ask for a report using either `make show` (which produces OCaml
code for each crash) or `make summary` (which shows a one-line summary
of each crash, by increasing order of length of the problematic code).
