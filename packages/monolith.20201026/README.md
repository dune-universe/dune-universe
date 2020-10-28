# Monolith

Monolith offers facilities for **testing an OCaml library** (for instance, a
data structure implementation) by **comparing it against a reference
implementation**. It can be used to perform either random testing or fuzz
testing. Fuzz testing relies on the external tool
[`afl-fuzz`](https://lcamtuf.coredump.cx/afl/).

The user must describe what types and operations the library provides. Under
the best circumstances, this requires **2-3 lines of code per type or
operation**. The user must also provide a reference implementation and a
candidate implementation of the library.

Then, like a monkey typing on a keyboard, Monolith attempts to exercise the
library in every possible way, in the hope of discovering a sequence of
operations that leads to an unexpected behavior (that is, a situation where
the library either raises an unexpected exception or returns an incorrect
result). If such a scenario is discovered, it is printed in the form of an
OCaml program, so as to help the user reproduce the problem.

Monolith assumes that the candidate implementation behaves in a deterministic
way. (Without this assumption, one cannot hope to reliably produce a
problematic scenario.) It does however allow nondeterministic specifications,
that is, situations where the candidate implementation is allowed to behave in
several possible ways.

## Installation

To install the latest released version, type:
```
  opam update
  opam install monolith
```

To install the latest development version, type:
```
  git clone git@gitlab.inria.fr:fpottier/monolith
  cd monolith
  opam pin add monolith .
```

## Documentation

The [documentation of the latest released
version](http://cambium.inria.fr/~fpottier/monolith/doc/monolith/Monolith/index.html)
is available online.

The documentation is built locally by `make doc` and can be viewed via `make
view`.

## Papers

The paper
[Strong Automated Testing of OCaml Libraries](http://cambium.inria.fr/~fpottier/publis/pottier-monolith-2021.pdf)
by François Pottier describes the use and the design of Monolith
in somewhat greater depth than the documentation.

## Demos and Workflow

A number of demos are found under `demos/`. The demos under `demos/working` do
not have any known bugs, so `make test` should run forever without finding any
problem. The demos under `demos/faulty` intentionally contain bugs, so `make
test` should very quickly find a number of problems.

Some demos require external libraries. Running `make dependencies` once at the
top level should install all of the libraries needed by the demos.

These demos share a common workflow, which is implemented in
[`Makefile.monolith`](Makefile.monolith).
This file is installed at the same time as the library. The
directory where it resides can be found via the command
`ocamlfind query monolith`.

First, the external tool `afl-fuzz` must be installed. Here is
[a suggested installation script](install-afl-fuzz.sh).

Then, an appropriate `opam` switch, such as `4.09.1+afl`, must be created. The
name of this switch is controlled by the variable `SWITCH`. This variable can
be set by passing something like `SWITCH=4.09.1+afl` on the command line in
every `make` invocation, or (better) in a parent `Makefile` which includes
`Makefile.monolith`.

Creating an `opam` switch is done only once, as follows:

```
  make setup               # this takes a few minutes
```

If you are using MacOS, you will probably need to disable the system's
built-in crash reporter by typing `make unload`. (Otherwise, `afl-fuzz` will
complain.)

If you are using an antivirus, it is advisable to disable it. As a case in
point, Kaspersky Endpoint Security on MacOS imposes a 5x speed penalty when
its protection is turned on.

After these steps, you are ready to run a demo. In each subdirectory of
`demos`, testing is carried out as follows:

```
  make test
```

This launches `afl-fuzz`, which keeps running forever, or until it is
interrupted by `Ctrl-C`. The number of problematic scenarios (also known as
crashes) found by `afl-fuzz` is displayed in red.

Once one or more crashes have been found and `afl-fuzz` has been interrupted,
it is recommended to first minimize the problem scenarios by using `make min`.

Then, you can ask for a report using either `make show` (which produces OCaml
code for each crash) or `make summary` (which shows a one-line summary of each
crash, by decreasing order of length).

A typical workflow is to first use `make summary`, so as to reveal the length
of the shortest crash, then type `make show` and search its output (by typing
`/`) for the shortest crash.

To perform purely random testing, without using `afl-fuzz`, type:

```
  make random
```

This uses a single processor core and runs in an infinite loop until a
problem is detected. Random testing is significantly faster than fuzz
testing, so it may be a good idea to first look for obvious bugs using
random testing, then look for nastier bugs using fuzz testing.

When running in random mode, after Monolith has a found a scenario, it reduces
the amount of fuel and continues searching for a shorter scenario. It is
therefore possible to start with a relatively large amount of fuel.

## Parallel Fuzzing

The command `make test` launches only one `afl-fuzz` process. Once you are
confident that it works, you can instead use `make multicore` or `make tmux`,
which launch several processes in parallel. They differ in their user
interface: whereas `make multicore` displays only periodic progress reports in
the terminal, `make tmux` splits the terminal window using `tmux` and shows
the activity of each `afl-fuzz` process in its own pane.

These commands launch one master process plus a number of slave processes. The
number of slave processes can be adjusted via the command line: for instance,
`make SLAVES=1 tmux` uses just one master process and one slave process.

While parallel fuzzing is ongoing, typing `make whatsup` displays a progress
report. The most interesting line is `Crashes found`, which reports the number
of violations found so far. The `Cumulative speed` line is also of interest,
but be aware that it seems to take a few minutes for this information to
become stable.

## Performance

Performance is measured in executions per second. It is visible in the
interactive interface, and is also reported by `make whatsup`.

Of course, the performance that you can expect depends on the cost of the
operations that you are testing. As a single data point, in a version of the
`map` demo, on a Linux machine equipped with two eight-core Intel Xeon CPUs
(E5-2620 v4 @ 2.10GHz), I am seeing 12k execs/second when using a single
`afl-fuzz` process, and about 5k execs/second/core when using all 32 cores
in parallel (therefore, about 160k execs/second).

Fuzzing under MacOS appears to be about 10 times slower, although I do not
know why; I have disabled both the MacOS crash reporter and the antivirus.

## Real-World Applications

Monolith has been used to test
[Sek](https://gitlab.inria.fr/fpottier/sek/),
a library that offers 4 abstract types and well over 100 operations on them.
