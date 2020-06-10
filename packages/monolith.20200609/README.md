# Monolith

Monolith offers facilities for **testing an OCaml library** (for instance, a
data structure implementation) by **comparing it against a reference
implementation**. It uses a form of black-box testing, and relies on
[`afl-fuzz`](https://lcamtuf.coredump.cx/afl/) for efficiency.

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

## Tutorial

At this time, a tutorial is not yet available. Stay tuned.

## Demos and Workflow

A number of demos are found under `demos/`. The demos under `demos/working` do
not have any known bugs, so `make test` should run forever without finding any
problem. The demos under `demos/faulty` intentionally contain bugs, so `make
test` should very quickly find a number of problems.

These demos share a common workflow, which is implemented in
[`Makefile.monolith`](Makefile.monolith).

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

## Real-Word Applications

Monolith has been used to test
[Sek](https://gitlab.inria.fr/fpottier/sek/),
a library that offers 4 abstract types and well over 100 operations on them.

## Documentation

The [documentation of the latest released
version](http://cambium.inria.fr/~fpottier/monolith/doc/monolith/Monolith/index.html)
is available online.

The documentation is built locally by `make doc` and can be viewed via `make
view`.
