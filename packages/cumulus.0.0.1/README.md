## Status

This library is in a working state, but the interface is not stable yet.

## Synopsis

Cumulus defines a signal-like type, which facilitates differential updates
of the underlying value. When a cumulus signal is changed, it emits a patch
along with the new value, allowing consumers to integrate it into an
existing value rather than rebuilding its value from scratch. Cumulus is
based on the [React](http://erratique.ch/software/react) FRP library.

## Documentation

- [API Reference](https://paurkedal.github.io/ocaml-cumulus/cumulus/Cumulus/index.html)

## Overview

The main type `('a, 'b) Cumulus.t` represents a cumulus signal over a state
of type `'a` with changes represented by `'b`. The idea is that an `(state,
change) Cumulus.t` instance makes sense if one can formulate a function
```ocaml
val patch : change -> state -> state
```
relating the previous value of the cumulus signal with the current via the
current change component. If this is the case, the cumulus signal can be
constructed from an initial value and an event of changes using
```ocaml
val integrate : ('da -> 'a -> 'a) -> 'da event -> 'a -> ('a, 'da) t
```
However, it is not required that `patch` is manifest and, as will be seen
below, the changes need not be complete for cumulus signals to be
practically useful.

As special cases
```ocaml
val of_event : 'a event -> (unit, 'a) t
```
shows how a react event is the analog of a cumulus signal where no state is
retained, and
```ocaml
val of_signal : 'a signal -> ('a, unit) t
```
shows how a react signal is the analog of a cumulus signal where no
information is provided about what changed from one value no the text.

The state of a cumulus signal can be pure, so that a new state is
constructed on each update of the signal, e.g.
```ocaml
('a list, [`Remove of int | `Add of int * 'a]) Cumulus.t
```
or mutable, allowing re-using the state between updates when possible, e.g.
```ocaml
('a array, [`Set of int * 'a | `Resize of int]) Cumulus.t
```

An essential part of the API is a set of lifting functions of different
arities, which allows deriving new cumulus signal from existing cumulus
signals. The basic pattern is:
```ocaml
let cs =
  let init state1 ... stateN =
    ... (* construct the initial state from input status *)
  in
  let patch (state1, change1) ... (stateN, changeN) state =
    (* inspect input states and changes *)
    if ... (* our state changes *) then
      let state', change' = ... in
      Cumulus.Patch (state', change')
    else
      Cumulus.Keep state
  in
  Cumulus.l2 ~init ~patch cs1 cs2
```
See [`tests/test_isecn.ml`](tests/test_isecn.ml) for a complete example.
