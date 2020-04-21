# bark

This is an ***UNOFFICIAL*** OCaml port of the
[`elm/parser`](https://package.elm-lang.org/packages/elm/parser/1.1.0/) library
(version 1.1.0) by Evan Czaplicki.

This port is extremely faithful and copies the original almost line-for-line. Accordingly,
the [documentation](https://package.elm-lang.org/packages/elm/parser/1.1.0/) for
the `elm/parser` library (which is excellent) will be enough to understand how to
use this library.

The main difference between `bark` and `elm/parser` is that `elm/parser`
supports:

- double-wide Unicode characters, and
- fancier number parsing (e.g. octal, hex),

neither of which are supported by `bark`.
