## OCaml bindings to macOS FSEvents

This package relies on
[osx-cf](https://github.com/mirage/ocaml-cf) for CoreFoundation
support. These bindings use
[ctypes](https://github.com/ocamllabs/ocaml-ctypes) for type-safe stub
generation.

### FSEvents Quirks

`chmod` will result in an `ItemChangeOwner` event.

## History

This library is based on the [ocaml-osx-fsevents](https://github.com/dsheets/ocaml-osx-fsevents)
bindings originally written by David Sheets, and now renamed and maintained
by the MirageOS project.
