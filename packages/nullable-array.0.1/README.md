## Nullable-array

Nullable-array is a small self-contained library providing an efficient implementation for a type equivalent to `'a option array`

```OCaml
let a = Nullable_array.make 3 in
Nullable_array.set 1 (Some 4);
assert(Nullable_array.get 0 = None);
assert(Nullable_array.get 1 = Some 4);
```

[![Build Status (Travis)](https://travis-ci.org/chambart/ocaml-nullable-array.svg)](https://travis-ci.org/chambart/ocaml-nullable-array)
[![Build Status (Appveyor)](https://ci.appveyor.com/api/projects/status/github/chambart/ocaml-nullable-array?svg=true)](https://ci.appveyor.com/project/chambart/ocaml-nullable-array)