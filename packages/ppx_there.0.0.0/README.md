# ppx_there

OCaml PPX extension for on-steroid `__MODULE__`.

[![Build Status](https://travis-ci.com/NathanReb/ppx_there.svg?branch=master)](https://travis-ci.com/NathanReb/ppx_there)

## Overview

`ppx_there` turns the `[%there]` extension points into the fully qualified path of the value in
which they are found, as a string.

For example, the following code in a `a.ml` file:
```ocaml
module B = struct
  let c = [%there]
end
```
Is expanded  into:
```ocaml
module B = struct
  let c = "A.B.c"
end
```
