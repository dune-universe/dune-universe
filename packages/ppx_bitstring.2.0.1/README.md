# ppx_bitstring

[![Build Status](https://travis-ci.org/xguerin/ppx_bitstring.svg?branch=master)](https://travis-ci.org/xguerin/ppx_bitstring)

PPX plugin for the OCAML OPAM bitstring package.


## Overview

The PPX extension is called `ppx_bitstring`. It aims at eventually being a drop-in replacement of the bitstring `ocamlp4` extension.

## Installation

`ppx_bitstring` is available on `opam`:

```bash
opam install ppx_bitstring
```

## Syntax

Usage example with the `match` PPX extension:

```ocaml
match%bitstring bs with
| {| 1 : 1
   ; a : 2
   ; b : 16 : bigendian
   ; ...
   |} -> (* Do something *)
| {| _ |} -> (* Do something else *)
```

Usage example with the PPX extension for constructing bitstrings using the `let` syntax:

```ocaml
let%bitstring my_bitstring =
  {| 1 : 1
   ; a : 2
   ; b : 16 : bigendian
   ; ...
   |} in (* Do something here *)
```
Usage example with the PPX extension for constructing bitstrings using the constructor syntax:

```ocaml
let my_bitstring = [%bitstring
  {| 1 : 1
   ; a : 2
   ; b : 16 : bigendian
   ; ...
   |}] in (* Do something here *)
```

The pattern syntax is the same as the one of the [original extension](http://people.redhat.com/~rjones/bitstring/html/Bitstring.html).

## Additional features

### `map` qualifier

This extension also supports a `map` qualifier which applies a lambda to a parsed value and stores the result in the field name:

```ocaml
{| field : size : map (fun v -> do_something_with v) }|
```

Which is equivalent to:

```ocaml
let field = (fun v -> do_something_with v) temporary_parsed_field
```

The `map` and `bind` qualifiers are mutually exclusive.

### Pattern matching using `function`

This extension adds support for pattern matching using the `function` keyword:

```ocaml
let pattern_matcher = function%bitstring
| {| 1 : 1
   ; a : 2
   ; b : 16 : bigendian
   ; ...
   |} -> (* Do something *)
| {| _ |} -> (* Do something else *)
```

## Error reporting

This extension point supports error reporting. However, the algorithm is rather crude and the best results are obtained by following these rules :

1. Statements should not be split over multiple lines. However, multiple statements per lines are supported.
2. Statement separators should be placed *before*, and not *after* the statement.

## Contribute

### Implementation details

* Uses `ocaml-migrate-parsetree` to make it forward and backward compatible.
* Uses `jbuilder` as compilation system.

### Code generation

To see the parse tree of a ML file:

```bash
ocamlc -dparsetree foo.ml
```

To see the output of a development version of the extension:

```bash
ocamlfind opt -package bitstring -thread -dsource -ppx '_build/default/.ppx/ppx_bitsring/ppx.exe --as-ppx' foo.ml
```

## License

Copyright (c) 2016 Xavier R. Guérin <copyright@applepine.org>

Permission to use, copy, modify, and distribute this software for any purpose with or without fee is hereby granted, provided that the above copyright notice and this permission notice appear in all copies.

THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
