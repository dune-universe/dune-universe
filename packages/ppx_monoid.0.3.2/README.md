# ppx-monoid

This is a syntax extension for OCaml to make building values of
monoids easier. Assumes the existence of two operations in scope for
some type `t`:

````ocaml
empty : t
(^^)  : t -> t -> t
````

`ppx-monoid`, triggered by the PPX extension point `monoid`,
reinterprets the semicolon `;` to mean the monoid operation `^^` and
the unit expression `()` to mean `empty`.

Example:

````ocaml
let empty = "" and (^^) = (^) in
begin%monoid
  "hello";
  " ";
  "world"
end
````

is translated to:

````ocaml
let empty = "" and (^^) = (^) in
"hello" ^^ " " ^^ "world"
````

It is also possible to use `concat` or `concatenate` instead of
`monoid`:

````ocaml
let empty = "" and (^^) = (^) in
begin%concat   (* or 'concatenate' *)
  "hello";
  " ";
  "world"
end
````

See the `test/test.ml` file for more examples of usage, and for
examples of how the translation interacts with `if then else` and
`match` expressions.

The main use case for this syntax extension is for writing code that
generates HTML which consists of long sequences of concatenated bits
of HTML.
