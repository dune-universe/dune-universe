# ppx_defer - Go-like `[%defer later]; now` syntax.

![Build status](https://github.com/hcarty/ppx_defer/workflows/Build%20and%20test%20ppx_defer/badge.svg)

This is an OCaml language extension implementing a somewhat Go-ish
`[%defer expr1]; expr2` which will defer the evaluation of `expr1` until after
`expr2`.  `expr1` will still be evaluated if `expr2` raises an exception.

If you are using Lwt you can use `[%defer.lwt expr1]; expr2`.

Thanks to Drup for guidance in figuring out ppx details!

## Using ppx_defer

As a simple example this code
```ocaml
let () =
  [%defer print_endline "world"];
  print_endline "Hello"
```
will print
```
Hello
world
```
as `print_endline "world"` was deferred until after `print_endline "Hello"`.

A more common use case would be closing an external resource at the end of the
current expression.
```ocaml
let () =
  let ic = open_in_bin "some_file" in
  [%defer close_in ic];
  let length = in_channel_length ic in
  let bytes = really_input_string ic length in
  print_endline bytes
```
This will `close_in ic` at the end of the current expression, even if an
exception is raised.

See the `examples/` directory for more examples.
