# `ppx_deriving` Plugin for Hardcaml

This module implements a plugin for the `ppx_deriving` rewriter that
supports the Hardcaml syntax:

* Provide a `record` annotation to generate helper functions
* Provide an optional `bits` attribute for signals
* Provide a required `width` attribute for `list` and `array`

It must be used in conjunction with `[@@deriving sexp_of]`, like this:

    ```ocaml
    [@@deriving sexp_of, hardcaml]
    ```
    
## Examples

### Module interface

Original syntax:

```ocaml
module S : interface
  signal
  signal_list{ }
  signal_array{| |}
end
```

New syntax:

```ocaml
module S : sig
  type 'a t = {
    signal       : 'a;
    signal_list  : 'a list;
    signal_array : 'a array;
  } [@@deriving sexp_of, hardcaml]
end
```

### Module implementation

Original syntax:

```ocaml
module S = interface
  signal[2]
  signal_list{2}[4]
  signal_array{|2|}[4]
end
```

New syntax:

```ocaml
module S = struct
  type 'a t = {
    signal       : 'a       [@bits 2];
    signal_list  : 'a list  [@length 2][@bits 4];
    signal_array : 'a array [@length 2][@bits 4];
  } [@@deriving sexp_of, hardcaml]
end
```
