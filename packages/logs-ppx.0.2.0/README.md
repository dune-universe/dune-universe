# logs-ppx

A simple ppx to remove some boilerplate when using the excelent [Logs library](https://github.com/dbuenzli/logs).

## Usage

Add this to your dune stanza for your executable

```lisp
(preprocess
  (pps logs-ppx))
```

Then you use it like this in OCaml:

```ocaml
[%log debug "Hello %s!" "world"]
(* Which genrates the following *)
Logs.debug (fun m -> m "Hello %s!" "world")
```

And in Reason it looks like this:

```reason
[%log debug("Hello %s!", "world")];
// Which generates the following
Logs.debug(m => m("Hello %s!", "world"));
```

## Thanks

A huge thanks goes to @davesnx for a lot of help and his [starter repo](https://github.com/davesnx/ppxlib-simple-example)
