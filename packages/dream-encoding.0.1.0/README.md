# Dream Encoding

[![Actions Status](https://github.com/tmattio/dream-encoding/workflows/CI/badge.svg)](https://github.com/tmattio/dream-encoding/actions)

Encoding primitives for Dream.

## Usage

The most common usage is to add the `Dream_encoding.compress` middleware to your Dream application:

```ocaml
let () =
  Dream.run ~debug:true
  @@ Dream.logger
  @@ Dream_encoding.compress
  @@ Dream.router [ Dream.get "/" (fun _ -> Dream.html "Hello World!") ]
  @@ Dream.not_found
```

The middleware will parse the `Accept-Encoding` header from the requests and compress the responses accordingly.

The library API also includes other lower-level functions for convenience, and are documented [here](lib/dream_encoding.mli).

## Limitation

As of now, the only supported encoding directives are `gzip` and `deflate`.

Support for more compression methods will come when they are supported in `decompress`, the underlying compression library used in `dream-encoding`.

## To Do

- [ ] Support Brotli compression (see https://github.com/mirage/decompress/issues/101)
