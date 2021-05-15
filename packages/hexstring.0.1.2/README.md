# Hexstring

An OCaml library to encode/decode hexadecimal strings.

Install with opam:

```console
opam install hexstring
```

Add to your dune file:

```lisp
(libraries hexstring)
```

Use as such:

```ocaml
(* you can encode bytes to an hexstring *)
b = Bytes.of_string "\x01\x02";;
s = Hexstring.encode b;; (* "0102" *)

(* you can also decode some hexstring, which returns a result of bytes *)
d = match Hexstring.decode s with
| Error err -> printf "error: %s\n"
| Ok b' -> assert b = b'
```
