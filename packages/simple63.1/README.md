# Simple63

Simple63 is an OCaml module for compressing and decompressing
sequences of integers along the ideas described in the [2010 paper by
Anh and
Moffat](https://scholar.google.com/scholar?q=Index+compression+using+64-bit+words+moffat+anh).
Like Simple-8b technique described in that paper, Simple63 is a
word-bounded, and (as the name suggests) is the result of adapting
Simple-8b to work with OCaml's 63-bit integers. While using `int64`
integer types would have been possible, the additional boxing required
to manipulate `int64`'s make this option unappealing.

Example usage:
```
let in_lst = [1; 22; 333; 4444] in
let in_seq = List.to_seq in_lst in
let out_seq = encode_to_seq in_seq in

(* confirm that we get out what we've put in: *)
let in_seq' = decode_from_seq out_seq in
let in_lst' = List.of_seq in_seq' in
assert (in_lst = in_lst')
```

To install:

```sh
opam install simple63
```

# Documentation

See [https://barko.github.io/simple63](https://barko.github.io/simple63)

# License

BSD
