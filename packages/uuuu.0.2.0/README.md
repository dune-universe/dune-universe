# [Uuuu](https://www.youtube.com/watch?v=jjD9WzW6dK4)

Uhuhuhuhuhuh! `uuuu` (Universal Unifier to Unicode *Un* OCaml) is a little
library to normalize an ISO-8859 input to Unicode code-point. This library uses
tables provided by the Unicode Consortium:

[Unicode table](https://ftp.unicode.org/Public/MAPPINGS/ISO8859/)

This project takes tables and converts them to OCaml code. Then, it provides a
non-blocking *best-effort* decoder to translate ISO-8859 codepoint to UTF-8
codepoint.

## How to use it?

`uuuu` has an _dbuenzli_ interface. So it should be easy to use it and trick on
it. `uuuu` has a simple goal, offer a general way to decode an ISO-8859 input
and normalize it to unicode codepoints. We need to be able to control
memory-consumption and ensure to offer a non-blocking computation. Finally, an
error should not stop the process of the decoding.

This is a little example with [uutf][uutf] to translate a latin1 to UTF-8:

```ocaml
let trans ic oc =
  let decoder = Uuuu.decoder (Uuuu.encoding_of_string "latin1") (`Channel ic) in
  let encoder = Uutf.encoder `UTF_8 (`Channel oc) in
  let rec go () = match Uuuu.decode decoder with
    | `Await -> assert false (* XXX(dinosaure): impossible when you use `String of `Channel as source. *)
    | `Uchar _ as uchar -> ignore @@ Uutf.encode encoder uchar ; go ()
    | `End -> ignore @@ Uutf.encoder `End
    | `Malformed err -> failwith err in
  go ()
  
let () = trans stdin stdout
```

### About `encoding_of_string`

`uuuu` follows aliases availables into IANA character sets database:
https://www.iana.org/assignments/character-sets.xhtml

Others aliases will raise an exception. This function is case-insensitive.

### About translation tables

`uuuu` integrates translation tables provided by Unicode consortium. They should
not be updated - so we statically save then into an `int array`.

### About encoding

`uuuu` supports only decoding to Unicode code-point. A support of encoding is
not on our plan where people should only use Unicode now.

### A larger decoder

`uuuu` is a part of a biggest project [rosetta][rosetta] which is a decoder for
some others encodings. If you want to handle more encodings than ISO-8859, you
should look into this higher library.

### Distribution

`uuuu` integrates a little binary to translate ISO-8859 flow to UTF-8:
`uuuu.to_utf8`. It is provided as an example of how to use `uuuu` with `uutf`.

[uutf]: https://github.com/dbuenzli/uutf.git
[rosetta]: https://github.com/mirage/rosetta.git
