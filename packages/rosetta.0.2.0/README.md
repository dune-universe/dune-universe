# Rosetta - universal decoder of an encoded flow to Unicode

Rosetta is a merge-point between [uuuu][uuuu], [coin][coin] and
[yuscii][yuscii]. It able to decode UTF-7, ISO-8859 and KOI8 and return Unicode
code-point - then, end-user can normalize it to UTF-8 with [uutf][uutf] for
example.

The final goal is to provide an universal decoder of any encoding. This project
is a part of [mrmime][mrmime], a parser of emails to be able to decode
*encoded-word* (according [rfc2047][rfc2047]).

If you want to handle a new encoding (like, hmmhmm, APL-ISO-IR-68...), you can
make a new issue - then, the process will be to make a new little library and
integrate it to `rosetta`.

## How to use it?

`rosetta` follows the same design as libraries used underlying. More precisely,
it follows the same API as [uutf][uutf] about encoding. This is a little example
to transform a latin1 flow to UTF-8:

```ocaml
let trans ic oc =
  let decoder = Rosetta.decoder (Rosetta.encoding_of_string "latin1") (`Channel ic) in
  let encoder = Uutf.encoder `UTF_8 (`Channel oc) in
  let rec go () = match Rosetta.decode decoder with
    | `Await -> assert false (* XXX(dinosaure): impossible when you use `String of `Channel as source. *)
    | `Uchar _ as uchar -> ignore @@ Uutf.encode encoder uchar ; go ()
    | `End -> ignore @@ Uutf.encoder `End
    | `Malformed err -> failwith err in
  go ()
  
let () = trans stdin stdout
```

### About `encoding_of_string`

`rosetta` follows aliases availables into IANA character sets database:
https://www.iana.org/assignments/character-sets.xhtml

Others aliases will raise an exception. This function is case-insensitive.

### About translation tables

`rosetta` relies on underlying libraries such as `uuuu` or `coin`. They
integrate translation tables provided by Unicode consortium. They should not be
updated - so we statically save them into an `int array`.

### About encoding

`rosetta` supports only decoding to Unicode code-point. A support of encoding is
not on our plan where people should only use Unicode now. Deal with many
encodings is a pain and we should only produce something according to Unicode
than old encoding like latin1.

[uuuu]: https://github.com/mirage/uuuu.git
[yuscii]: https://github.com/mirage/yuscii.git
[coin]: https://github.com/mirage/coin.git
[uutf]: https://github.com/dbuenzli/uutf.git
[mrmime]: https://github.com/mirage/mrmime.git
[rfc2047]: https://tools.ietf.org/html/rfc2047
