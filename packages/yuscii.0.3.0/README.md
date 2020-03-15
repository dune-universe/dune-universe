# [Yuscii](https://en.wikipedia.org/wiki/YUSCII)

Yuscii is a little library to decode an UTF-7 ([RFC2152][rfc2152] for instance)
input flow to Unicode. This library does not implement an encoder because, Eh
guy, we are in 2018...

### How to use it?

`yuscii` follows the same design than [uutf][uutf] or some others libraries with
the same purpose: translate something to Unicode. We need to be able to control
memory-consumption and ensure to offer a non-blocking computation. Finally, an
error should not stop the process of the decoding.

This is a little example with [uutf][uutf] to translate UTF-7 to UTF-8:

```ocaml
let trans ic oc =
  let decoder = Yuscii.decoder (`Channel ic) in
  let encoder = Uutf.encoder `UTF_8 (`Channel oc) in
  let rec go () = match Yuscii.decode decoder with
    | `Await -> assert false (* XXX(dinosaure): impossible when you use `String of `Channel as source. *)
    | `Uchar _ as uchar -> ignore @@ Uutf.encode encoder uchar ; go ()
    | `End -> ignore @@ Uutf.encoder `End
    | `Malformed err -> failwith err in
  go ()
  
let () = trans stdin stdout
```

### About UTF-7

SMTP protocol, for historical reasons is not necessary *8-bit clean* protocol.
In others words, SMTP may only support 7-bit data - and a 8-bit message had high
chances to be garbled during transmission.

For this purpose, UTF-7 exists and provide a way to encode a message under this
limit. The *advantage* of UTF-7 if we compare with the _quoted-printable_
encoding or the _base64_ encoding (RFC2045), is the size where UTF-8 combined
with _quoted-printable_ produces a very size-inefficient flow.

Of course, nobody uses it...

### About RFC2060

We rely only on RFC2152 where IMAP has his own UTF-7 and this package does not
want to handle both - in others words, if you want to decode an IMAP UTF-7 flow
(a `mUTF-7`), you probably should use something else than this library.

### About encoding

As we said, nobody continues to use UTF-7 (0.002 % according [w3techs][w3techs])
and this library is just an excuse to lost our times. So, the encoding is
definitely not a part of our plan and if you really want to encode something to
UTF-7, you are probably wrong.

### A larger decoder

As a part of the [mrmime][mrmime] project, `yuscii` is used by
[rosetta][rosetta] has an higher decoder of a larger set of encodings. You
probably want to use it to decode everythings.

### Distribution

`yuscii` integrates a little binary to translate UTF-7 flow to UTF-8:
`yuscii.to_utf8`. It is provided as an example of how to use `yuscii` with `uutf`.

### Did you know?

YUSCII is a 7-bit character encoding used in Yugoslavia. That's all...

[rfc2152]: https://tools.ietf.org/html/rfc2152
[mrmime]: https://github.com/mirage/mrmime.git
[rosetta]: https://github.com/mirage/rosetta.git
[w3techs]: https://w3techs.com/technologies/details/en-utf7/all/all
[uutf]: https://github.com/dbuenzli/uutf.git
