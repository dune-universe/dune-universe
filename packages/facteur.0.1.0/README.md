# Facteur

`facteur` is a little tool (experimental) to easily send a mail. It is a
milestone of several projects to implement the SMTP stack on MirageOS:

- [mrmime][mrmime]: is the main library which can parse and produce a mail
  according several RFCs. It support multipart and multiple encoding (when it
  uses as a parser).
- [colombe][colombe]: is the core library about SMTP protocol (without extension
  but can be extended) as decoder and encoder.
- [sendmail][sendmail]: is an implementation of the Submission RFC over TLS or
  with STARTTLS extension. This library is an example to implement several
  extension on top of [colombe][colombe].
- [emile][emile]: is a little library to parse a mail address according RFCs and
  provide a [cmdliner][cmdliner] helper. It uses as a cross representation of a
  mail address between [colombe][colombe] (RFC 5321) and [mrmime][mrmime] (RFC
  5322).
  
Of course, `facteur` uses several libraries. [mrmime][mrmime] uses
[rosetta][rosetta] to decode any encoding to Unicode. It uses [base64][base64]
or [pecu][pecu] as encoder/decoder of bodies. Or [angstrom][angstrom] as the
main library to describe (a)BNF of emails.

## Internal libraries

`facteur` needs `libmagic` to be able to recognize kind of your files. It
implements a C stubs and depends on an UNIX environment. In other side,
`facteur` has an other internal library, `rfc2183` which wants to emit a
`Content-Disposition` field. This library is on top of [mrmime][mrmime].

## SMTP details

`facteur` wants to use two ways to send an email. The first one wraps entirely a
SMTP flow with TLS (with [ocaml-tls][ocaml-tls]). The second way is to start a
SMTP flow and try to establish a TLS flow with STARTTLS.

Of course, the first way is better than the second. If both are not available,
we are not able to send an email.

## Experimental 

As we said, this library is really experimental and should not be used in
production where it deserves mostly a deep work about API on [mrmime][mrmime] or
[colombe][colombe] mainly.

However, it works (mostly)!

[mrmime]: https://github.com/mirage/mrmime.git
[colombe]: https://github.com/dinosaure/colombe.git
[sendmail]: https://github.com/dinosaure/colombe.git
[emile]: https://github.com/dinosaure/emile.git
[rosetta]: https://github.com/mirage/rosetta.git
[base64]: https://github.com/mirage/ocaml-base64.git
[pecu]: https://github.com/mirage/pecu.git
[angstrom]: https://github.com/inhabitedtype/angstrom.git
