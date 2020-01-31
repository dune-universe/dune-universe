### Unstrctrd

An header of an email has a formal format described by RFC5322. After `mrmime`,
it reveals that the more general form for any values of fields (like a date, an
email address, etc.) is the _unstructured_ form.

This library wants to provide the first ground of how to parse an email header.
From that, we want to post-process _unstructured_ values to cast them to any
expected values like email address.

The idea behind this library is to handle a common format which can be found
into several standards, the _folding-whitespace_. For example, the [`*.deb*`][deb]
file uses this kind of format where it's possible that one field can be
associated to a _multiline_ value.

```deb
Description: my superb Debian
 package!
```

This library wants to _fold_ the value and, by this way, delete insignificant
_folding-whitespace_ to be able to apply a post-process like: parse an email
address (eg. [`emile`][emile] for more examples).

```mail
To: my.valid.mail
 (comment) @x25519.net
```

#### API

`unstrctrd` comes with several post-processes like:
- `val fold_fws : t -> t`
- `val without_comments : t -> (t, [> error ]) result`
- `val split_at : index:int -> t -> t * t`
- `val split_on : on:[ WSP | FWS | Uchar of Uchar.t | Char of char | LF | CR ] -> t -> (t * t) option`

Of course, it provides processes to manipulate a `string` and to convert an
_unstructured_ value to an UTF-8 `string`. With that, we can imply that
`unstrctrd` handles UTF-8 encoding (and only UTF-8 according RFC 6532).

The API gives you a way to craft an unstructured value and ensures that this
value is correct (and invalid any _unstructured_ values which produces CRLF
terminating token)

#### Angstrom

`unstrctrd.parser` provides an [angstrom][angstrom] parser which can be safely
composed with others `angstrom` parsers. It requires the allocation of an
internal buffer used by `ocamllex` and ensure the safety.

[deb]: https://fr.wikipedia.org/wiki/Deb
[emile]: https://github.com/dinosaure/emile.git
[angstrom]: https://github.com/inhabitedtype/angstrom.git
