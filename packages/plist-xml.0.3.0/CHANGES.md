## 0.3.0 (Oct 28, 2020)

- `plist_of_stream_exn` now takes a `(Markup.signal, s) Markup.stream` instead
  of a `(Markup.content_signal, s) Markup.stream`, as the
  `Markup.content_signal` type was removed from Markup.ml in version 1.0.0-1.

## 0.2.0 (Aug 11, 2020)

- `signals` now emits `xml` and `doctype` elements.
- `signals` now gives `plist` element the attribute `version="1.0"`.
- Add `?encoding` parameter to `signals` function.

## 0.1 (Aug 5, 2020)

Initial release.
