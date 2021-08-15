# Identification of Languages and Language Groups for OCaml

## Synopsis

This library defines abstract types for languages and language families
covered by ISO 639 and provides related functionality and conversions.

- `Lang.t` maps one-to-one to ISO 639-3 language codes, which covers
  individual languages and macrolanguages.
- `Lang_family.t` maps one-to-one to ISO 639-5 language codes, which covers
  language families and groups.
- `Lang_or_family.t` is the sum of the two above types.

## Example

```ocaml
# #require "iso639";;
# open Iso639;;
# #install_printer Lang.pp;;

# let por = Lang.of_string_exn "por";;
val por : Lang.t = por
# Lang.to_iso639p1 por;;
- : string option = Some "pt"
# Lang.macrolanguage por;;
- : Lang.t option = None
# Lang.macrolanguage_members por;;
- : Lang.t list = []

# let nor = Lang.of_string_exn "nor";;
val nor : Lang.t = nor
# Lang.macrolanguage_members nor;;
- : Lang.t list = [nob; nno]
# Lang.macrolanguage (Lang.of_string_exn "nob");;
- : Lang.t option = Some nor
```

## Limitations

Only part of ISO 639 which are expected to be commonly used by applications
are implemented.

The library does not include the English end French names of languages, and
there is no plan to add them, as this would significantly increase the size
of the library.
