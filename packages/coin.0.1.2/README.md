# Coin

`coin` is a little library to normalize an KOI8-{U,R} input to Unicode. This
library uses tables provided by the Unicode Consortium:

https://ftp.unicode.org/Public/MAPPINGS/VENDORS/MISC

This project takes tables and converts them to OCaml code. Then, it provides a
non-blocking decoder to translate KOI8-{U,R} codepoint to Unicode codepoint.
