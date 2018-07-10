## color: converts between different color formats

Library that converts between different color formats. Right now it deals with
HSL, HSLA, RGB and RGBA formats.

The goal for this library is to provide easy handling of colors on the web, when working
with `js_of_ocaml`.

## Examples

```ocaml
# Color.to_hexstring (Color.of_rgb 12 121 229);;
- : "#0c79e5"
```

## Credit

Based on [purescript-colors](https://github.com/sharkdp/purescript-colors)
