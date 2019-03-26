uritemplate-ocaml
===============

Work in progress implementation of URI templates for OCaml. (RFC6570 - http://tools.ietf.org/html/rfc6570)

**Currently compliant to level 4**

ODoc documentation avaliable [here](https://corinchappy.github.io/uritemplate-ocaml/).

## Usage

```ocaml
# #require "uritemplate";;

# template_uri_with_strings ~template:"https://example.com{/a,b}{?b}{#e,f}" ~variables:[("a", "a"); ("b", "b"); ("e", "e"); ("f", "f")];;
- : string = "https://example.com/a/b?b=b#e,f"
```
